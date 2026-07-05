port module Main exposing (main)

import Browser
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode exposing (Decoder, Error, Value, decodeValue, errorToString)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value, encode, object)
import TypedSvg
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Length(..), Paint(..))



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sentenceGraph = Nothing, rankerGraphs = Dict.empty }
    , Cmd.batch
        (layout (encodeRequest sentenceKey "LR" Nothing sentenceSpec)
            :: List.map
                (\r -> layout (encodeRequest r "TB" (Just r) slide456Spec))
                rankers
        )
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Graph data_ ->
            case decodeValue envelope data_ of
                Ok { key, graph } ->
                    if key == sentenceKey then
                        ( { model | sentenceGraph = Just graph }, Cmd.none )

                    else
                        ( { model | rankerGraphs = Dict.insert key graph model.rankerGraphs }, Cmd.none )

                Err e ->
                    Debug.log ("Decode failed: " ++ errorToString e) ( model, Cmd.none )



-- Ports


port graphs : (Value -> msg) -> Sub msg


port layout : String -> Cmd msg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    graphs Graph



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Types


type Msg
    = Graph Value


type alias Model =
    { sentenceGraph : Maybe GraphData
    , rankerGraphs : Dict String GraphData
    }



-- Graph (dagre input/output format)


type alias GraphData =
    { options : GraphOptions
    , values : GraphValues
    , nodes : List Node
    , edges : List Edge
    }


type alias GraphOptions =
    { directed : Bool
    , multigraph : Bool
    , compound : Bool
    }


type alias GraphValues =
    { nodesep : Int
    , ranksep : Int
    , rankdir : String
    , marginx : Int
    , marginy : Int
    , width : Int
    , height : Int
    }


type alias Node =
    { id : String
    , value : NodeValue
    }


type alias NodeValue =
    { label : String
    , class : String
    , height : Int
    , width : Int
    , x : Float
    , y : Float
    }


type alias Edge =
    { from : String
    , to : String
    , values : EdgeValue
    }


type alias EdgeValue =
    { label : String
    , height : Int
    , width : Int
    , points : List Point
    }


type alias Point =
    { x : Float
    , y : Float
    }



-- Request specs (what we want dagre to lay out, before layout is computed)


type alias NodeSpec =
    { id : String
    , label : String
    , class : String
    }


type alias EdgeSpec =
    { from : String
    , to : String
    }


type alias GraphSpec =
    { nodes : List NodeSpec
    , edges : List EdgeSpec
    }


type alias Envelope =
    { key : String
    , graph : GraphData
    }


sentenceKey : String
sentenceKey =
    "sentence"


rankers : List String
rankers =
    [ "network-simplex", "tight-tree", "longest-path" ]



-- Aufgabe 11.1 / 11.2: dagre-d3 "sentence tokenization" demo, rebuilt from
-- https://dagrejs.github.io/project/dagre-d3/latest/demo/sentence-tokenization.html
-- Parse of "This is an example sentence."


sentenceSpec : GraphSpec
sentenceSpec =
    { nodes =
        [ NodeSpec "0" "TOP" "type-TOP"
        , NodeSpec "1" "S" "type-S"
        , NodeSpec "2" "NP" "type-NP"
        , NodeSpec "3" "DT" "type-DT"
        , NodeSpec "4" "This" "type-TK"
        , NodeSpec "5" "VP" "type-VP"
        , NodeSpec "6" "VBZ" "type-VBZ"
        , NodeSpec "7" "is" "type-TK"
        , NodeSpec "8" "NP" "type-NP"
        , NodeSpec "9" "DT" "type-DT"
        , NodeSpec "10" "an" "type-TK"
        , NodeSpec "11" "NN" "type-NN"
        , NodeSpec "12" "example" "type-TK"
        , NodeSpec "13" "." "type-dot"
        , NodeSpec "14" "sentence" "type-TK"
        ]
    , edges =
        [ EdgeSpec "3" "4"
        , EdgeSpec "2" "3"
        , EdgeSpec "1" "2"
        , EdgeSpec "6" "7"
        , EdgeSpec "5" "6"
        , EdgeSpec "9" "10"
        , EdgeSpec "8" "9"
        , EdgeSpec "11" "12"
        , EdgeSpec "8" "11"
        , EdgeSpec "5" "8"
        , EdgeSpec "1" "5"
        , EdgeSpec "13" "14"
        , EdgeSpec "1" "13"
        , EdgeSpec "0" "1"
        ]
    }



-- Aufgabe 11.3: der Graph von Folie 456 der Vorlesung.


slide456Spec : GraphSpec
slide456Spec =
    { nodes = List.map (\n -> NodeSpec n n "") (List.map String.fromInt (List.range 0 14))
    , edges =
        [ EdgeSpec "3" "9"
        , EdgeSpec "3" "12"
        , EdgeSpec "3" "10"
        , EdgeSpec "3" "8"
        , EdgeSpec "9" "12"
        , EdgeSpec "9" "13"
        , EdgeSpec "8" "14"
        , EdgeSpec "10" "14"
        , EdgeSpec "1" "12"
        , EdgeSpec "1" "5"
        , EdgeSpec "2" "8"
        , EdgeSpec "2" "5"
        , EdgeSpec "2" "13"
        , EdgeSpec "2" "7"
        , EdgeSpec "2" "6"
        , EdgeSpec "4" "12"
        , EdgeSpec "4" "10"
        , EdgeSpec "4" "14"
        , EdgeSpec "5" "14"
        , EdgeSpec "0" "5"
        , EdgeSpec "0" "13"
        , EdgeSpec "13" "14"
        , EdgeSpec "7" "13"
        , EdgeSpec "11" "13"
        , EdgeSpec "6" "11"
        , EdgeSpec "6" "13"
        ]
    }



-- Node sizing: width grows with label length instead of being fixed,
-- so "sentence" gets a wider box than "is".


estimateNodeSize : String -> ( Int, Int )
estimateNodeSize label =
    ( String.length label * 8 + 24, 36 )



-- Encoder


encodeRequest : String -> String -> Maybe String -> GraphSpec -> String
encodeRequest key rankdir ranker spec =
    encode 0 <|
        object
            [ ( "key", Encode.string key )
            , ( "nodes", Encode.list encodeNodeSpec spec.nodes )
            , ( "edges", Encode.list encodeEdgeSpec spec.edges )
            , ( "options", jsonOptions )
            , ( "value", jsonGraphValues rankdir ranker )
            ]


encodeNodeSpec : NodeSpec -> Value
encodeNodeSpec n =
    let
        ( w, h ) =
            estimateNodeSize n.label
    in
    object
        [ ( "v", Encode.string n.id )
        , ( "value"
          , object
                [ ( "label", Encode.string n.label )
                , ( "class", Encode.string n.class )
                , ( "width", Encode.int w )
                , ( "height", Encode.int h )
                ]
          )
        ]


encodeEdgeSpec : EdgeSpec -> Value
encodeEdgeSpec e =
    object
        [ ( "v", Encode.string e.from )
        , ( "w", Encode.string e.to )
        , ( "value"
          , object
                [ ( "label", Encode.string "" )
                , ( "width", Encode.int 0 )
                , ( "height", Encode.int 0 )
                ]
          )
        ]


jsonOptions : Value
jsonOptions =
    object
        [ ( "directed", Encode.bool True )
        , ( "multigraph", Encode.bool False )
        , ( "compound", Encode.bool False )
        ]


jsonGraphValues : String -> Maybe String -> Value
jsonGraphValues rankdir ranker =
    object
        (List.filterMap identity
            [ Just ( "nodesep", Encode.int 20 )
            , Just ( "ranksep", Encode.int 50 )
            , Just ( "rankdir", Encode.string rankdir )
            , Just ( "marginx", Encode.int 20 )
            , Just ( "marginy", Encode.int 20 )
            , Maybe.map (\r -> ( "ranker", Encode.string r )) ranker
            ]
        )



-- Decoder


envelope : Decoder Envelope
envelope =
    Decode.succeed Envelope
        |> required "key" Decode.string
        |> custom graphData


graphData : Decoder GraphData
graphData =
    Decode.succeed GraphData
        |> required "options" graphOptions
        |> required "value" graphValues
        |> required "nodes" (Decode.list node)
        |> required "edges" (Decode.list edge)


graphOptions : Decoder GraphOptions
graphOptions =
    Decode.succeed GraphOptions
        |> required "directed" Decode.bool
        |> required "multigraph" Decode.bool
        |> required "compound" Decode.bool


graphValues : Decoder GraphValues
graphValues =
    Decode.succeed GraphValues
        |> required "nodesep" (Decode.map floor Decode.float)
        |> required "ranksep" (Decode.map floor Decode.float)
        |> required "rankdir" Decode.string
        |> required "marginx" (Decode.map floor Decode.float)
        |> required "marginy" (Decode.map floor Decode.float)
        |> required "width" (Decode.map floor Decode.float)
        |> required "height" (Decode.map floor Decode.float)


node : Decoder Node
node =
    Decode.succeed Node
        |> required "v" Decode.string
        |> required "value" nodeValue


nodeValue : Decoder NodeValue
nodeValue =
    Decode.succeed NodeValue
        |> required "label" Decode.string
        |> required "class" Decode.string
        |> required "height" Decode.int
        |> required "width" Decode.int
        |> required "x" Decode.float
        |> required "y" Decode.float


edge : Decoder Edge
edge =
    Decode.succeed Edge
        |> required "v" Decode.string
        |> required "w" Decode.string
        |> required "value" edgeValue


edgeValue : Decoder EdgeValue
edgeValue =
    Decode.succeed EdgeValue
        |> required "label" Decode.string
        |> required "height" Decode.int
        |> required "width" Decode.int
        |> required "points" (Decode.list point)


point : Decoder Point
point =
    Decode.succeed Point
        |> required "x" Decode.float
        |> required "y" Decode.float



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Übung 11: Sugiyama-Methode (Dagre)"
    , body =
        [ Html.div [ HA.style "font-family" "sans-serif", HA.style "padding" "20px" ]
            [ Html.h1 [] [ Html.text "Übung 11: Sugiyama-Methode" ]
            , Html.h2 [] [ Html.text "Aufgabe 11.1: Sentence Tokenization (ohne CSS-Klassen)" ]
            , graphPane False model.sentenceGraph
            , Html.h2 [] [ Html.text "Aufgabe 11.2: Sentence Tokenization (mit CSS-Klassen)" ]
            , graphPane True model.sentenceGraph
            , Html.h2 [] [ Html.text "Aufgabe 11.3: Folie 456 — drei Ranker-Varianten" ]
            , Html.div [] (List.map (rankerPane model.rankerGraphs) rankers)
            ]
        ]
    }


rankerPane : Dict String GraphData -> String -> Html Msg
rankerPane rankerGraphs ranker =
    Html.div []
        [ Html.h3 [] [ Html.text ("ranker: " ++ ranker) ]
        , graphPane False (Dict.get ranker rankerGraphs)
        ]


defaultGraphOptions : GraphOptions
defaultGraphOptions =
    GraphOptions True False False


defaultGraphValues : GraphValues
defaultGraphValues =
    GraphValues 20 50 "LR" 20 20 600 200


graphPane : Bool -> Maybe GraphData -> Html msg
graphPane withClasses graph =
    let
        defaultGraph =
            GraphData defaultGraphOptions defaultGraphValues [] []

        graphData_ =
            Maybe.withDefault defaultGraph graph

        sw =
            toFloat graphData_.values.width

        sh =
            toFloat graphData_.values.height
    in
    (arrowMarker
        :: (if withClasses then
                [ svgStyle ]

            else
                []
           )
    )
        ++ viewEdges graphData_.edges
        ++ viewNodes withClasses graphData_.nodes
        |> TypedSvg.svg
            [ SA.width <| Px sw
            , SA.height <| Px sh
            , SA.viewBox 0 0 sw sh
            ]


svgStyle : Svg msg
svgStyle =
    TypedSvg.style []
        [ text
            """
            g.type-TK > rect { fill: #00ffd0; }
            .node rect { stroke: #999; fill: #fff; stroke-width: 1.5px; }
            text { font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; font-size: 14px; }
            """
        ]


arrowMarker : Svg msg
arrowMarker =
    TypedSvg.defs []
        [ TypedSvg.marker
            [ SA.id "arrowhead"
            , SA.markerWidth (Px 8)
            , SA.markerHeight (Px 8)
            , SA.refX "6"
            , SA.refY "3"
            , SA.orient "auto"
            ]
            [ TypedSvg.path
                [ SA.d "M0,0 L6,3 L0,6 Z"
                , SA.fill (Paint Color.black)
                ]
                []
            ]
        ]


viewNodes : Bool -> List Node -> List (Svg msg)
viewNodes withClasses nodes =
    List.map (viewNode withClasses) nodes


viewNode : Bool -> Node -> Svg msg
viewNode withClasses node_ =
    let
        mx =
            node_.value.x

        my =
            node_.value.y

        sx =
            node_.value.x - (toFloat node_.value.width / 2.0)

        sy =
            node_.value.y - (toFloat node_.value.height / 2.0)

        sh =
            toFloat node_.value.height

        sw =
            toFloat node_.value.width

        classAttrs =
            if withClasses then
                [ SA.class [ "node", node_.value.class ] ]

            else
                []
    in
    TypedSvg.g
        classAttrs
        [ TypedSvg.rect
            [ SA.x <| Px sx
            , SA.y <| Px sy
            , SA.width <| Px sw
            , SA.height <| Px sh
            , SA.rx <| Px 5
            , SA.ry <| Px 5
            , SA.stroke <| Paint Color.blue
            , SA.fill <| Paint Color.white
            ]
            []
        , TypedSvg.text_
            [ SA.x <| Px mx
            , SA.y <| Px my
            , SA.alignmentBaseline TypedSvg.Types.AlignmentCentral
            , SA.textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ text node_.value.label ]
        ]


viewEdges : List Edge -> List (Svg msg)
viewEdges edges =
    List.map viewEdge edges


viewEdge : Edge -> Svg msg
viewEdge edge_ =
    let
        pnts =
            List.map (\p -> ( p.x, p.y )) edge_.values.points
    in
    TypedSvg.g
        []
        [ TypedSvg.polyline
            [ SA.fill TypedSvg.Types.PaintNone
            , SA.stroke <| Paint Color.black
            , SA.points pnts
            , SA.markerEnd "url(#arrowhead)"
            ]
            []
        ]
