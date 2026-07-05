port module Main exposing (main)

-- import Svg exposing (..)
-- import Svg.Attributes as SA

import Browser
import Color
import Html
import Json.Decode exposing (Decoder, Error, Value, decodeValue, errorToString)
import Json.Decode.Pipeline exposing (required)
import Json.Encode exposing (Value, encode, object)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Core
import TypedSvg.Types



-- Testgraph


testGraph : GraphData
testGraph =
    { options = defaultGraphOptions
    , values = defaultGraphValues
    , nodes =
        [ Node "testid1" (NodeValue "testLabel1" 50 140 1.0 1.0)
        , Node "testid2" (NodeValue "testLabel2" 50 140 1.0 1.0)
        ]
    , edges = [ Edge "testid1" "testid2" (EdgeValue "edgeLabel1" 10 90 1.0 1.0 []) ]
    }



-- Init


init : () -> ( Model, Cmd msg )
init _ =
    ( Model Nothing, layout (graphToJsonStr2 testGraph) )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Graph data_ ->
            let
                graph =
                    case decodeGraph data_ of
                        Ok g ->
                            Just g

                        Err e ->
                            Debug.log ("Decode failed" ++ errorToString e) Nothing
            in
            ( { model | graph = graph }, Cmd.none )

        Layout ->
            ( model
            , layout (graphToJsonStr2 testGraph)
            )



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
    | Layout


type alias Model =
    { graph : Maybe GraphData }



-- Graph


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
    , x : Float
    , y : Float
    , points : List Point
    }


type alias Point =
    { x : Float
    , y : Float
    }



-- Encoder


graphToJsonStr2 : GraphData -> String
graphToJsonStr2 g =
    encode 0 <|
        object
            [ ( "nodes"
              , Json.Encode.list identity
                    (List.map
                        (\n ->
                            jsonNode n.id n.value.label n.value.width n.value.height
                        )
                        g.nodes
                    )
              )
            , ( "edges"
              , Json.Encode.list identity
                    (List.map
                        (\e ->
                            jsonEdge e.from e.to e.values.label e.values.width e.values.height
                        )
                        g.edges
                    )
              )
            , ( "options", jsonOptions )
            , ( "value", jsonGraphValues )
            ]


jsonNode : String -> String -> Int -> Int -> Value
jsonNode a l w h =
    object
        [ ( "v", Json.Encode.string a )
        , ( "value"
          , object
                [ ( "label", Json.Encode.string l )
                , ( "width", Json.Encode.int w )
                , ( "height", Json.Encode.int h )
                ]
          )
        ]


jsonEdge : String -> String -> String -> Int -> Int -> Value
jsonEdge a b l w h =
    object
        [ ( "v", Json.Encode.string a )
        , ( "w", Json.Encode.string b )
        , ( "value"
          , object
                [ ( "label", Json.Encode.string l )
                , ( "width", Json.Encode.int w )
                , ( "height", Json.Encode.int h )
                ]
          )
        ]


jsonOptions : Value
jsonOptions =
    object
        [ ( "directed", Json.Encode.bool True )
        , ( "multigraph", Json.Encode.bool False )
        , ( "compound", Json.Encode.bool False )
        ]


jsonGraphValues : Value
jsonGraphValues =
    object
        [ ( "nodesep", Json.Encode.int 10 )
        , ( "ranksep", Json.Encode.int 100 )
        , ( "rankdir", Json.Encode.string "LR" )
        , ( "marginx", Json.Encode.int 20 )
        , ( "marginy", Json.Encode.int 20 )
        ]



-- Decoder


decodeGraph : Value -> Result Error GraphData
decodeGraph json =
    decodeValue graphData json


graphData : Decoder GraphData
graphData =
    Json.Decode.succeed GraphData
        |> required "options" graphOptions
        |> required "value" graphValues
        |> required "nodes" (Json.Decode.list node)
        |> required "edges" (Json.Decode.list edge)


graphOptions : Decoder GraphOptions
graphOptions =
    Json.Decode.succeed GraphOptions
        |> required "directed" Json.Decode.bool
        |> required "multigraph" Json.Decode.bool
        |> required "compound" Json.Decode.bool


graphValues : Decoder GraphValues
graphValues =
    Json.Decode.succeed GraphValues
        |> required "nodesep" (Json.Decode.map floor Json.Decode.float)
        |> required "ranksep" (Json.Decode.map floor Json.Decode.float)
        |> required "rankdir" Json.Decode.string
        |> required "marginx" (Json.Decode.map floor Json.Decode.float)
        |> required "marginy" (Json.Decode.map floor Json.Decode.float)
        |> required "width"  (Json.Decode.map floor Json.Decode.float)
        |> required "height" (Json.Decode.map floor Json.Decode.float) 


node : Decoder Node
node =
    Json.Decode.succeed Node
        |> required "v" Json.Decode.string
        |> required "value" nodeValue


nodeValue : Decoder NodeValue
nodeValue =
    Json.Decode.succeed NodeValue
        |> required "label" Json.Decode.string
        |> required "height" Json.Decode.int
        |> required "width" Json.Decode.int
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


edge : Decoder Edge
edge =
    Json.Decode.succeed Edge
        |> required "v" Json.Decode.string
        |> required "w" Json.Decode.string
        |> required "value" edgeValue


edgeValue : Decoder EdgeValue
edgeValue =
    Json.Decode.succeed EdgeValue
        |> required "label" Json.Decode.string
        |> required "height" Json.Decode.int
        |> required "width" Json.Decode.int
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float
        |> required "points" (Json.Decode.list point)


point : Decoder Point
point =
    Json.Decode.succeed Point
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float



-- View


defaultGraphOptions : GraphOptions
defaultGraphOptions =
    GraphOptions True False False


defaultGraphValues : GraphValues
defaultGraphValues =
    GraphValues 10 100 "LR" 20 20 600 600


view : Model -> Browser.Document Msg
view model =
    { title = "Dagre Viewer"
    , body = [ body model ]
    }


body : Model -> Html.Html Msg
body { graph } =
    Html.div []
        [ graphPane graph
        ]


graphPane : Maybe GraphData -> Html.Html msg
graphPane graph =
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
    viewNodes graphData_.nodes
        |> (++) (viewEdges graphData_.edges)
        |> TypedSvg.svg
            [ TypedSvg.Attributes.width <| TypedSvg.Types.Px sw
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px sh
            , TypedSvg.Attributes.viewBox 0 0 sw sh
            ]


viewNodes : List Node -> List (TypedSvg.Core.Svg msg)
viewNodes nodes =
    List.map viewNode nodes


viewNode : Node -> TypedSvg.Core.Svg msg
viewNode node_ =
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
            toFloat  node_.value.width
    in
    TypedSvg.g
        []
        [ TypedSvg.rect
            [ TypedSvg.Attributes.x <| TypedSvg.Types.Px sx
            , TypedSvg.Attributes.y <| TypedSvg.Types.Px sy
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px sw
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px sh
            , TypedSvg.Attributes.rx <| TypedSvg.Types.Px 10
            , TypedSvg.Attributes.ry <| TypedSvg.Types.Px 10
            , TypedSvg.Attributes.stroke <| TypedSvg.Types.Paint Color.blue
            , TypedSvg.Attributes.fill <| TypedSvg.Types.Paint Color.white
            ]
            []
        , TypedSvg.text_
            [ TypedSvg.Attributes.x <| TypedSvg.Types.Px mx
            , TypedSvg.Attributes.y <| TypedSvg.Types.Px my
            , TypedSvg.Attributes.alignmentBaseline TypedSvg.Types.AlignmentCentral
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ TypedSvg.Core.text node_.value.label ]
        ]


viewEdges : List Edge -> List (TypedSvg.Core.Svg msg)
viewEdges edges =
    List.map viewEdge edges


viewEdge : Edge -> TypedSvg.Core.Svg msg
viewEdge edge_ =
    let
        mx =
            edge_.values.x

        my =
            edge_.values.y

        sx { x } =
            String.fromFloat x

        sy { y } =
            String.fromFloat y

        p2s p =
            sx p ++ "," ++ sy p

        pnts =
            --String.join " " <| List.map p2s edge_.values.points
            List.map (\p -> ( p.x, p.y )) edge_.values.points
    in
    TypedSvg.g
        []
        [ TypedSvg.polyline
            [ TypedSvg.Attributes.fill TypedSvg.Types.PaintNone
            , TypedSvg.Attributes.stroke <| TypedSvg.Types.Paint Color.black
            , TypedSvg.Attributes.points pnts
            ]
            []
        , TypedSvg.text_
            [ TypedSvg.Attributes.x <| TypedSvg.Types.Px mx
            , TypedSvg.Attributes.y <| TypedSvg.Types.Px my
            , TypedSvg.Attributes.alignmentBaseline TypedSvg.Types.AlignmentCentral
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ TypedSvg.Core.text edge_.values.label ]
        ]
