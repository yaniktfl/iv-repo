port module Main exposing (main)

import Browser
import Json.Decode exposing (errorToString)
import Json.Encode exposing (Value)
import Process
import Task
import Json.Encode exposing (Value, bool, encode, float, int, list, object, string)
import Json.Decode exposing (Decoder, Error, Value, bool, decodeValue, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Char
import Parser exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes as SA



-- Init


init : () -> ( Model, Cmd msg )
init _ =
    let
        model =
            Model Nothing data 0
    in
    ( model, layout (graphToJsonStr model.editor) )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Edit str ->
            ( { model | editor = str, layoutRequested = model.layoutRequested + 1 }
            , Task.perform
                (\_ -> Layout)
                (Process.sleep (1.0 * 1000))
            )

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
            let
                outstanding =
                    if model.layoutRequested > 0 then
                        model.layoutRequested - 1

                    else
                        0
            in
            ( { model | layoutRequested = model.layoutRequested - 1 }
            , if outstanding > 0 then
                Cmd.none

              else
                layout (graphToJsonStr model.editor)
            )

        NoOp ->
            ( model, Cmd.none )



-- Ports


port graphs : (Value -> msg) -> Sub msg


port layout : String -> Cmd msg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
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

--module Types exposing (Edge, EdgeValue, Entry(..), GraphData, GraphOptions, GraphValues, Model, Msg(..), Node, NodeValue, Point)

--import Json.Encode exposing (Value)


type Msg
    = Edit String
    | Graph Value
    | Layout
    | NoOp


type alias Model =
    { graph : Maybe GraphData
    , editor : String
    , layoutRequested : Int
    }



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



-- Parsed lines


type Entry
    = N String String
    | E String String String
    | C String
    | Blank


-- Encoder
--module Encoder exposing (graphToJsonStr)



graphToJsonStr : String -> String
graphToJsonStr str =
    encode 0 <|
        object
            [ ( "nodes", nodesJson str )
            , ( "edges", edgesJson str )
            , ( "options", jsonOptions )
            , ( "value", jsonGraphValues )
            ]


parsed : String -> List (Result (List DeadEnd) Entry)
parsed str =
    parse str


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


nodeToJson : Result (List DeadEnd) Entry -> Maybe Value
nodeToJson n =
    case n of
        Ok (N a l) ->
            Just <| jsonNode a l 140 50

        _ ->
            Nothing


edgeToJson : Result (List DeadEnd) Entry -> Maybe Value
edgeToJson e =
    case e of
        Ok (E a b l) ->
            Just <| jsonEdge a b l 90 10

        _ ->
            Nothing


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


nodesJson : String -> Value
nodesJson str =
    Json.Encode.list identity (List.filterMap nodeToJson <| parsed str)


edgesJson : String -> Value
edgesJson str =
    Json.Encode.list identity (List.filterMap edgeToJson <| parsed str)



-- Decoder

-- module Decoder exposing (decodeGraph)



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
        |> required "nodesep" Json.Decode.int
        |> required "ranksep" Json.Decode.int
        |> required "rankdir" Json.Decode.string
        |> required "marginx" Json.Decode.int
        |> required "marginy" Json.Decode.int
        |> required "width" Json.Decode.int
        |> required "height" Json.Decode.int


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


-- Graphparser

-- module GraphParser exposing (parse)



parse : String -> List (Result (List DeadEnd) Entry)
parse str =
    str
        |> String.lines
        |> List.map (run parseLine)


parseLine : Parser Entry
parseLine =
    oneOf
        [ parseNodeOrEdge
        , parseComment
        , parseBlank
        ]


parseComment : Parser Entry
parseComment =
    Parser.succeed C
        |. Parser.symbol "#"
        |. spaces
        |= (getChompedString <|
                (Parser.succeed () |. chompUntilEndOr "\n")
           )
        |. end


parseBlank : Parser Entry
parseBlank =
    Parser.succeed Blank
        |. end



{-
   Node:
     foo : Fooish label
     bar : Barish label

   Edge:
     foo -> bar : Relation label
-}


parseNodeOrEdge : Parser Entry
parseNodeOrEdge =
    Parser.succeed addFirstName
        |. spaces
        |= parseName
        |. spaces
        |= (oneOf
                [ Parser.succeed True |. Parser.symbol ":"
                , Parser.succeed False |. Parser.symbol "->"
                ]
                |> andThen
                    (\v ->
                        if v then
                            Parser.succeed (\l -> N "" l)
                                |. spaces
                                |= parseLabel
                                |. spaces

                        else
                            Parser.succeed (\n2 l -> E "" n2 l)
                                |. spaces
                                |= parseName
                                |. spaces
                                |. Parser.symbol ":"
                                |. spaces
                                |= parseLabel
                                |. spaces
                    )
           )


addFirstName : String -> Entry -> Entry
addFirstName n e =
    case e of
        N _ l ->
            N n l

        E _ n2 l ->
            E n n2 l

        x ->
            x


parseLabel : Parser String
parseLabel =
    getChompedString <|
        Parser.succeed ()
            |. chompWhile isLabelChar


parseName : Parser String
parseName =
    getChompedString <|
        Parser.succeed ()
            |. chompWhile isNameChar


isNameChar : Char -> Bool
isNameChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


isLabelChar : Char -> Bool
isLabelChar char =
    isNameChar char
        || char
        == ' '


spaces : Parser ()
spaces =
    Parser.succeed ()
        |. chompWhile (\c -> c == ' ')


-- Testgraph

--module TestGraph exposing (data)


data : String
data =
    """# Nodes are specified as
# <identifier> : <label>

kspacey : Kevin Spacey
swilliams : Saul Williams
bpitt : Brad Pitt
hford : Harrison Ford
lwilson : Luke Wilson
kbacon : Kevin Bacon


# Edges are specified as
# <node id 1> -> <node id 2> : <edge label>
kspacey -> swilliams : worked with
swilliams -> kbacon : worked with
bpitt -> kbacon : worked with
hford -> lwilson : worked with
lwilson -> kbacon : worked with
"""


-- View

-- module View exposing (view)



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
body { editor, graph } =
    Element.layout [] <|
        column
            [ width fill
            , spacing 10
            ]
            [ header
            , editorResult editor graph
            ]


header =
    paragraph
        [ padding 25
        , Background.color (rgb 120 120 120)
        ]
        [ Element.text "Specify a graph in the left pane, and see the result in the right. The layout is calculated with the external JS library"
        , newTabLink [ Font.bold ]
            { url = "https://github.com/dagrejs/dagre/wiki"
            , label = Element.text "dagre.js"
            }
        ]


editorResult editor graph =
    let
        le =
            List.length errors |> Debug.log "Number of errors"

        errors =
            graphErrors editor

        hasErrors =
            List.any (\s -> s /= "") errors
    in
    row
        [ width fill
        , spacing 10
        ]
        [ dataPane editor hasErrors
        , if hasErrors then
            errorPane errors

          else
            graphPane graph
        ]


dataPane : String -> Bool -> Element Msg
dataPane editor hasErrors =
    mLine editor "Graph specification:" hasErrors Edit


mLine : String -> String -> Bool -> (String -> Msg) -> Element Msg
mLine content lbl hasErrors msg =
    el
        [ width <| fillPortion 1
        , Font.family [ Font.monospace ]
        , padding 10
        ]
    <|
        Input.multiline
            [ HA.rows 30 |> Element.htmlAttribute
            ]
            { onChange = msg
            , text = content
            , placeholder = Nothing
            , label = Input.labelAbove [] <| Element.text lbl
            , spellcheck = False
            }


errorPane : List String -> Element Msg
errorPane errors =
    let
        content =
            String.join "\n" errors
    in
    mLine content "Errors in specification:" False (\_ -> NoOp)


graphPane : Maybe GraphData -> Element msg
graphPane graph =
    let
        defaultGraph =
            GraphData defaultGraphOptions defaultGraphValues [] []

        graphData_ =
            Maybe.withDefault defaultGraph graph

        sw =
            String.fromInt graphData_.values.width

        sh =
            String.fromInt graphData_.values.height
    in
    viewNodes graphData_.nodes
        |> (++) (viewEdges graphData_.edges)
        |> svg [ SA.width sw, SA.height sh, SA.viewBox ("0 0 " ++ sw ++ " " ++ sh) ]
        |> Element.html
        |> el [ centerX, alignTop, width <| fillPortion 1 ]


viewNodes : List Node -> List (Svg msg)
viewNodes nodes =
    List.map viewNode nodes


viewNode : Node -> Svg msg
viewNode node_ =
    let
        mx =
            String.fromFloat <| node_.value.x

        my =
            String.fromFloat <| node_.value.y

        sx =
            String.fromFloat <| node_.value.x - (toFloat node_.value.width / 2.0)

        sy =
            String.fromFloat <| node_.value.y - (toFloat node_.value.height / 2.0)

        sh =
            String.fromInt node_.value.height

        sw =
            String.fromInt node_.value.width
    in
    Svg.node "g"
        []
        [ rect
            [ SA.x sx
            , SA.y sy
            , SA.width sw
            , SA.height sh
            , SA.rx "10"
            , SA.ry "10"
            , SA.stroke "blue"
            , SA.fill "white"
            ]
            []
        , text_
            [ SA.x mx
            , SA.y my
            , SA.alignmentBaseline "central"
            , SA.textAnchor "middle"
            ]
            [ Svg.text node_.value.label ]
        ]


viewEdges : List Edge -> List (Svg msg)
viewEdges edges =
    List.map viewEdge edges


viewEdge : Edge -> Svg msg
viewEdge edge_ =
    let
        mx =
            String.fromFloat edge_.values.x

        my =
            String.fromFloat edge_.values.y

        sx { x } =
            String.fromFloat x

        sy { y } =
            String.fromFloat y

        p2s p =
            sx p ++ "," ++ sy p

        pnts =
            String.join " " <| List.map p2s edge_.values.points
    in
    Svg.node "g"
        []
        [ polyline [ SA.fill "none", SA.stroke "black", SA.points pnts ] []
        , text_
            [ SA.x mx
            , SA.y my
            , SA.alignmentBaseline "central"
            , SA.textAnchor "middle"
            ]
            [ Svg.text edge_.values.label ]
        ]


graphErrors : String -> List String
graphErrors str =
    let
        parsed_ =
            parse str

        getError x =
            case x of
                Ok _ ->
                    ""

                Err err ->
                    String.join " or " <| List.map (\{ col, problem } -> Debug.toString problem ++ " at column " ++ String.fromInt col) err
    in
    List.map getError parsed_

