module Main exposing (main)

import Browser
import Hierarchy
import Html exposing (Html, button, div, h2, text)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Http
import Json.Decode as Decode exposing (Decoder)
import Tree exposing (Tree)
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, dominantBaseline, fontFamily, strokeLinecap, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (onMouseOut, onMouseOver)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), StrokeLinecap(..), Transform(..))


type alias Node =
    { id : Int
    , label : String
    , value : Maybe Int
    }


type alias PositionedNode =
    { height : Float
    , node : Node
    , width : Float
    , x : Float
    , y : Float
    }


type Exercise
    = BinaryTree
    | OrderedTree
    | FlareTree
    | CountryTree


type LoadState
    = Loading
    | Loaded (Tree Node)
    | Failed String


type alias Model =
    { active : Exercise
    , hovered : Maybe Int
    , flare : LoadState
    , countries : LoadState
    }


type Msg
    = Select Exercise
    | Hover Int
    | Leave
    | GotFlare (Result Http.Error (Tree Node))
    | GotCountries (Result Http.Error (Tree Node))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = BinaryTree
      , hovered = Just 0
      , flare = Loading
      , countries = Loading
      }
    , Cmd.batch
        [ Http.get { url = "flare.json", expect = Http.expectJson GotFlare flareDecoder }
        , Http.get { url = "countryHierarchy.json", expect = Http.expectJson GotCountries countryDecoder }
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select exercise ->
            ( { model | active = exercise, hovered = Nothing }, Cmd.none )

        Hover id ->
            ( { model | hovered = Just id }, Cmd.none )

        Leave ->
            ( { model | hovered = Nothing }, Cmd.none )

        GotFlare (Ok tree) ->
            ( { model | flare = Loaded (assignIds tree) }, Cmd.none )

        GotFlare (Err error) ->
            ( { model | flare = Failed (httpErrorToString error) }, Cmd.none )

        GotCountries (Ok tree) ->
            ( { model | countries = Loaded (assignIds tree) }, Cmd.none )

        GotCountries (Err error) ->
            ( { model | countries = Failed (httpErrorToString error) }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ HtmlAttr.class "page" ]
        [ Html.node "style" [] [ text stylesheet ]
        , div [ HtmlAttr.class "toolbar" ]
            [ tabButton model.active BinaryTree "9.1 Binärer Baum"
            , tabButton model.active OrderedTree "9.1 Geordneter Baum"
            , tabButton model.active FlareTree "9.2 flare.json"
            , tabButton model.active CountryTree "9.3 Länder"
            ]
        , h2 [] [ text (exerciseTitle model.active) ]
        , activeTreeView model
        ]


tabButton : Exercise -> Exercise -> String -> Html Msg
tabButton active target label =
    button
        [ HtmlAttr.classList
            [ ( "tab", True )
            , ( "tab-active", active == target )
            ]
        , HtmlEvents.onClick (Select target)
        ]
        [ text label ]


type alias TreeConfig =
    { fixedSize : Maybe ( Float, Float )
    , nodeRadius : Float
    , parentChildMargin : Float
    , peerMargin : Float
    , rotateNodeLabels : Bool
    , showLeafLabels : Bool
    }


activeTreeView : Model -> Html Msg
activeTreeView model =
    case model.active of
        BinaryTree ->
            treeView
                { fixedSize = Just ( 1320, 520 )
                , nodeRadius = 20
                , parentChildMargin = 80
                , peerMargin = 36
                , rotateNodeLabels = False
                , showLeafLabels = False
                }
                model.hovered
                binaryTree

        OrderedTree ->
            treeView
                { fixedSize = Just ( 1320, 520 )
                , nodeRadius = 20
                , parentChildMargin = 80
                , peerMargin = 36
                , rotateNodeLabels = False
                , showLeafLabels = False
                }
                model.hovered
                orderedTree

        FlareTree ->
            loadedTreeView
                { fixedSize = Nothing
                , nodeRadius = 4
                , parentChildMargin = 130
                , peerMargin = 16
                , rotateNodeLabels = True
                , showLeafLabels = True
                }
                model.hovered
                model.flare

        CountryTree ->
            loadedTreeView
                { fixedSize = Nothing
                , nodeRadius = 4
                , parentChildMargin = 130
                , peerMargin = 18
                , rotateNodeLabels = True
                , showLeafLabels = True
                }
                model.hovered
                model.countries


loadedTreeView : TreeConfig -> Maybe Int -> LoadState -> Html Msg
loadedTreeView config hovered state =
    case state of
        Loading ->
            div [ HtmlAttr.class "notice" ] [ text "Lade lokale JSON-Datei ..." ]

        Failed message ->
            div [ HtmlAttr.class "notice error" ] [ text message ]

        Loaded tree ->
            treeView config hovered tree


treeView : TreeConfig -> Maybe Int -> Tree Node -> Html Msg
treeView config hovered sourceTree =
    let
        layoutAttrs =
            (case config.fixedSize of
                Just ( w, h ) ->
                    [ Hierarchy.size w h ]

                Nothing ->
                    []
            )
                ++ [ Hierarchy.nodeSize (\_ -> ( config.nodeRadius * 2, config.nodeRadius * 2 ))
                   , Hierarchy.parentChildMargin config.parentChildMargin
                   , Hierarchy.peerMargin config.peerMargin
                   , Hierarchy.layered
                   ]

        positionedTree =
            Hierarchy.tidy layoutAttrs sourceTree

        positionedNodes =
            Tree.toList positionedTree

        links =
            Tree.links positionedTree

        xs =
            List.map .x positionedNodes

        ys =
            List.map .y positionedNodes

        minX =
            List.minimum xs |> Maybe.withDefault 0

        maxX =
            List.maximum xs |> Maybe.withDefault 0

        minY =
            List.minimum ys |> Maybe.withDefault 0

        maxY =
            List.maximum ys |> Maybe.withDefault 0

        -- gedrehte Labels laufen senkrecht nach unten -> unten extra Platz
        bottomPad =
            if config.rotateNodeLabels then
                170

            else
                config.nodeRadius * 4

        sidePad =
            config.nodeRadius * 5

        topPad =
            config.nodeRadius * 4

        vbX =
            minX - sidePad

        vbY =
            minY - topPad

        vbW =
            (maxX - minX) + sidePad * 2

        vbH =
            (maxY - minY) + topPad + bottomPad

        contents =
            List.map linkElement links
                ++ List.map (nodeElement config hovered) positionedNodes
                ++ List.filterMap (nodeLabelElement config) positionedNodes
                ++ List.filterMap (hoverLabelElement config hovered) positionedNodes
    in
    case config.fixedSize of
        Just _ ->
            svg
                [ class [ "tree-svg" ]
                , viewBox vbX vbY vbW vbH
                ]
                contents

        Nothing ->
            -- echte Pixelgroesse + scrollbarer Container, damit nicht herunterskaliert wird
            div [ HtmlAttr.class "tree-scroll" ]
                [ svg
                    [ class [ "tree-svg-natural" ]
                    , width vbW
                    , height vbH
                    , viewBox vbX vbY vbW vbH
                    ]
                    contents
                ]


linkElement : ( PositionedNode, PositionedNode ) -> Svg Msg
linkElement ( parent, child ) =
    line
        [ class [ "edge" ]
        , x1 parent.x
        , y1 parent.y
        , x2 child.x
        , y2 child.y
        , strokeLinecap StrokeLinecapRound
        ]
        []


nodeElement :
    { config | nodeRadius : Float }
    -> Maybe Int
    -> PositionedNode
    -> Svg Msg
nodeElement config hovered positioned =
    let
        isHovered =
            hovered == Just positioned.node.id
    in
    circle
        [ class
            [ "node"
            , if isHovered then
                "node-hovered"

              else
                "node-default"
            ]
        , cx positioned.x
        , cy positioned.y
        , r config.nodeRadius
        , onMouseOver (Hover positioned.node.id)
        , onMouseOut Leave
        ]
        []


nodeLabelElement :
    { config | showLeafLabels : Bool, nodeRadius : Float, rotateNodeLabels : Bool }
    -> PositionedNode
    -> Maybe (Svg Msg)
nodeLabelElement config positioned =
    if config.showLeafLabels && String.length positioned.node.label <= 60 then
        let
            labelX =
                positioned.x + config.nodeRadius + 5

            labelY =
                positioned.y + 3

            rotation =
                if config.rotateNodeLabels then
                    [ transform [ Rotate 90 labelX labelY ] ]

                else
                    []
        in
        Just
            (text_
                (rotation
                    ++ [ class [ "node-label" ]
                       , x labelX
                       , y labelY
                       , fontSize 10
                       , fontFamily [ "Arial", "Helvetica", "sans-serif" ]
                       ]
                )
                [ TypedSvg.Core.text positioned.node.label ]
            )

    else
        Nothing


hoverLabelElement :
    { config | nodeRadius : Float }
    -> Maybe Int
    -> PositionedNode
    -> Maybe (Svg Msg)
hoverLabelElement config hovered positioned =
    if hovered == Just positioned.node.id then
        let
            labelText =
                hoverText positioned.node

            tx =
                positioned.x

            ty =
                positioned.y - config.nodeRadius - 12

            attrs =
                [ class [ "hover-label" ]
                , x tx
                , y ty
                , textAnchor AnchorMiddle
                , dominantBaseline DominantBaselineAuto
                , fontSize 28
                , fontFamily [ "Arial", "Helvetica", "sans-serif" ]
                ]
        in
        Just
            (text_
                attrs
                [ TypedSvg.Core.text labelText ]
            )

    else
        Nothing


hoverText : Node -> String
hoverText node =
    case node.value of
        Just value ->
            node.label ++ " (" ++ String.fromInt value ++ ")"

        Nothing ->
            node.label ++ " " ++ String.fromInt node.id


assignIds : Tree Node -> Tree Node
assignIds =
    Tree.indexedMap (\id node -> { node | id = id })


mkNode : String -> Maybe Int -> Node
mkNode label value =
    { id = 0, label = label, value = value }


leaf : String -> Tree Node
leaf label =
    Tree.singleton (mkNode label Nothing)


binaryTree : Tree Node
binaryTree =
    assignIds
        (Tree.tree (mkNode "root" Nothing)
            [ Tree.tree (mkNode "left" Nothing)
                [ Tree.tree (mkNode "left.left" Nothing)
                    [ Tree.tree (mkNode "left.left.left" Nothing)
                        [ leaf "left.left.left.left" ]
                    ]
                , Tree.tree (mkNode "left.right" Nothing)
                    [ Tree.tree (mkNode "user12" Nothing)
                        [ Tree.tree (mkNode "user11" Nothing)
                            [ leaf "user10" ]
                        , leaf "user13"
                        ]
                    ]
                ]
            , Tree.tree (mkNode "right" Nothing)
                [ leaf "user2"
                , Tree.tree (mkNode "right.right" Nothing)
                    [ leaf "right.right.child" ]
                ]
            ]
        )


orderedTree : Tree Node
orderedTree =
    assignIds
        (Tree.tree (mkNode "root" Nothing)
            [ Tree.tree (mkNode "group-a" Nothing)
                [ leaf "a1"
                , leaf "a2"
                , leaf "a3"
                , leaf "a4"
                , leaf "a5"
                , leaf "a6"
                , Tree.tree (mkNode "a7" Nothing)
                    [ leaf "a7.1" ]
                ]
            , leaf "single-b"
            , leaf "single-c"
            , Tree.tree (mkNode "chain-d" Nothing)
                [ leaf "chain-d.1" ]
            , leaf "single-e"
            , Tree.tree (mkNode "group-f" Nothing)
                [ Tree.tree (mkNode "1" Nothing)
                    [ leaf "f1"
                    , leaf "f2"
                    , leaf "f3"
                    , leaf "f4"
                    , leaf "f5"
                    , leaf "f6"
                    , leaf "f7"
                    ]
                ]
            ]
        )


flareDecoder : Decoder (Tree Node)
flareDecoder =
    Decode.map3
        (\name value children ->
            Tree.tree (mkNode name value) (Maybe.withDefault [] children)
        )
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "value" Decode.int))
        (Decode.maybe (Decode.field "children" (Decode.list (Decode.lazy (\_ -> flareDecoder)))))


countryDecoder : Decoder (Tree Node)
countryDecoder =
    Decode.map2
        (\name children ->
            Tree.tree (mkNode name Nothing) (Maybe.withDefault [] children)
        )
        (Decode.at [ "data", "id" ] Decode.string)
        (Decode.maybe (Decode.field "children" (Decode.list (Decode.lazy (\_ -> countryDecoder)))))


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Ungueltige URL: " ++ url

        Http.Timeout ->
            "Zeitueberschreitung beim Laden der Datei."

        Http.NetworkError ->
            "Die Datei konnte nicht geladen werden. Starte die Seite ueber einen lokalen Webserver im Ordner uebung/9."

        Http.BadStatus status ->
            "HTTP-Fehler " ++ String.fromInt status

        Http.BadBody message ->
            "JSON-Decoder-Fehler: " ++ message


exerciseTitle : Exercise -> String
exerciseTitle exercise =
    case exercise of
        BinaryTree ->
            "Aufgabe 9.1a: Binaerer Baum"

        OrderedTree ->
            "Aufgabe 9.1b: Allgemeiner geordneter Baum"

        FlareTree ->
            "Aufgabe 9.2: flare.json"

        CountryTree ->
            "Aufgabe 9.3: Laenderhierarchie"


stylesheet : String
stylesheet =
    """
body {
  margin: 0;
  color: #1f2933;
  background: #f7f8f8;
  font-family: Arial, Helvetica, sans-serif;
}

.page {
  padding: 18px 22px 28px;
}

.toolbar {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-bottom: 10px;
}

.tab {
  border: 1px solid #9aa4aa;
  border-radius: 4px;
  background: #ffffff;
  color: #29343b;
  cursor: pointer;
  font: inherit;
  padding: 8px 12px;
}

.tab-active {
  background: #263238;
  border-color: #263238;
  color: #ffffff;
}

h2 {
  font-size: 18px;
  font-weight: 600;
  margin: 10px 0 14px;
}

.tree-svg {
  display: block;
  width: 100%;
  height: calc(100vh - 118px);
  min-height: 520px;
  background: #ffffff;
}

.tree-scroll {
  overflow: auto;
  max-height: calc(100vh - 130px);
  border: 1px solid #d0d5d8;
  background: #ffffff;
}

.tree-svg-natural {
  display: block;
  background: #ffffff;
}

.edge {
  stroke: #666666;
  stroke-width: 4px;
}

.node {
  cursor: pointer;
  stroke: #111111;
  stroke-width: 3px;
}

.node-default {
  fill: #626262;
  stroke: #626262;
}

.node-hovered {
  fill: #70d94f;
  stroke: #111111;
}

.hover-label {
  fill: #000000;
  pointer-events: none;
}

.node-label {
  fill: #555555;
  pointer-events: none;
}

.notice {
  display: flex;
  align-items: center;
  min-height: 360px;
  padding: 20px;
  background: #ffffff;
  color: #263238;
}

.error {
  color: #a61b1b;
}
"""
