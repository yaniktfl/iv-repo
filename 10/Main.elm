module Main exposing (main)

import Browser
import Char
import Hierarchy
import Html exposing (Html, div, h2, h3, text)
import Html.Attributes as HtmlAttr
import Http
import Json.Decode as Decode exposing (Decoder)
import Tree exposing (Tree)
import TypedSvg as Svg exposing (rect, svg, text_)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as SvgPx
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types as SvgTypes


type alias Model =
    { testTree : Tree ( String, Int )
    , flare : Maybe (Tree ( String, Int ))
    , statusMsg : String
    }


type Msg
    = GotFlare (Result Http.Error (Tree ( String, Maybe Int )))


type alias PositionedNode =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , value : Float
    , node : ( String, Int )
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { testTree = testTree
      , flare = Nothing
      , statusMsg = "Lade flare.json ..."
      }
    , Http.get
        { url = "flare.json"
        , expect = Http.expectJson GotFlare treeDecoder
        }
    )


testTree : Tree ( String, Int )
testTree =
    Tree.tree ( "a", 10 )
        [ Tree.tree ( "b", 3 )
            [ Tree.singleton ( "d", 1 )
            , Tree.singleton ( "e", 2 )
            ]
        , Tree.tree ( "c", 7 )
            [ Tree.tree ( "f", 3 )
                [ Tree.singleton ( "i", 1 )
                , Tree.tree ( "j", 2 )
                    [ Tree.singleton ( "k", 1 )
                    , Tree.singleton ( "l", 1 )
                    ]
                ]
            , Tree.singleton ( "g", 2 )
            , Tree.singleton ( "h", 2 )
            ]
        ]


treeDecoder : Decoder (Tree ( String, Maybe Int ))
treeDecoder =
    Decode.map3
        (\name value children ->
            Tree.tree ( name, value ) (Maybe.withDefault [] children)
        )
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "value" Decode.int))
        (Decode.maybe (Decode.field "children" (Decode.list (Decode.lazy (\_ -> treeDecoder)))))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            let
                numericTree =
                    Tree.map (\( name, value ) -> ( name, Maybe.withDefault 0 value )) newTree
            in
            ( { model | flare = Just (computeSums numericTree), statusMsg = "flare.json geladen" }
            , Cmd.none
            )

        GotFlare (Err error) ->
            ( { model | flare = Nothing, statusMsg = httpErrorToString error }
            , Cmd.none
            )


computeSums : Tree ( String, Int ) -> Tree ( String, Int )
computeSums tree =
    let
        children =
            Tree.children tree
                |> List.map computeSums

        ownValue =
            Tree.label tree |> Tuple.second

        childSum =
            children
                |> List.map (Tree.label >> Tuple.second)
                |> List.sum
    in
    Tree.tree ( Tree.label tree |> Tuple.first, ownValue + childSum ) children


view : Model -> Html Msg
view model =
    div [ HtmlAttr.class "page" ]
        [ Html.node "style" [] [ text stylesheet ]
        , h2 [] [ text "Uebung 10: Treemaps" ]
        , sectionView "10.1 Einfache Treemap - testTree" [ drawTreemapSimple model.testTree ]
        , case model.flare of
            Just flareTree ->
                div []
                    [ sectionView "10.1 Einfache Treemap - flare.json" [ drawTreemapSimple flareTree ]
                    , sectionView "10.2 Squarified Treemap - Farben ab Ebene 1" [ drawTreemapSquarified ColorByLevel1 flareTree ]
                    , sectionView "10.2 Squarified Treemap - Farben ab Ebene 2" [ drawTreemapSquarified ColorByLevel2 flareTree ]
                    , sectionView "Kontrollausgabe" [ treeAsHtml flareTree ]
                    ]

            Nothing ->
                div [ HtmlAttr.class "notice" ] [ text model.statusMsg ]
        ]


sectionView : String -> List (Html Msg) -> Html Msg
sectionView title content =
    div [ HtmlAttr.class "section" ]
        (h3 [] [ text title ] :: content)


drawTreemapSimple : Tree ( String, Int ) -> Html Msg
drawTreemapSimple tree =
    let
        canvasWidth =
            760

        canvasHeight =
            760

        padding =
            16
    in
    svg
        [ SvgAttr.class [ "treemap-svg" ]
        , SvgAttr.viewBox 0 0 (canvasWidth + 2 * padding) (canvasHeight + 2 * padding)
        ]
        [ Svg.g [ SvgAttr.transform [ SvgTypes.Translate padding padding ] ]
            (drawTreeNode True 0 0 canvasWidth canvasHeight tree)
        ]


drawTreeNode : Bool -> Float -> Float -> Float -> Float -> Tree ( String, Int ) -> List (Svg Msg)
drawTreeNode splitX x y width height tree =
    let
        nodeRectangle =
            rect
                [ SvgPx.x x
                , SvgPx.y y
                , SvgPx.width width
                , SvgPx.height height
                , SvgAttr.class [ "simple-rect" ]
                ]
                []

        children =
            Tree.children tree

        childTotal =
            children
                |> List.map (Tree.label >> Tuple.second)
                |> List.sum
                |> toFloat

        childRectangles =
            if List.isEmpty children || childTotal <= 0 then
                []

            else
                children
                    |> List.foldl
                        (\child ( offset, acc ) ->
                            let
                                childValue =
                                    Tree.label child
                                        |> Tuple.second
                                        |> toFloat

                                childSize =
                                    childValue / childTotal

                                nextWidth =
                                    if splitX then
                                        width * childSize

                                    else
                                        width

                                nextHeight =
                                    if splitX then
                                        height

                                    else
                                        height * childSize

                                nextX =
                                    if splitX then
                                        offset

                                    else
                                        x

                                nextY =
                                    if splitX then
                                        y

                                    else
                                        offset

                                nextOffset =
                                    if splitX then
                                        offset + nextWidth

                                    else
                                        offset + nextHeight
                            in
                            ( nextOffset
                            , acc ++ drawTreeNode (not splitX) nextX nextY nextWidth nextHeight child
                            )
                        )
                        ( if splitX then x else y, [] )
                    |> Tuple.second
    in
    nodeRectangle :: childRectangles


type ColorMode
    = ColorByLevel1
    | ColorByLevel2


drawTreemapSquarified : ColorMode -> Tree ( String, Int ) -> Html Msg
drawTreemapSquarified mode tree =
    let
        canvasWidth =
            920

        canvasHeight =
            620

        padding =
            12

        positionedTree =
            Hierarchy.treemap
                [ Hierarchy.size canvasWidth canvasHeight
                , Hierarchy.paddingInner (\_ -> 1)
                , Hierarchy.paddingOuter (\_ -> 0)
                , Hierarchy.tile Hierarchy.squarify
                ]
                (Tuple.second >> toFloat)
                tree
    in
    svg
        [ SvgAttr.class [ "treemap-svg", "squarified-svg" ]
        , SvgAttr.viewBox 0 0 (canvasWidth + 2 * padding) (canvasHeight + 2 * padding)
        ]
        [ Svg.g [ SvgAttr.transform [ SvgTypes.Translate padding padding ] ]
            (squarifiedNodes mode positionedTree)
        ]


squarifiedNodes :
    ColorMode
    -> Tree PositionedNode
    -> List (Svg Msg)
squarifiedNodes mode tree =
    Tree.depthFirstFold
        (\acc ancestors node _ ->
            Tree.Continue
                (case ancestors of
                    [] ->
                        rootBorder node :: acc

                    _ ->
                        squarifiedRect mode ancestors node :: labelIfRoom node ++ acc
                )
        )
        []
        tree
        |> List.reverse


squarifiedRect :
    ColorMode
    -> List PositionedNode
    -> PositionedNode
    -> Svg Msg
squarifiedRect mode ancestors node =
    rect
        [ SvgPx.x node.x
        , SvgPx.y node.y
        , SvgPx.width node.width
        , SvgPx.height node.height
        , SvgAttr.class [ "squarified-rect", colorClassForNode mode ancestors node ]
        ]
        []


rootBorder : PositionedNode -> Svg Msg
rootBorder node =
    rect
        [ SvgPx.x node.x
        , SvgPx.y node.y
        , SvgPx.width node.width
        , SvgPx.height node.height
        , SvgAttr.class [ "root-border" ]
        ]
        []


labelIfRoom : PositionedNode -> List (Svg Msg)
labelIfRoom node =
    let
        name =
            Tuple.first node.node
    in
    if node.width > 82 && node.height > 26 && String.length name < 18 then
        [ text_
            [ SvgPx.x (node.x + 4)
            , SvgPx.y (node.y + 15)
            , SvgAttr.class [ "map-label" ]
            ]
            [ SvgCore.text name ]
        ]

    else
        []


colorClassForNode :
    ColorMode
    -> List PositionedNode
    -> PositionedNode
    -> String
colorClassForNode mode ancestors node =
    let
        path =
            List.reverse (node :: ancestors)
                |> List.map (.node >> Tuple.first)

        key =
            case mode of
                ColorByLevel1 ->
                    itemAt 1 path
                        |> Maybe.withDefault "root"

                ColorByLevel2 ->
                    itemAt 2 path
                        |> Maybe.withDefault (itemAt 1 path |> Maybe.withDefault "root")
    in
    "color-" ++ String.fromInt (paletteIndex key)


itemAt : Int -> List a -> Maybe a
itemAt index list =
    list
        |> List.drop index
        |> List.head


paletteIndex : String -> Int
paletteIndex key =
    hashString key |> modBy 12


hashString : String -> Int
hashString value =
    value
        |> String.toList
        |> List.foldl (\char acc -> acc + Char.toCode char) 0


treeAsHtml : Tree ( String, Int ) -> Html msg
treeAsHtml tree =
    Tree.restructure labelToHtml toListItem tree
        |> \root -> Html.ul [ HtmlAttr.class "tree-list" ] [ root ]


labelToHtml : ( String, Int ) -> Html msg
labelToHtml ( label, value ) =
    text (label ++ " " ++ String.fromInt value)


toListItem : Html msg -> List (Html msg) -> Html msg
toListItem label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Ungueltige URL: " ++ url

        Http.Timeout ->
            "Zeitueberschreitung beim Laden der Datei."

        Http.NetworkError ->
            "flare.json konnte nicht geladen werden. Starte die Seite ueber einen lokalen Webserver im Ordner uebung/10."

        Http.BadStatus status ->
            "HTTP-Fehler " ++ String.fromInt status

        Http.BadBody message ->
            "JSON-Decoder-Fehler: " ++ message


stylesheet : String
stylesheet =
    """
body {
  margin: 0;
  background: #f6f7f8;
  color: #1e252b;
  font-family: Arial, Helvetica, sans-serif;
}

.page {
  padding: 18px 22px 34px;
}

h2 {
  margin: 0 0 16px;
  font-size: 22px;
  font-weight: 700;
}

h3 {
  margin: 22px 0 10px;
  font-size: 16px;
  font-weight: 700;
}

.section {
  max-width: 1000px;
}

.treemap-svg {
  display: block;
  width: min(100%, 820px);
  height: auto;
  background: #ffffff;
}

.squarified-svg {
  width: min(100%, 960px);
}

.simple-rect {
  fill: none;
  stroke: #111111;
  stroke-width: 1.4px;
}

.root-border {
  fill: none;
  stroke: #111111;
  stroke-width: 1.5px;
}

.squarified-rect {
  stroke: #ffffff;
  stroke-width: 1px;
}

.color-0 { fill: #457b9d; }
.color-1 { fill: #2a9d8f; }
.color-2 { fill: #e9c46a; }
.color-3 { fill: #f4a261; }
.color-4 { fill: #e76f51; }
.color-5 { fill: #755b95; }
.color-6 { fill: #50a05f; }
.color-7 { fill: #be4b82; }
.color-8 { fill: #5885af; }
.color-9 { fill: #d27d2d; }
.color-10 { fill: #699178; }
.color-11 { fill: #965f4b; }

.map-label {
  fill: #111111;
  font-size: 11px;
  pointer-events: none;
}

.tree-list {
  margin-top: 10px;
  padding-left: 22px;
  font-size: 13px;
  line-height: 1.25;
}

.notice {
  padding: 16px;
  background: #ffffff;
  border: 1px solid #c8ced3;
}
"""
