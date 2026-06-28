module Main exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import List.Extra
import Scale
import Statistics
import Tree exposing (Tree)
import Tree.Zipper exposing (Zipper)
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


type alias Model =
    { testTree : Tree ( String, Int ), tree : Maybe (Tree ( String, Int )), statusMsg : String }


init : () -> ( Model, Cmd Msg )
init () =
    ( { testTree =
            Tree.tree ( "a", 10 )
                [ Tree.tree ( "b", 3 )
                    [ Tree.singleton ( "d", 1 ), Tree.singleton ( "e", 2 ) ]
                , Tree.tree ( "c", 7 )
                    [ Tree.tree ( "f", 3 )
                        [ Tree.singleton ( "i", 1 )
                        , Tree.tree ( "j", 2 )
                            [ Tree.singleton ( "k", 1 ), Tree.singleton ( "l", 1 ) ]
                        ]
                    , Tree.singleton ( "g", 2 )
                    , Tree.singleton ( "h", 2 )
                    ]
                ]
      , tree = Nothing
      , statusMsg = "Loading ..."
      }
    , Http.get
        { url = "https://cors-anywhere.herokuapp.com/https://users.informatik.uni-halle.de/~hinnebur/Lehre/InfoVis/U08/flare.json"
        , expect = Http.expectJson GotFlare treeDecoder
        }
    )


type Msg
    = GotFlare (Result Http.Error (Tree ( String, Maybe Int )))



-- {
--  "name": "flare",
--  "children": [
--   {
--    "name": "analytics",
--    "children": [
--     {
--      "name": "cluster",
--      "children": [
--       {"name": "AgglomerativeCluster", "value": 3938},
--       {"name": "CommunityStructure", "value": 3812},


treeDecoder : Json.Decode.Decoder (Tree ( String, Maybe Int ))
treeDecoder =
    Json.Decode.map3
        (\name value children ->
            case children of
                Nothing ->
                    Tree.tree ( name, value ) []

                Just c ->
                    Tree.tree ( name, value ) c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "value" Json.Decode.int)
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            let
                t : Tree ( String, Int )
                t =
                    newTree
                        |> Tree.map (\( a, b ) -> ( a, Maybe.withDefault 0 b ))

                computeSumofChildren : Tree ( String, Int ) -> Tree ( String, Int )
                computeSumofChildren t_ =
                    let
                        newChildren =
                            Tree.children t_
                                |> List.map computeSumofChildren

                        s =
                            (t_ :: newChildren)
                                |> List.map
                                    (Tree.label >> Tuple.second)
                                |> List.sum
                    in
                    Tree.tree ( Tree.label t_ |> Tuple.first, s ) newChildren
            in
            ( { model | tree = Just <| computeSumofChildren t, statusMsg = "No Error" }, Cmd.none )

        GotFlare (Err error) ->
            ( { model
                | tree = Nothing
                , statusMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            ""
              }
            , Cmd.none
            )


drawTreemapSimple : Tree ( String, Int ) -> Html Msg
drawTreemapSimple t =
    let
        w =
            500

        h =
            500

        padding =
            20
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.preserveAspectRatio (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin) TypedSvg.Types.Slice
        ]
        [ TypedSvg.g [ TypedSvg.Attributes.transform [ Translate padding padding ] ] <| drawTreeNode True 0 0 w h t ]


drawTreeNode : Bool -> Float -> Float -> Float -> Float -> Tree ( String, Int ) -> List (Svg Msg)
drawTreeNode splitX x y w h t =
    -- Ergänzen Sie diese Funktion zum Zeichnen der TreeMap
    [ TypedSvg.rect
        [ TypedSvg.Attributes.x <| TypedSvg.Types.px x
        , TypedSvg.Attributes.y <| TypedSvg.Types.px y
        , TypedSvg.Attributes.width <| TypedSvg.Types.px w
        , TypedSvg.Attributes.height <| TypedSvg.Types.px h
        , TypedSvg.Attributes.stroke <| TypedSvg.Types.Paint Color.black
        , TypedSvg.Attributes.fill <| TypedSvg.Types.PaintNone
        ]
        []
    ]


labelToHtml : ( String, Int ) -> Html msg
labelToHtml ( l, i ) =
    Html.text <| l ++ " " ++ String.fromInt i


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ drawTreemapSimple model.testTree ]
        , model.tree
            |> Maybe.map drawTreemapSimple
            |> Maybe.withDefault (Html.text model.statusMsg)
        , model.tree
            |> Maybe.map
                (\t ->
                    Tree.restructure
                        labelToHtml
                        toListItems
                        t
                        |> (\root -> Html.ul [] [ root ])
                )
            |> Maybe.withDefault (div [] [])
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }
