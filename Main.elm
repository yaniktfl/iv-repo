module Main exposing (main)

import Browser
import Color
import Csv
import Csv.Decode
import Date exposing (Interval(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Scale.Color
import Time exposing (Weekday(..))
import TypedSvg exposing (g, rect, svg, title)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Core as SvgCore
import TypedSvg.Types exposing (Paint(..), Transform(..))


type alias StockDay =
    ( String, Maybe Float )


type alias Series =
    { name : String
    , file : String
    , rawCount : Int
    , calendarData : List StockDay
    , weekdayData : List StockDay
    }


type alias Level =
    { width : Int
    , height : Int
    }


type alias PixelPosition =
    { x : Int
    , y : Int
    }


type alias RecordedData =
    { position : PixelPosition
    , value : StockDay
    }


type alias Model =
    { selected : String
    , weekendMode : WeekendMode
    , loaded : Dict String Series
    , failed : List String
    }


type WeekendMode
    = WithWeekends
    | WithoutWeekends


type Msg
    = GotCsv String (Result Http.Error String)
    | SelectSeries String
    | ToggleWeekendMode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selected = "DJ", weekendMode = WithWeekends, loaded = Dict.empty, failed = [] }
    , files
        |> List.map loadSeries
        |> Cmd.batch
    )


files : List ( String, String )
files =
    [ ( "DJ", "DJ.csv" )
    , ( "NIKKEI", "NIKKEI.csv" )
    , ( "HANGSENG", "HANGSENG.csv" )
    , ( "DAX", "DAX.csv" )
    , ( "BOVESPA", "BOVESPA.csv" )
    ]


loadSeries : ( String, String ) -> Cmd Msg
loadSeries ( name, file ) =
    Http.get
        { url = file
        , expect = Http.expectString (GotCsv name)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCsv name result ->
            case result of
                Ok csvRaw ->
                    let
                        rawData =
                            csvStringToData csvRaw

                        series =
                            { name = name
                            , file = name ++ ".csv"
                            , rawCount = List.length rawData
                            , calendarData = fillMissingDays allDays rawData
                            , weekdayData = fillMissingDays allWeekdays rawData
                            }
                    in
                    ( { model | loaded = Dict.insert name series model.loaded }, Cmd.none )

                Err _ ->
                    ( { model | failed = name :: model.failed }, Cmd.none )

        SelectSeries name ->
            ( { model | selected = name }, Cmd.none )

        ToggleWeekendMode ->
            ( { model
                | weekendMode =
                    case model.weekendMode of
                        WithWeekends ->
                            WithoutWeekends

                        WithoutWeekends ->
                            WithWeekends
              }
            , Cmd.none
            )


csvStringToData : String -> List StockDay
csvStringToData csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\date open -> ( date, Just open ))
        (Csv.Decode.field "Date" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "Open"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                )
        )


startDate : Date.Date
startDate =
    Date.fromIsoString "1980-12-23"
        |> Result.withDefault (Date.fromRataDie 1)


endDateExclusive : Date.Date
endDateExclusive =
    Date.fromIsoString "2011-06-10"
        |> Result.withDefault (Date.fromRataDie 1)


allDays : List String
allDays =
    Date.range Day 1 startDate endDateExclusive
        |> List.map Date.toIsoString


allWeekdays : List String
allWeekdays =
    Date.range Day 1 startDate endDateExclusive
        |> List.filter isTradingWeekday
        |> List.map Date.toIsoString


isTradingWeekday : Date.Date -> Bool
isTradingWeekday date =
    case Date.weekday date of
        Sat ->
            False

        Sun ->
            False

        _ ->
            True


fillMissingDays : List String -> List StockDay -> List StockDay
fillMissingDays targetDays data =
    let
        emptyDays : Dict String (Maybe Float)
        emptyDays =
            targetDays
                |> List.map (\date -> ( date, Nothing ))
                |> Dict.fromList

        actualDays : Dict String (Maybe Float)
        actualDays =
            data
                |> List.filter (\( date, _ ) -> Dict.member date emptyDays)
                |> Dict.fromList
    in
    Dict.union actualDays emptyDays
        |> Dict.toList


levels : List Level
levels =
    [ Level 5 1
    , Level 1 12
    , Level 4 1
    , Level 2 3
    , Level 3 3
    , Level 1 1
    ]


weekdayLevels : List Level
weekdayLevels =
    [ Level 5 1
    , Level 1 9
    , Level 4 1
    , Level 2 3
    , Level 3 3
    , Level 1 1
    ]


pixelListFor : WeekendMode -> List PixelPosition
pixelListFor mode =
    createPixelMap (levelsFor mode)


levelsFor : WeekendMode -> List Level
levelsFor mode =
    case mode of
        WithWeekends ->
            levels

        WithoutWeekends ->
            weekdayLevels


pixelWidthFor : WeekendMode -> Int
pixelWidthFor mode =
    levelsFor mode
        |> List.map .width
        |> List.product


pixelHeightFor : WeekendMode -> Int
pixelHeightFor mode =
    levelsFor mode
        |> List.map .height
        |> List.product


cellSize : Float
cellSize =
    6


createPixelMap : List Level -> List PixelPosition
createPixelMap remainingLevels =
    case remainingLevels of
        [] ->
            [ PixelPosition 0 0 ]

        level :: rest ->
            let
                inner =
                    createPixelMap rest

                innerWidth =
                    rest |> List.map .width |> List.product

                innerHeight =
                    rest |> List.map .height |> List.product

                offsets =
                    List.range 0 (level.height - 1)
                        |> List.concatMap
                            (\localY ->
                                List.range 0 (level.width - 1)
                                    |> List.map (\localX -> ( localX, localY ))
                            )
            in
            offsets
                |> List.concatMap
                    (\( localX, localY ) ->
                        inner
                            |> List.map
                                (\position ->
                                    { x = localX * innerWidth + position.x
                                    , y = localY * innerHeight + position.y
                                    }
                                )
                    )


view : Model -> Html Msg
view model =
    let
        maybeSeries =
            Dict.get model.selected model.loaded
    in
    div [ class "page" ]
        [ Html.node "style" [] [ text css ]
        , h1 [] [ text "Übung 7: Recursive-Pattern-Technik" ]
        , div [ class "toolbar" ]
            (List.map (seriesButton model) files
                ++ [ button [ onClick ToggleWeekendMode, class "mode" ]
                        [ text <|
                            case model.weekendMode of
                                WithWeekends ->
                                    "Wochenenden entfernen"

                                WithoutWeekends ->
                                    "Kalendertage zeigen"
                        ]
                   ]
            )
        , case maybeSeries of
            Just series ->
                viewSeries model.weekendMode series

            Nothing ->
                p [ class "status" ]
                    [ text <|
                        if List.member model.selected model.failed then
                            "Fehler beim Laden von " ++ model.selected ++ ".csv"

                        else
                            "Loading..."
                    ]
        ]


seriesButton : Model -> ( String, String ) -> Html Msg
seriesButton model ( name, _ ) =
    button
        [ onClick (SelectSeries name)
        , class <|
            if model.selected == name then
                "active"

            else
                ""
        ]
        [ text name ]


viewSeries : WeekendMode -> Series -> Html Msg
viewSeries mode series =
    let
        data =
            case mode of
                WithWeekends ->
                    series.calendarData

                WithoutWeekends ->
                    series.weekdayData

        currentPixelList =
            pixelListFor mode

        drawnData =
            List.map2 RecordedData currentPixelList data

        values =
            data
                |> List.filterMap Tuple.second

        mappingSize =
            List.length currentPixelList

        minX =
            0

        maxX =
            pixelWidthFor mode - 1

        minY =
            0

        maxY =
            pixelHeightFor mode - 1
    in
    div []
        [ p [ class "status" ]
            [ text <|
                "Loaded: "
                    ++ series.name
                    ++ "("
                    ++ String.fromInt (List.length data)
                    ++ "), raw "
                    ++ String.fromInt series.rawCount
                    ++ ", missing "
                    ++ String.fromInt (List.length data - List.length values)
            ]
        , p [ class "status" ]
            [ text <|
                "PixelMapping Size: "
                    ++ String.fromInt mappingSize
                    ++ " Min X"
                    ++ String.fromInt minX
                    ++ " Max X"
                    ++ String.fromInt maxX
                    ++ " Min Y"
                    ++ String.fromInt minY
                    ++ " Max Y"
                    ++ String.fromInt maxY
            ]
        , svg
            [ Px.width (toFloat (pixelWidthFor mode) * cellSize)
            , Px.height (toFloat (pixelHeightFor mode) * cellSize)
            , SvgAttr.viewBox 0 0 (toFloat (pixelWidthFor mode) * cellSize) (toFloat (pixelHeightFor mode) * cellSize)
            , SvgAttr.class [ "recursive-pattern" ]
            ]
            [ g [] (List.map (drawCell values) drawnData) ]
        ]


drawCell : List Float -> RecordedData -> SvgCore.Svg Msg
drawCell values recorded =
    let
        ( date, maybeValue ) =
            recorded.value

        label =
            date
                ++ ": "
                ++ Maybe.withDefault "N.A." (Maybe.map String.fromFloat maybeValue)
    in
    rect
        [ Px.x (toFloat recorded.position.x * cellSize)
        , Px.y (toFloat recorded.position.y * cellSize)
        , Px.width cellSize
        , Px.height cellSize
        , SvgAttr.fill (Paint (valueColor values maybeValue))
        ]
        [ title [] [ SvgCore.text label ] ]


valueColor : List Float -> Maybe Float -> Color.Color
valueColor values maybeValue =
    case maybeValue of
        Just value ->
            Scale.Color.viridisInterpolator (normalize values value)

        Nothing ->
            Color.rgb255 188 198 190


normalize : List Float -> Float -> Float
normalize values value =
    case ( List.minimum values, List.maximum values ) of
        ( Just minValue, Just maxValue ) ->
            if minValue == maxValue then
                0.5

            else
                (value - minValue) / (maxValue - minValue)
                    |> clamp 0 1

        _ ->
            0.5


css : String
css =
    """
body {
  margin: 0;
  font-family: Georgia, "Times New Roman", serif;
  color: #050505;
  background: white;
}

.page {
  padding: 0 0 40px;
}

h1 {
  margin: 0 0 4px;
  font-size: 20px;
  font-weight: 600;
}

.toolbar {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  margin: 0 0 8px;
}

button {
  border: 1px solid #9a9a9a;
  background: #f7f7f7;
  padding: 4px 8px;
  font: inherit;
  cursor: pointer;
}

button.active {
  color: white;
  background: #222;
  border-color: #222;
}

.status {
  margin: 0;
  font-size: 20px;
  line-height: 1.05;
}

.recursive-pattern {
  display: block;
  margin-top: 0;
}
"""
