module Main exposing (main)

{-| Einstiegspunkt der Anwendung.

Der Datensatz kommt als Flag aus `index.html`. Das Dekodieren kann
fehlschlagen, deshalb liegt im Model ein `Result` und nicht der nackte
Datensatz: Die view-Funktion muss den Fehlerfall damit behandeln.

Der gesamte Interaktionszustand liegt hier zentral. Die Ansichten halten
keinen eigenen Zustand, sondern lesen aus diesem Model -- damit ist ihre
Kopplung strukturell erzwungen und nicht nur Konvention.

-}

import Browser
import Data exposing (Dataset)
import Html exposing (Html, div, h1, node, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Views.Heatmap as Heatmap
import Views.Layout as Layout
import Views.TimeSeries as TimeSeries


type alias Model =
    { dataset : Result String Dataset
    , selectedMonth : Maybe Int
    , hoveredDay : Maybe String
    , selectedDay : Maybe String
    }


type Msg
    = SelectMonth (Maybe Int)
    | HoverDay String
    | LeaveDay
    | SelectDay String


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( { dataset =
            Decode.decodeValue Data.datasetDecoder flags
                |> Result.mapError Decode.errorToString
      , selectedMonth = Nothing
      , hoveredDay = Nothing
      , selectedDay = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMonth month ->
            ( { model | selectedMonth = month }, Cmd.none )

        HoverDay date ->
            ( { model | hoveredDay = Just date }, Cmd.none )

        LeaveDay ->
            ( { model | hoveredDay = Nothing }, Cmd.none )

        SelectDay date ->
            ( { model | selectedDay = Just date }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ node "style" [] [ text Layout.stylesheet ]
        , case model.dataset of
            Err error ->
                div [ class "content" ]
                    [ h1 [] [ text "EnergyCharts Visual Analytics" ]
                    , p [] [ text ("Der Datensatz konnte nicht dekodiert werden: " ++ error) ]
                    ]

            Ok dataset ->
                viewApp model dataset
        ]


viewApp : Model -> Dataset -> Html Msg
viewApp model dataset =
    let
        filteredHourly =
            Data.filterHourly model.selectedMonth dataset.hourly

        filteredDaily =
            Data.filterDaily model.selectedMonth dataset.daily
    in
    div []
        [ div [ class "topbar" ]
            [ h1 [] [ text "EnergyCharts Visual Analytics" ]
            , p []
                [ text
                    ("Deutschland "
                        ++ String.fromInt dataset.meta.year
                        ++ ", Strommarkt "
                        ++ dataset.meta.priceMarket
                        ++ ". Wie hängen Solar- und Windmuster mit der Deckung durch "
                        ++ "erneuerbare Energien (EE), Preisen und Nettohandel zusammen?"
                    )
                ]
            ]
        , div [ class "content" ]
            [ Layout.monthControls model.selectedMonth SelectMonth
            , Layout.metricCards filteredHourly filteredDaily
            , Layout.section "Zeitreihen-Übersicht"
                (TimeSeries.view
                    { selectedMonth = model.selectedMonth
                    , onSelectMonth = \month -> SelectMonth (Just month)
                    }
                    dataset.daily
                )
            , Layout.section "Stunden-Heatmap (pixelorientiert)"
                (Heatmap.view
                    { hoveredDay = model.hoveredDay
                    , selectedDay = model.selectedDay
                    , onHoverDay = HoverDay
                    , onLeave = LeaveDay
                    , onSelectDay = SelectDay
                    }
                    filteredHourly
                )
            ]
        ]
