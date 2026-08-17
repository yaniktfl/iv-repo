module Main exposing (main)

{-| Einstiegspunkt der Anwendung.

Der Datensatz kommt als Flag aus `index.html`. Das Dekodieren kann
fehlschlagen, deshalb liegt im Model ein `Result` und nicht der nackte
Datensatz: Die view-Funktion muss den Fehlerfall damit behandeln.

-}

import Browser
import Data exposing (Dataset)
import Html exposing (Html, div, h1, p, text)
import Json.Decode as Decode


type alias Model =
    { dataset : Result String Dataset
    }


type Msg
    = NoOp


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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "EnergyCharts Visual Analytics" ]
        , case model.dataset of
            Err error ->
                p [] [ text ("Der Datensatz konnte nicht dekodiert werden: " ++ error) ]

            Ok dataset ->
                p []
                    [ text
                        ("Deutschland "
                            ++ String.fromInt dataset.meta.year
                            ++ ", Strommarkt "
                            ++ dataset.meta.priceMarket
                            ++ ": "
                            ++ String.fromInt (List.length dataset.hourly)
                            ++ " Stundenwerte, "
                            ++ String.fromInt (List.length dataset.daily)
                            ++ " Tagesprofile."
                        )
                    ]
        ]
