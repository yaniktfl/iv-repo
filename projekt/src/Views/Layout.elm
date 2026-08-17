module Views.Layout exposing (metricCards, monthControls, section, stylesheet)

{-| Rahmen der Anwendung: Monatsfilter, Kennzahlenkarten, Abschnittsueberschriften
und das Stylesheet.

Das CSS wird als String gefuehrt und in `Main` in ein `<style>`-Element
gehaengt, damit die Anwendung ohne weitere Dateien ausgeliefert werden kann.

-}

import Data exposing (Daily, Hourly)
import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


{-| Schaltflaechenleiste fuer den Monatsfilter. `Nothing` steht fuer "Alle". -}
monthControls : Maybe Int -> (Maybe Int -> msg) -> Html msg
monthControls selectedMonth selectMonth =
    div [ class "month-controls" ]
        (button
            [ classList [ ( "active", selectedMonth == Nothing ) ]
            , onClick (selectMonth Nothing)
            ]
            [ text "Alle" ]
            :: List.map
                (\month ->
                    button
                        [ classList [ ( "active", selectedMonth == Just month ) ]
                        , onClick (selectMonth (Just month))
                        ]
                        [ text (Data.monthName month) ]
                )
                (List.range 1 12)
        )


{-| Kennzahlen zum aktuell gefilterten Ausschnitt. -}
metricCards : List Hourly -> List Daily -> Html msg
metricCards hourly daily =
    let
        avgRenewable =
            mean (List.map .renewableShare hourly)

        avgPrice =
            mean (List.filterMap .priceEurMwh hourly)

        maxShare =
            hourly |> List.map .renewableShare |> List.maximum |> Maybe.withDefault 0

        negHours =
            daily |> List.map .negativePriceHours |> List.sum
    in
    div [ class "metrics" ]
        [ card "Stundenwerte" (String.fromInt (List.length hourly))
        , card "Tagesprofile" (String.fromInt (List.length daily))
        , card "Ø EE-Anteil" (Data.formatFloat 1 avgRenewable ++ " %")
        , card "Max. EE-Anteil" (Data.formatFloat 1 maxShare ++ " %")
        , card "Ø Preis" (Data.formatFloat 1 avgPrice ++ " €/MWh")
        , card "Neg. Preisstunden" (String.fromInt negHours)
        ]


section : String -> Html msg -> Html msg
section title child =
    div [ class "section" ]
        [ h2 [] [ text title ]
        , child
        ]


card : String -> String -> Html msg
card label value =
    div [ class "metric" ]
        [ span [ class "metric-value" ] [ text value ]
        , span [ class "metric-label" ] [ text label ]
        ]


mean : List Float -> Float
mean values =
    case values of
        [] ->
            0

        _ ->
            List.sum values / toFloat (List.length values)


classList : List ( String, Bool ) -> Html.Attribute msg
classList pairs =
    pairs
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> String.join " "
        |> class


stylesheet : String
stylesheet =
    """
* { box-sizing: border-box; }
body { margin: 0; font-family: Inter, Segoe UI, Arial, sans-serif; color: #1d252c; background: #f4f6f2; }
.page { min-height: 100vh; }
.topbar { padding: 22px 28px 16px; background: #ffffff; border-bottom: 1px solid #d9ded8; }
h1 { margin: 0 0 6px; font-size: 28px; letter-spacing: 0; }
h2 { margin: 0 0 12px; font-size: 17px; letter-spacing: 0; }
p { margin: 0; color: #51606b; line-height: 1.45; }
.content { max-width: 1180px; margin: 0 auto; padding: 18px; }
.month-controls { display: flex; flex-wrap: wrap; gap: 6px; margin: 12px 0 16px; }
button { border: 1px solid #b9c3bd; background: #ffffff; color: #23313a; padding: 7px 10px; border-radius: 6px; cursor: pointer; }
button:hover, button.active { background: #214e57; border-color: #214e57; color: #ffffff; }
.metrics { display: grid; grid-template-columns: repeat(6, minmax(0, 1fr)); gap: 10px; margin-bottom: 16px; }
.metric { background: #ffffff; border: 1px solid #d9ded8; border-radius: 8px; padding: 10px 12px; min-width: 0; }
.metric-value { display: block; font-size: 20px; font-weight: 700; color: #173b42; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.metric-label { display: block; margin-top: 4px; font-size: 12px; color: #65727a; }
.section { background: #ffffff; border: 1px solid #d9ded8; border-radius: 8px; padding: 14px; margin-bottom: 16px; overflow: hidden; }
.chart { display: block; width: 100%; height: auto; }
.chart-label { font-size: 13px; font-weight: 700; fill: #26343c; }
.tick-label, .month-label { font-size: 11px; fill: #64717a; }
.grid-line { stroke: #e3e7e3; stroke-width: 1px; }
.axis-line { stroke: #9aa59f; stroke-width: 1px; }
.month-bar { fill: #76b6a2; opacity: 0.78; cursor: pointer; }
.month-bar:hover, .month-bar.active { fill: #214e57; opacity: 1; }
.legend-swatch { opacity: 0.9; }
.price-line { fill: none; stroke: #c04f3f; stroke-width: 2.5px; }
.price-dot { fill: #c04f3f; cursor: pointer; }
@media (max-width: 900px) {
  .metrics { grid-template-columns: 1fr; }
  .content { padding: 12px; }
}
"""
