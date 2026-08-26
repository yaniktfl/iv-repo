module Views.Layout exposing (detailPanel, metricCards, monthControls, section, stylesheet, tooltipView)

{-| Rahmen der Anwendung: Monatsfilter, Kennzahlenkarten, Abschnittsueberschriften
und das Stylesheet.

Das CSS wird als String gefuehrt und in `Main` in ein `<style>`-Element
gehaengt, damit die Anwendung ohne weitere Dateien ausgeliefert werden kann.

-}

import Data exposing (Daily, Hourly, Tooltip)
import Html exposing (Html, button, div, h2, p, span, table, td, text, th, tr)
import Html.Attributes as HtmlAttr exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)


{-| Schaltflaechenleiste fuer den Monatsfilter. `Nothing` steht fuer "Alle".
-}
monthControls : Set Int -> (Maybe Int -> msg) -> Html msg
monthControls selectedMonths toggleMonth =
    div [ class "month-controls" ]
        (button
            [ classList [ ( "active", Set.isEmpty selectedMonths ) ]
            , onClick (toggleMonth Nothing)
            ]
            [ text "Alle" ]
            :: List.map
                (\month ->
                    button
                        [ classList [ ( "active", Set.member month selectedMonths ) ]
                        , onClick (toggleMonth (Just month))
                        ]
                        [ text (Data.monthName month) ]
                )
                (List.range 1 12)
        )


{-| Kennzahlen zum aktuell gefilterten Ausschnitt.
-}
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


type alias DetailConfig msg =
    { focus : Maybe Daily
    , focusHourly : List Hourly
    , selected : List ( Daily, List Hourly )
    , onRemoveDay : String -> msg
    , onClearDays : msg
    }


{-| Details on demand: Kennzahlen und Stundenprofil des Tages, der gerade
ueberfahren oder ausgewaehlt ist.
-}
detailPanel : DetailConfig msg -> Html msg
detailPanel config =
    div [ class "detail" ]
        (h2 [] [ text "Tagesdetails" ]
            :: selectedBlock config
            ++ compareBlock config
            ++ focusBlock config
        )


{-| Liste der dauerhaft ausgewaehlten Tage; jeder einzeln abwaehlbar.
-}
selectedBlock : DetailConfig msg -> List (Html msg)
selectedBlock config =
    if List.isEmpty config.selected then
        []

    else
        [ p [ class "detail-hint" ]
            [ text ("Ausgewählte Tage (" ++ String.fromInt (List.length config.selected) ++ ")") ]
        , div [ class "chips" ]
            (List.map (\( day, _ ) -> chip config.onRemoveDay day) config.selected)
        , button [ class "clear-days", onClick config.onClearDays ] [ text "Auswahl aufheben" ]
        ]


chip : (String -> msg) -> Daily -> Html msg
chip onRemove day =
    div [ class "chip" ]
        [ span []
            [ text
                (day.date
                    ++ " · EE "
                    ++ Data.formatFloat 0 day.meanRenewableShare
                    ++ " % · "
                    ++ (day.meanPriceEurMwh
                            |> Maybe.map (\price -> Data.formatFloat 0 price ++ " €/MWh")
                            |> Maybe.withDefault "–"
                       )
                )
            ]
        , button
            [ onClick (onRemove day.date), HtmlAttr.title "Tag abwählen" ]
            [ text "×" ]
        ]


focusBlock : DetailConfig msg -> List (Html msg)
focusBlock config =
    case config.focus of
        Nothing ->
            [ p [ class "empty" ]
                [ text "Zelle in der Heatmap oder Linie in den parallelen Koordinaten überfahren bzw. anklicken (Mehrfachauswahl möglich), um einen Tag zu inspizieren." ]
            ]

        Just day ->
            [ p [ class "detail-hint" ] [ text day.date ]
            , div [ class "detail-grid" ]
                [ detail "EE-Anteil" (Data.formatFloat 1 day.meanRenewableShare ++ " %")
                , detail "Solaranteil" (Data.formatFloat 1 day.solarShare ++ " %")
                , detail "Windanteil" (Data.formatFloat 1 day.windShare ++ " %")
                , detail "Ø Last" (Data.formatFloat 1 day.meanLoadGw ++ " GW")
                , detail "Nettohandel" (Data.formatFloat 1 day.meanNetImportGw ++ " GW")
                , detail "Ø Preis"
                    (day.meanPriceEurMwh
                        |> Maybe.map (\price -> Data.formatFloat 1 price ++ " €/MWh")
                        |> Maybe.withDefault "–"
                    )
                ]
            , p [ class "detail-hint" ] [ text "EE-Anteil je Stunde" ]
            , sparkline config.focusHourly
            ]


{-| Gegenueberstellung ab zwei ausgewaehlten Tagen. Das Detailpanel darunter
folgt weiterhin dem Mauszeiger und zeigt immer nur *einen* Tag; erst diese
Tabelle macht die Auswahl als Ganzes lesbar. Die Stundenprofile stehen
untereinander, weil sich die Tagesform -- Solarbogen gegen flaches Windband --
im direkten Vergleich am schnellsten erschliesst.
-}
compareBlock : DetailConfig msg -> List (Html msg)
compareBlock config =
    if List.length config.selected < 2 then
        []

    else
        let
            days =
                List.map Tuple.first config.selected
        in
        [ p [ class "detail-hint" ]
            [ text ("Vergleich (" ++ String.fromInt (List.length days) ++ " Tage)") ]
        , div [ class "compare-scroll" ]
            [ table [ class "compare" ]
                (tr []
                    (th [] [ text "" ]
                        :: List.map (\day -> th [] [ text (shortDate day.date) ]) days
                    )
                    :: List.map (compareRow days) compareFields
                )
            ]
        , p [ class "detail-hint" ] [ text "EE-Anteil je Stunde im Vergleich" ]
        ]
            ++ List.map compareSpark config.selected


{-| Zeilen der Vergleichstabelle: Beschriftung und Formatierung je Attribut.
-}
compareFields : List ( String, Daily -> String )
compareFields =
    [ ( "EE-Anteil", \day -> Data.formatFloat 1 day.meanRenewableShare ++ " %" )
    , ( "Solaranteil", \day -> Data.formatFloat 1 day.solarShare ++ " %" )
    , ( "Windanteil", \day -> Data.formatFloat 1 day.windShare ++ " %" )
    , ( "Ø Last", \day -> Data.formatFloat 1 day.meanLoadGw ++ " GW" )
    , ( "Nettohandel", \day -> Data.formatFloat 1 day.meanNetImportGw ++ " GW" )
    , ( "Ø Preis"
      , \day ->
            day.meanPriceEurMwh
                |> Maybe.map (\price -> Data.formatFloat 1 price ++ " €/MWh")
                |> Maybe.withDefault "–"
      )
    , ( "Neg. Std.", \day -> String.fromInt day.negativePriceHours )
    ]


compareRow : List Daily -> ( String, Daily -> String ) -> Html msg
compareRow days ( label, format ) =
    tr []
        (td [] [ text label ]
            :: List.map (\day -> td [] [ text (format day) ]) days
        )


compareSpark : ( Daily, List Hourly ) -> Html msg
compareSpark ( day, hourly ) =
    div [ class "compare-spark" ]
        [ span [ class "detail-label" ]
            [ text (shortDate day.date ++ " · EE " ++ Data.formatFloat 0 day.meanRenewableShare ++ " %") ]
        , sparkSmall hourly
        ]


{-| "2024-02-04" wird zu "04.02." -- in Tabellenkoepfen zaehlt jede Stelle.
-}
shortDate : String -> String
shortDate date =
    case String.split "-" date of
        [ _, month, day ] ->
            day ++ "." ++ month ++ "."

        _ ->
            date


detail : String -> String -> Html msg
detail label value =
    div [ class "detail-item" ]
        [ span [ class "detail-label" ] [ text label ]
        , span [ class "detail-value" ] [ text value ]
        ]


{-| Stundenprofil eines Tages. Die Sparkline zeigt vor allem die *Form* des
Tages. Damit die Balken trotzdem einzuordnen sind, liegen Bezugslinien darueber;
die 100er-Linie ist die inhaltlich wichtige, denn oberhalb davon uebersteigt die
erneuerbare Einspeisung die Last.

`divisor` ist der Massstab in Prozent je Pixel und muss zur Hoehe der jeweiligen
CSS-Klasse passen, damit Linien und Balken dieselbe Skala benutzen.

-}
sparkline : List Hourly -> Html msg
sparkline points =
    sparkChart "" 1.4 True points


{-| Kompakte Fassung fuer die Gegenueberstellung mehrerer Tage: flacher, und
ohne die 50er-Linie, die auf so wenig Hoehe nur Unruhe stiftet.
-}
sparkSmall : List Hourly -> Html msg
sparkSmall points =
    sparkChart "small" 2.5 False points


sparkChart : String -> Float -> Bool -> List Hourly -> Html msg
sparkChart variant divisor withMinor points =
    let
        minorLine =
            if withMinor then
                [ gridLine divisor False 50 ]

            else
                []
    in
    div []
        [ div [ classList [ ( "spark", True ), ( variant, variant /= "" ) ] ]
            (gridLine divisor True 100 :: (minorLine ++ List.map (hourBar divisor) points))
        , div [ class "spark-hours" ] (List.map hourTick (List.range 0 23))
        ]


{-| Waagerechte Bezugslinie, auf derselben Skala wie die Balken.
-}
gridLine : Float -> Bool -> Int -> Html msg
gridLine divisor major percent =
    div
        [ classList [ ( "spark-grid", True ), ( "major", major ) ]
        , HtmlAttr.style "bottom" (String.fromFloat (toFloat percent / divisor) ++ "px")
        ]
        [ span [] [ text (String.fromInt percent ++ " %") ] ]


{-| Stundenmarke unter den Balken. Beschriftet wird nur alle sechs Stunden,
damit die Achse bei 24 Werten lesbar bleibt.
-}
hourTick : Int -> Html msg
hourTick hour =
    div [ class "spark-hour" ]
        [ text
            (if modBy 6 hour == 0 then
                String.fromInt hour

             else
                ""
            )
        ]


hourBar : Float -> Hourly -> Html msg
hourBar divisor point =
    div
        [ class "spark-bar"
        , HtmlAttr.style "height" (String.fromFloat (max 3 (point.renewableShare / divisor)) ++ "px")
        , HtmlAttr.title (String.fromInt point.hour ++ " Uhr: " ++ Data.formatFloat 1 point.renewableShare ++ " %")
        ]
        []


{-| Ein einziger Tooltip fuer die gesamte Anwendung, absolut positioniert an
der Mausposition. pointer-events sind aus, damit er die Interaktion mit der
darunterliegenden Markierung nicht unterbricht.
-}
tooltipView : Maybe Tooltip -> Html msg
tooltipView maybeTooltip =
    case maybeTooltip of
        Nothing ->
            text ""

        Just tooltip ->
            div
                [ class "tooltip"
                , HtmlAttr.style "left" (String.fromFloat (tooltip.x + 14) ++ "px")
                , HtmlAttr.style "top" (String.fromFloat (tooltip.y + 14) ++ "px")
                ]
                (div [ class "tooltip-title" ] [ text tooltip.title ]
                    :: List.map
                        (\( label, value ) ->
                            div [ class "tooltip-row" ]
                                [ span [] [ text label ], span [] [ text value ] ]
                        )
                        tooltip.rows
                )


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
.content { max-width: 1900px; margin: 0 auto; padding: 18px; }
.month-controls { display: flex; flex-wrap: wrap; gap: 6px; margin: 12px 0 16px; }
button { border: 1px solid #b9c3bd; background: #ffffff; color: #23313a; padding: 7px 10px; border-radius: 6px; cursor: pointer; }
button:hover, button.active { background: #214e57; border-color: #214e57; color: #ffffff; }
.metrics { display: grid; grid-template-columns: repeat(6, minmax(0, 1fr)); gap: 10px; margin-bottom: 16px; }
.metric { background: #ffffff; border: 1px solid #d9ded8; border-radius: 8px; padding: 10px 12px; min-width: 0; }
.metric-value { display: block; font-size: 20px; font-weight: 700; color: #173b42; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.metric-label { display: block; margin-top: 4px; font-size: 12px; color: #65727a; }
.grid { display: grid; grid-template-columns: minmax(0, 1fr) 280px; gap: 16px; align-items: start; }
/* Das Panel bleibt beim Scrollen sichtbar, damit es auch beim Arbeiten in
   der Heatmap weiter unten im Blick ist. */
.detail { position: sticky; top: 12px; }
.section, .detail { background: #ffffff; border: 1px solid #d9ded8; border-radius: 8px; padding: 14px; margin-bottom: 16px; overflow: hidden; }
.chart { display: block; width: 100%; height: auto; }
.chart-fixed { display: block; }
.pc-scroll { overflow-x: auto; }
.chart-label { font-size: 13px; font-weight: 700; fill: #26343c; }
.tick-label, .month-label, .dimension-label { font-size: 11px; fill: #64717a; }
.dimension-label { font-weight: 700; }
.grid-line { stroke: #e3e7e3; stroke-width: 1px; }
.axis-line { stroke: #9aa59f; stroke-width: 1px; }
.month-bar { fill: #76b6a2; opacity: 0.78; cursor: pointer; }
.month-bar:hover, .month-bar.active { fill: #214e57; opacity: 1; }
.legend-swatch { opacity: 0.9; }
.price-line { fill: none; stroke: #c04f3f; stroke-width: 2.5px; }
.price-dot { fill: #c04f3f; cursor: pointer; }
.heat-missing { fill: #dfe5e1; stroke: #ffffff; stroke-width: 0.25px; }
.heat-cell { stroke: none; cursor: pointer; }
.heat-cell:hover, .heat-cell.active { stroke: #111111; stroke-width: 1.2px; }
.pc-line { fill: none; stroke: #45646e; stroke-width: 1px; stroke-opacity: 0.16; cursor: pointer; }
.pc-line:hover, .pc-line.hovered { stroke: #c04f3f; stroke-width: 2px; stroke-opacity: 0.95; }
.pc-line.selected { stroke: #111111; stroke-width: 2.4px; stroke-opacity: 1; }
.pc-line.dimmed { stroke-opacity: 0.03; pointer-events: none; }
.pc-axis-hit { fill: #ffffff; fill-opacity: 0; cursor: ns-resize; }
.pc-overlay { fill: #ffffff; fill-opacity: 0; cursor: ns-resize; }
.brush-rect { fill: #214e57; fill-opacity: 0.16; stroke: #214e57; stroke-width: 1px; pointer-events: none; }
.pc-controls { display: flex; justify-content: space-between; align-items: center; gap: 10px; margin-bottom: 8px; font-size: 12px; color: #65727a; }
.pc-controls button { padding: 4px 8px; font-size: 12px; }
.tooltip { position: absolute; z-index: 10; background: #ffffff; border: 1px solid #c9d2cc; border-radius: 6px; padding: 8px 10px; pointer-events: none; box-shadow: 0 4px 14px rgba(0, 0, 0, 0.14); font-size: 12px; min-width: 180px; }
.tooltip-title { font-weight: 700; margin-bottom: 6px; color: #173b42; }
.tooltip-row { display: flex; justify-content: space-between; gap: 14px; color: #51606b; }
.tooltip-row span:last-child { font-weight: 600; color: #26343c; }
.detail-hint { font-size: 12px; font-weight: 700; color: #26343c; margin: 0 0 8px; }
.detail-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-bottom: 12px; }
.detail-item { border-bottom: 1px solid #edf0ed; padding-bottom: 6px; }
.detail-label { display: block; font-size: 11px; color: #66747c; }
.detail-value { display: block; font-size: 16px; font-weight: 700; color: #26343c; }
.spark { position: relative; height: 110px; display: flex; align-items: end; gap: 2px; padding-top: 10px; border-top: 1px solid #edf0ed; }
.spark-grid { position: absolute; left: 0; right: 0; border-top: 1px dashed #c9d4cd; pointer-events: none; }
.spark-grid.major { border-top: 1px solid #9db0a5; }
.spark-grid span { position: absolute; right: 0; top: -6px; font-size: 9px; line-height: 1; color: #66747c; background: #ffffff; padding: 0 2px; }
.spark-hours { display: flex; gap: 2px; margin-top: 3px; }
.spark.small { height: 66px; padding-top: 6px; }
.compare-scroll { overflow-x: auto; margin-bottom: 12px; }
.compare { width: 100%; border-collapse: collapse; font-size: 11px; }
.compare th { text-align: right; font-weight: 700; color: #26343c; padding: 3px 4px; border-bottom: 1px solid #d9ded8; white-space: nowrap; }
.compare th:first-child { text-align: left; }
.compare td { text-align: right; color: #26343c; padding: 3px 4px; border-bottom: 1px solid #edf0ed; white-space: nowrap; }
.compare td:first-child { text-align: left; color: #66747c; }
.compare-spark { margin-bottom: 10px; }
.spark-hour { flex: 1 1 0; min-width: 3px; font-size: 9px; line-height: 1; color: #66747c; text-align: center; white-space: nowrap; }
.spark-bar { flex: 1 1 0; min-width: 3px; background: #76b6a2; border-radius: 2px 2px 0 0; }
.chips { display: flex; flex-direction: column; gap: 6px; margin-bottom: 10px; }
.chip { display: flex; align-items: center; justify-content: space-between; gap: 8px; background: #eef3ee; border: 1px solid #d5ded6; border-radius: 6px; padding: 5px 8px; font-size: 12px; color: #26343c; }
.chip button { padding: 0 6px; border: none; background: transparent; color: #65727a; font-size: 15px; line-height: 1; }
.chip button:hover { color: #c04f3f; background: transparent; border: none; }
.clear-days { margin-bottom: 14px; font-size: 12px; padding: 5px 9px; }
.empty { color: #65727a; }
@media (max-width: 900px) {
  .grid, .metrics { grid-template-columns: 1fr; }
  .content { padding: 12px; }
}
"""
