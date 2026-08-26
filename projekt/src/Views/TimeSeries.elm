module Views.TimeSeries exposing (view)

{-| Visualisierung Eins: verdichtete Zeitreihe ueber die zwoelf Monate.

Die Ansicht ist Einstieg und Navigationsebene, nicht die Analyseebene: Der
mittlere Erneuerbarenanteil liegt als Balkenhoehe auf dem genauesten Kanal
(Position auf gemeinsamer Basislinie), der Preis als Linie auf einer zweiten
Achse. Die doppelte Achse ist eine bewusste Schwaeche -- der Abstand zwischen
Balken und Linie hat keine Bedeutung -- und deshalb sind beide Achsen
ausdruecklich beschriftet.

Beide Achsen bekommen runde Grenzen und gleich viele Intervalle: Die Balken
stehen auf einer Nulllinie und reichen bis zum naechsten runden Wert oberhalb
des hoechsten Balkens; die Preisachse laesst oben und unten Luft, damit kein
Punkt auf einer Achse klebt. Weil beide Achsen dieselbe Zahl an Intervallen
haben, liegen die Preisbeschriftungen auf den Gitterlinien der Balkenachse.

Ein Balken ist zugleich Schaltflaeche fuer den Monatsfilter. Das ist der
Grund, warum hier Mittelwertbalken und kein Boxplot steht: Die grosse,
eindeutige Klickflaeche laesst sich nicht ersetzen.

-}

import Data exposing (Daily, Tooltip)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Set exposing (Set)
import TypedSvg exposing (g, line, path, rect, svg, text_)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types exposing (AnchorAlignment(..))


type alias Config msg =
    { selectedMonths : Set Int
    , onToggleMonth : Int -> msg
    , onShowTooltip : Tooltip -> msg
    , onMove : Float -> Float -> msg
    , onLeave : msg
    }


type alias MonthSummary =
    { month : Int
    , renewable : Float
    , price : Float
    , wind : Float
    , solar : Float
    , negativeHours : Int
    }


view : Config msg -> List Daily -> Html msg
view config daily =
    let
        summaries =
            monthly daily

        w =
            920

        h =
            250

        left =
            54

        right =
            60

        top =
            24

        bottom =
            44

        chartW =
            w - left - right

        chartH =
            h - top - bottom

        xStep =
            chartW / 12

        renewableMax =
            summaries
                |> List.map .renewable
                |> List.maximum
                |> Maybe.withDefault 100
                |> niceCeiling

        renewableTicks =
            List.map (\i -> renewableMax * toFloat i / toFloat tickCount) (List.range 0 tickCount)

        yRenewable value =
            top + chartH - clamp 0 renewableMax value / renewableMax * chartH

        priceValues =
            List.map .price summaries

        priceScale =
            niceScale
                (List.minimum priceValues |> Maybe.withDefault 0)
                (List.maximum priceValues |> Maybe.withDefault 100)

        priceTicks =
            List.map (\i -> priceScale.low + priceScale.step * toFloat i)
                (List.range 0 (round ((priceScale.high - priceScale.low) / priceScale.step)))

        yPrice value =
            top + chartH - (value - priceScale.low) / (priceScale.high - priceScale.low) * chartH

        pricePath =
            summaries
                |> List.map (\m -> ( left + (toFloat m.month - 0.5) * xStep, yPrice m.price ))
                |> linePath
    in
    svg
        [ SvgAttr.viewBox 0 0 w h
        , SvgAttr.class [ "chart", "time-series" ]
        ]
        [ g [] (grid left top chartW chartH renewableTicks yRenewable)
        , g [] (priceAxis left top chartW chartH priceTicks yPrice)
        , g [] (List.map (monthBar config left top chartH xStep yRenewable) summaries)
        , path [ SvgAttr.class [ "price-line" ], SvgAttr.d pricePath ] []
        , g [] (List.map (priceDot config left xStep yPrice) summaries)
        , axisLabel 14 18 "Monatliche Tagesprofile 2024"
        , axisLabel 14 (h - 16) "Klick auf einen Balken filtert den Monat (Mehrfachauswahl möglich)."
        , legend (left + chartW - 400) 8
        ]


legend : Float -> Float -> Svg msg
legend x0 y0 =
    g []
        [ rect [ SvgAttr.class [ "month-bar", "legend-swatch" ], Px.x x0, Px.y y0, Px.width 14, Px.height 10 ] []
        , text_ [ SvgAttr.class [ "tick-label" ], Px.x (x0 + 20), Px.y (y0 + 9) ] [ SvgCore.text "Ø EE-Anteil in % (Balken)" ]
        , line [ SvgAttr.class [ "price-line" ], Px.x1 (x0 + 175), Px.y1 (y0 + 5), Px.x2 (x0 + 203), Px.y2 (y0 + 5) ] []
        , rect [ SvgAttr.class [ "price-dot" ], Px.x (x0 + 185), Px.y (y0 + 1), Px.width 8, Px.height 8 ] []
        , text_ [ SvgAttr.class [ "tick-label" ], Px.x (x0 + 209), Px.y (y0 + 9) ] [ SvgCore.text "Ø Preis in €/MWh (Linie)" ]
        ]


priceAxis : Float -> Float -> Float -> Float -> List Float -> (Float -> Float) -> List (Svg msg)
priceAxis left top chartW chartH ticks yPrice =
    let
        axisX =
            left + chartW

        tick value =
            let
                y0 =
                    yPrice value
            in
            g []
                [ line [ SvgAttr.class [ "axis-line" ], Px.x1 axisX, Px.y1 y0, Px.x2 (axisX + 4), Px.y2 y0 ] []
                , text_
                    [ SvgAttr.class [ "tick-label" ]
                    , Px.x (axisX + 8)
                    , Px.y (y0 + 4)
                    ]
                    [ SvgCore.text (Data.formatFloat 0 value) ]
                ]
    in
    [ line [ SvgAttr.class [ "axis-line" ], Px.x1 axisX, Px.y1 top, Px.x2 axisX, Px.y2 (top + chartH) ] []
    , text_ [ SvgAttr.class [ "tick-label" ], Px.x (axisX + 8), Px.y (top - 8) ] [ SvgCore.text "€/MWh" ]
    ]
        ++ List.map tick ticks


monthBar : Config msg -> Float -> Float -> Float -> Float -> (Float -> Float) -> MonthSummary -> Svg msg
monthBar config left top chartH xStep yRenewable summary =
    let
        x0 =
            left + (toFloat summary.month - 1) * xStep + 8

        barW =
            xStep - 16

        y0 =
            yRenewable summary.renewable

        className =
            if Set.member summary.month config.selectedMonths then
                [ "month-bar", "active" ]

            else
                [ "month-bar" ]
    in
    g
        [ SvgEvents.onClick (config.onToggleMonth summary.month) ]
        [ rect
            [ SvgAttr.class className
            , Px.x x0
            , Px.y y0
            , Px.width barW
            , Px.height (top + chartH - y0)
            , onTooltipOver config summary
            , onTooltipMove config
            , SvgEvents.onMouseOut config.onLeave
            ]
            []
        , text_
            [ SvgAttr.class [ "month-label" ]
            , SvgAttr.textAnchor AnchorMiddle
            , Px.x (x0 + barW / 2)
            , Px.y (top + chartH + 18)
            ]
            [ SvgCore.text (Data.monthName summary.month) ]
        ]


priceDot : Config msg -> Float -> Float -> (Float -> Float) -> MonthSummary -> Svg msg
priceDot config left xStep yPrice summary =
    let
        x0 =
            left + (toFloat summary.month - 0.5) * xStep
    in
    rect
        [ SvgAttr.class [ "price-dot" ]
        , Px.x (x0 - 4)
        , Px.y (yPrice summary.price - 4)
        , Px.width 8
        , Px.height 8
        , SvgEvents.onClick (config.onToggleMonth summary.month)
        , onTooltipOver config summary
        , onTooltipMove config
        , SvgEvents.onMouseOut config.onLeave
        ]
        []


onTooltipOver : Config msg -> MonthSummary -> SvgCore.Attribute msg
onTooltipOver config summary =
    Html.Events.on "mouseover"
        (positionDecoder (\x y -> config.onShowTooltip (monthTooltip summary x y)))


onTooltipMove : Config msg -> SvgCore.Attribute msg
onTooltipMove config =
    Html.Events.on "mousemove" (positionDecoder config.onMove)


positionDecoder : (Float -> Float -> msg) -> Decode.Decoder msg
positionDecoder toMsg =
    Decode.map2 toMsg
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


monthTooltip : MonthSummary -> Float -> Float -> Tooltip
monthTooltip summary x y =
    { x = x
    , y = y
    , title = Data.monthFullName summary.month ++ " 2024"
    , rows =
        [ ( "Ø EE-Anteil", Data.formatFloat 1 summary.renewable ++ " %" )
        , ( "Ø Preis", Data.formatFloat 1 summary.price ++ " €/MWh" )
        , ( "Ø Solaranteil", Data.formatFloat 1 summary.solar ++ " %" )
        , ( "Ø Windanteil", Data.formatFloat 1 summary.wind ++ " %" )
        , ( "Neg. Preisstunden", String.fromInt summary.negativeHours )
        ]
    }


{-| Tagesprofile zu zwoelf Monatswerten falten. Monate ohne Daten werden
ausgelassen und nicht als Null gezeichnet.
-}
monthly : List Daily -> List MonthSummary
monthly daily =
    List.range 1 12
        |> List.filterMap
            (\month ->
                let
                    days =
                        List.filter (\day -> day.month == month) daily

                    priceValues =
                        List.filterMap .meanPriceEurMwh days
                in
                if List.isEmpty days then
                    Nothing

                else
                    Just
                        { month = month
                        , renewable = mean (List.map .meanRenewableShare days)
                        , price = mean priceValues
                        , wind = mean (List.map .windShare days)
                        , solar = mean (List.map .solarShare days)
                        , negativeHours = List.sum (List.map .negativePriceHours days)
                        }
            )


grid : Float -> Float -> Float -> Float -> List Float -> (Float -> Float) -> List (Svg msg)
grid left top chartW chartH ticks yRenewable =
    [ line [ SvgAttr.class [ "axis-line" ], Px.x1 left, Px.y1 top, Px.x2 left, Px.y2 (top + chartH) ] []
    , line [ SvgAttr.class [ "axis-line" ], Px.x1 left, Px.y1 (top + chartH), Px.x2 (left + chartW), Px.y2 (top + chartH) ] []
    ]
        ++ List.map
            (\value ->
                let
                    y0 =
                        yRenewable value
                in
                g []
                    [ line [ SvgAttr.class [ "grid-line" ], Px.x1 left, Px.y1 y0, Px.x2 (left + chartW), Px.y2 y0 ] []
                    , text_ [ SvgAttr.class [ "tick-label" ], Px.x 12, Px.y (y0 + 4) ] [ SvgCore.text (String.fromInt (round value) ++ "%") ]
                    ]
            )
            ticks


{-| Anzahl der Intervalle auf beiden Achsen. Weil Balken- und Preisachse
dieselbe Zahl an Intervallen benutzen, liegen die Beschriftungen der rechten
Achse genau auf den Gitterlinien der linken.
-}
tickCount : Int
tickCount =
    4


{-| Naechster runder Wert oberhalb des Maximums, damit die Balkenachse bei
null beginnt und nicht deutlich hoeher endet als der hoechste Balken.
-}
niceCeiling : Float -> Float
niceCeiling value =
    let
        step =
            niceStep (max 1 value / toFloat tickCount)
    in
    toFloat (ceiling (value / step)) * step


{-| Achsenbereich fuer die Preislinie: runde Grenzen mit Luft nach oben und
unten, damit kein Punkt auf einer Achse klebt und die Beschriftungen runde
Zahlen zeigen. Die Linie kodiert Position, nicht Laenge, deshalb darf dieser
Bereich abgeschnitten sein -- er ist aber ausdruecklich beziffert.
-}
niceScale : Float -> Float -> { low : Float, high : Float, step : Float }
niceScale lowest highest =
    let
        span =
            max 1 (highest - lowest)

        step =
            niceStep (span / toFloat (tickCount - 1))

        slack =
            step * toFloat tickCount - span

        low =
            toFloat (floor ((lowest - slack / 2) / step)) * step

        high =
            max (low + step * toFloat tickCount)
                (toFloat (ceiling (highest / step)) * step)
    in
    { low = low, high = high, step = step }


{-| Rundet eine Schrittweite auf 1, 2, 5 oder 10 mal eine Zehnerpotenz.
-}
niceStep : Float -> Float
niceStep raw =
    let
        magnitude =
            10 ^ toFloat (floor (logBase 10 (max 1.0e-6 raw)))

        normalized =
            raw / magnitude

        factor =
            if normalized <= 1 then
                1

            else if normalized <= 2 then
                2

            else if normalized <= 5 then
                5

            else
                10
    in
    factor * magnitude


linePath : List ( Float, Float ) -> String
linePath points =
    case points of
        [] ->
            ""

        ( x0, y0 ) :: rest ->
            "M "
                ++ String.fromFloat x0
                ++ " "
                ++ String.fromFloat y0
                ++ " "
                ++ (rest
                        |> List.map (\( x, y ) -> "L " ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                        |> String.join " "
                   )


axisLabel : Float -> Float -> String -> Svg msg
axisLabel x y label =
    text_ [ SvgAttr.class [ "chart-label" ], Px.x x, Px.y y ] [ SvgCore.text label ]


mean : List Float -> Float
mean values =
    case values of
        [] ->
            0

        _ ->
            List.sum values / toFloat (List.length values)
