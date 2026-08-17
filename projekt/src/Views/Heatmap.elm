module Views.Heatmap exposing (view)

{-| Visualisierung Zwei: pixelorientierte Stunden-Heatmap.

Jede Stunde des Jahres ist eine Zelle: x laeuft ueber die Tage, y ueber die
24 Tagesstunden, die Farbe kodiert den Erneuerbarenanteil. Diese Faltung der
eindimensionalen Zeit in ein zweidimensionales Raster ist der eigentliche
Zweck der Ansicht -- eine Struktur, die vom Wetter kommt, erscheint als
vertikaler Streifen, eine Struktur, die vom Sonnenstand kommt, als
horizontales Band. Beides ist ohne Rechnung an der Form unterscheidbar.

-}

import Color
import Data exposing (Hourly)
import Html exposing (Html)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..))


type alias Config msg =
    { hoveredDay : Maybe String
    , selectedDay : Maybe String
    , onHoverDay : String -> msg
    , onLeave : msg
    , onSelectDay : String -> msg
    }


view : Config msg -> List Hourly -> Html msg
view config hourly =
    let
        minDay =
            hourly |> List.map .dayOfYear |> List.minimum |> Maybe.withDefault 1

        maxDay =
            hourly |> List.map .dayOfYear |> List.maximum |> Maybe.withDefault 366

        dayCount =
            max 1 (maxDay - minDay + 1)

        -- Bei ungefiltertem Jahr eine Pixelspalte je Tag, bei Monatsfilterung
        -- breitere Kacheln. Die Semantik bleibt dieselbe.
        cellW =
            min 8 (840 / toFloat dayCount)

        cellH =
            10

        left =
            58

        top =
            34

        w =
            left + toFloat dayCount * cellW + 28

        h =
            top + 24 * cellH + 66
    in
    svg
        [ SvgAttr.viewBox 0 0 w h
        , SvgAttr.class [ "chart", "heatmap" ]
        ]
        [ text_ [ SvgAttr.class [ "chart-label" ], Px.x 14, Px.y 18 ] [ SvgCore.text "Stundenraster: EE-Anteil je Tag und Stunde" ]
        , g [] (hourLabels left top cellH)

        -- Zuerst ein vollstaendiges Raster grauer Zellen, darueber die
        -- Datenzellen. Wo Daten fehlen, bleibt Grau stehen -- ohne
        -- Sonderfallbehandlung und ohne dass die Tagesspalten zusammenruecken.
        , g [] (missingCells minDay dayCount left top cellW cellH)
        , g [] (List.map (cell config minDay left top cellW cellH) hourly)
        , legend left (top + 24 * cellH + 16)
        ]


cell : Config msg -> Int -> Float -> Float -> Float -> Float -> Hourly -> Svg msg
cell config minDay left top cellW cellH point =
    let
        x0 =
            left + toFloat (point.dayOfYear - minDay) * cellW

        y0 =
            top + toFloat point.hour * cellH

        isActive =
            config.selectedDay == Just point.date || config.hoveredDay == Just point.date

        className =
            if isActive then
                [ "heat-cell", "active" ]

            else
                [ "heat-cell" ]
    in
    rect
        [ SvgAttr.class className
        , SvgAttr.fill (Paint (renewableColor point.renewableShare))
        , Px.x x0
        , Px.y y0
        , Px.width (max 1.4 (cellW - 0.3))
        , Px.height (cellH - 0.5)
        , SvgEvents.onMouseOver (config.onHoverDay point.date)
        , SvgEvents.onMouseOut config.onLeave
        , SvgEvents.onClick (config.onSelectDay point.date)
        ]
        []


{-| Sequentielle Rampe von dunklem Blau nach hellem Gruen mit steigender
Helligkeit. Bewusst keine Regenbogenskala: Deren nicht-monotone Helligkeit
erzeugt kuenstliche Grenzen und verschleiert die Ordnung der Werte.

Die Begrenzung liegt bei 120 % und nicht bei 100 %, damit die Stunden mit
Ueberschuss sichtbar bleiben, ohne den Kontrast des Hauptbereichs
aufzubrauchen.
-}
renewableColor : Float -> Color.Color
renewableColor value =
    let
        t =
            clamp 0 1 (value / 120)

        r =
            0.12 + (1 - t) * 0.72

        g =
            0.25 + t * 0.47

        b =
            0.42 + (1 - abs (t - 0.5) * 2) * 0.2
    in
    Color.rgb r g b


{-| Hintergrundraster fuer den gesamten sichtbaren Zeitraum. Die Luecken der
Quelle werden bewusst nicht interpoliert: Interpolierte Werte waeren in einer
pixelorientierten Darstellung nicht von Messwerten zu unterscheiden.
-}
missingCells : Int -> Int -> Float -> Float -> Float -> Float -> List (Svg msg)
missingCells minDay dayCount left top cellW cellH =
    List.range minDay (minDay + dayCount - 1)
        |> List.concatMap
            (\dayOfYear ->
                List.map
                    (\hour ->
                        rect
                            [ SvgAttr.class [ "heat-missing" ]
                            , Px.x (left + toFloat (dayOfYear - minDay) * cellW)
                            , Px.y (top + toFloat hour * cellH)
                            , Px.width (max 1.4 (cellW - 0.3))
                            , Px.height (cellH - 0.5)
                            ]
                            []
                    )
                    (List.range 0 23)
            )


hourLabels : Float -> Float -> Float -> List (Svg msg)
hourLabels left top cellH =
    [ 0, 6, 12, 18, 23 ]
        |> List.map
            (\hour ->
                text_
                    [ SvgAttr.class [ "tick-label" ]
                    , SvgAttr.textAnchor AnchorEnd
                    , Px.x (left - 8)
                    , Px.y (top + toFloat hour * cellH + 8)
                    ]
                    [ SvgCore.text (String.fromInt hour ++ "h") ]
            )


legend : Float -> Float -> Svg msg
legend x0 y0 =
    let
        stops =
            [ 0, 30, 60, 90, 120 ]

        swatch index value =
            g []
                [ rect
                    [ SvgAttr.fill (Paint (renewableColor (toFloat value)))
                    , Px.x (x0 + 70 + toFloat index * 36)
                    , Px.y y0
                    , Px.width 34
                    , Px.height 10
                    ]
                    []
                , text_
                    [ SvgAttr.class [ "tick-label" ]
                    , SvgAttr.textAnchor AnchorMiddle
                    , Px.x (x0 + 70 + toFloat index * 36 + 17)
                    , Px.y (y0 + 24)
                    ]
                    [ SvgCore.text
                        (if value == 120 then
                            "≥120 %"

                         else
                            String.fromInt value ++ " %"
                        )
                    ]
                ]

        missingX =
            x0 + 70 + 5 * 36 + 24
    in
    g []
        (text_ [ SvgAttr.class [ "tick-label" ], Px.x x0, Px.y (y0 + 9) ] [ SvgCore.text "EE-Anteil:" ]
            :: List.indexedMap swatch stops
            ++ [ rect [ SvgAttr.class [ "heat-missing" ], Px.x missingX, Px.y y0, Px.width 14, Px.height 10 ] []
               , text_ [ SvgAttr.class [ "tick-label" ], Px.x (missingX + 20), Px.y (y0 + 9) ] [ SvgCore.text "keine Daten (Lücke in der Quelle)" ]
               ]
        )
