module Views.ParallelCoordinates exposing (Drag, view)

{-| Visualisierung Drei: parallele Koordinaten der Tagesprofile.

Jeder Tag ist eine Polylinie ueber sieben Achsen. Das ist die einzige der
drei Ansichten, in der ein Tag ein einziges, als Ganzes wahrnehmbares Objekt
ist -- Voraussetzung dafuer, Aehnlichkeit zweier Tage ueberhaupt beurteilen zu
koennen.

Der entscheidende Vorteil gegenueber einer Projektion (PCA, MDS) ist, dass
die Achsen ihre Originaleinheiten behalten: Ein abgelesener Wert ist eine
physikalische Groesse und keine Komponente.

Die Achsenreihenfolge ist nicht beliebig, denn nur benachbarte Achsen zeigen
ihren Zusammenhang als Buendelung oder Kreuzung. Sie folgt der Kausalkette
des Anwendungsgebiets: Last, Solar, Wind, EE, Handel, Preis, negative
Stunden.

-}

import Data exposing (Daily)
import Dict exposing (Dict)
import Html exposing (Html, button, div, span, text)
import Html.Attributes as HtmlAttr
import Html.Events
import Json.Decode as Decode
import TypedSvg exposing (g, line, path, rect, svg, text_)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types exposing (AnchorAlignment(..))


{-| Ein laufender Brush-Vorgang: Achsenindex plus Start- und aktuelle
Position als Wertanteil 0..1 (0 = Achsenminimum, 1 = Achsenmaximum).
-}
type alias Drag =
    { axis : Int
    , start : Float
    , current : Float
    }


type alias Config msg =
    { hoveredDay : Maybe String
    , selectedDay : Maybe String
    , brushes : Dict Int ( Float, Float )
    , dragging : Maybe Drag
    , onHoverDay : String -> msg
    , onLeave : msg
    , onSelectDay : String -> msg
    , onBrushStart : Int -> Float -> msg
    , onBrushMove : Float -> msg
    , onBrushEnd : msg
    , onClearBrushes : msg
    }


type alias Dimension =
    { label : String
    , read : Daily -> Float
    , minValue : Float
    , maxValue : Float
    }


-- Feste Pixelgroesse statt responsiver Skalierung: Nur so entspricht das
-- offsetY eines Mausereignisses direkt der SVG-y-Koordinate. Der Alternativweg
-- ueber getBoundingClientRect braeuchte Ports oder Browser.Dom-Tasks.


chartWidth : Float
chartWidth =
    780


chartHeight : Float
chartHeight =
    330


marginLeft : Float
marginLeft =
    70


marginRight : Float
marginRight =
    40


marginTop : Float
marginTop =
    42


marginBottom : Float
marginBottom =
    58


innerW : Float
innerW =
    chartWidth - marginLeft - marginRight


innerH : Float
innerH =
    chartHeight - marginTop - marginBottom


view : Config msg -> List Daily -> Html msg
view config daily =
    let
        dims =
            dimensions daily

        effective =
            effectiveBrushes config

        passes =
            passesBrushes dims effective

        visibleCount =
            List.length (List.filter passes daily)
    in
    div []
        [ controls config (List.length daily) visibleCount
        , div [ HtmlAttr.class "pc-scroll" ]
            [ svg
                [ SvgAttr.viewBox 0 0 chartWidth chartHeight
                , Px.width chartWidth
                , Px.height chartHeight
                , SvgAttr.class [ "chart-fixed", "parallel" ]
                ]
                [ text_ [ SvgAttr.class [ "chart-label" ], Px.x 14, Px.y 20 ] [ SvgCore.text "Parallele Koordinaten: Tagesprofile" ]
                , g [] (List.map (polyline config dims passes) daily)
                , g [] (List.indexedMap (axis (List.length dims)) dims)
                , g [] (brushRects effective (List.length dims))
                , g [] (List.indexedMap (\index _ -> axisHit config (List.length dims) index) dims)
                ]
            ]
        ]


controls : Config msg -> Int -> Int -> Html msg
controls config total visible =
    div [ HtmlAttr.class "pc-controls" ]
        [ span []
            [ text
                ("Bereich auf einer Achse aufziehen = filtern (kombinierbar) · Klick auf Achse = Filter löschen · "
                    ++ String.fromInt visible
                    ++ " von "
                    ++ String.fromInt total
                    ++ " Tagen sichtbar"
                )
            ]
        , if Dict.isEmpty config.brushes && config.dragging == Nothing then
            text ""

          else
            button [ Html.Events.onClick config.onClearBrushes ] [ text "Achsenfilter zurücksetzen" ]
        ]


{-| Waehrend des Ziehens wirkt der laufende Brush bereits wie ein gesetzter,
damit live gefiltert wird, ohne den bestaetigten Zustand zu ueberschreiben.
-}
effectiveBrushes : Config msg -> Dict Int ( Float, Float )
effectiveBrushes config =
    case config.dragging of
        Just drag ->
            Dict.insert drag.axis
                ( min drag.start drag.current, max drag.start drag.current )
                config.brushes

        Nothing ->
            config.brushes


{-| Brushes auf mehreren Achsen werden UND-verknuepft; List.all drueckt das
direkt aus.
-}
passesBrushes : List Dimension -> Dict Int ( Float, Float ) -> Daily -> Bool
passesBrushes dims brushes day =
    Dict.toList brushes
        |> List.all
            (\( index, ( lo, hi ) ) ->
                case List.head (List.drop index dims) of
                    Nothing ->
                        True

                    Just dim ->
                        let
                            span =
                                max 0.0001 (dim.maxValue - dim.minValue)

                            fraction =
                                (dim.read day - dim.minValue) / span
                        in
                        lo - 0.001 <= fraction && fraction <= hi + 0.001
            )


brushRects : Dict Int ( Float, Float ) -> Int -> List (Svg msg)
brushRects brushes count =
    Dict.toList brushes
        |> List.map
            (\( index, ( lo, hi ) ) ->
                rect
                    [ SvgAttr.class [ "brush-rect" ]
                    , Px.x (axisX count index - 6)
                    , Px.y (marginTop + (1 - hi) * innerH)
                    , Px.width 12
                    , Px.height ((hi - lo) * innerH)
                    ]
                    []
            )


{-| Unsichtbare Trefferflaeche je Achse, auf der der Brush aufgezogen wird. -}
axisHit : Config msg -> Int -> Int -> Svg msg
axisHit config count index =
    rect
        [ SvgAttr.class [ "pc-axis-hit" ]
        , Px.x (axisX count index - 9)
        , Px.y marginTop
        , Px.width 18
        , Px.height innerH
        , Html.Events.preventDefaultOn "mousedown"
            (Decode.field "offsetY" Decode.float
                |> Decode.map (\offsetY -> ( config.onBrushStart index (yToFraction offsetY), True ))
            )
        , Html.Events.on "mousemove"
            (Decode.field "offsetY" Decode.float
                |> Decode.map (yToFraction >> config.onBrushMove)
            )
        , Html.Events.on "mouseup" (Decode.succeed config.onBrushEnd)
        ]
        []


yToFraction : Float -> Float
yToFraction offsetY =
    1 - clamp 0 1 ((offsetY - marginTop) / innerH)


{-| Die Achsen sind datengetrieben: Beschriftung, Zugriffsfunktion und der im
sichtbaren Datenbestand beobachtete Wertebereich. Eine achte Dimension waere
eine Zeile.
-}
dimensions : List Daily -> List Dimension
dimensions daily =
    [ makeDimension "Last (GW)" .meanLoadGw daily
    , makeDimension "Solar (%)" .solarShare daily
    , makeDimension "Wind (%)" .windShare daily
    , makeDimension "EE (%)" .meanRenewableShare daily
    , makeDimension "Handel (GW)" .meanNetImportGw daily
    , makeDimension "Preis (€/MWh)" (\day -> Maybe.withDefault 0 day.meanPriceEurMwh) daily
    , makeDimension "Neg. Std." (\day -> toFloat day.negativePriceHours) daily
    ]


makeDimension : String -> (Daily -> Float) -> List Daily -> Dimension
makeDimension label read daily =
    let
        values =
            List.map read daily
    in
    { label = label
    , read = read
    , minValue = List.minimum values |> Maybe.withDefault 0
    , maxValue = List.maximum values |> Maybe.withDefault 1
    }


polyline : Config msg -> List Dimension -> (Daily -> Bool) -> Daily -> Svg msg
polyline config dims passes day =
    let
        className =
            if not (passes day) then
                [ "pc-line", "dimmed" ]

            else if config.selectedDay == Just day.date then
                [ "pc-line", "selected" ]

            else if config.hoveredDay == Just day.date then
                [ "pc-line", "hovered" ]

            else
                [ "pc-line" ]
    in
    path
        [ SvgAttr.class className
        , SvgAttr.d (linePath dims day)
        , SvgEvents.onMouseOver (config.onHoverDay day.date)
        , SvgEvents.onMouseOut config.onLeave
        , SvgEvents.onClick (config.onSelectDay day.date)
        ]
        []


axis : Int -> Int -> Dimension -> Svg msg
axis count index dim =
    let
        x0 =
            axisX count index
    in
    g []
        [ line [ SvgAttr.class [ "axis-line" ], Px.x1 x0, Px.y1 marginTop, Px.x2 x0, Px.y2 (marginTop + innerH) ] []
        , text_
            [ SvgAttr.class [ "dimension-label" ]
            , SvgAttr.textAnchor AnchorMiddle
            , Px.x x0
            , Px.y (marginTop + innerH + 24)
            ]
            [ SvgCore.text dim.label ]
        , text_ [ SvgAttr.class [ "tick-label" ], SvgAttr.textAnchor AnchorMiddle, Px.x x0, Px.y (marginTop - 10) ] [ SvgCore.text (Data.formatFloat 1 dim.maxValue) ]
        , text_ [ SvgAttr.class [ "tick-label" ], SvgAttr.textAnchor AnchorMiddle, Px.x x0, Px.y (marginTop + innerH + 12) ] [ SvgCore.text (Data.formatFloat 1 dim.minValue) ]
        ]


linePath : List Dimension -> Daily -> String
linePath dims day =
    dims
        |> List.indexedMap
            (\index dim ->
                ( axisX (List.length dims) index, scaleY dim (dim.read day) )
            )
        |> pointsToPath


axisX : Int -> Int -> Float
axisX count index =
    if count <= 1 then
        marginLeft

    else
        marginLeft + toFloat index / toFloat (count - 1) * innerW


scaleY : Dimension -> Float -> Float
scaleY dim value =
    let
        span =
            max 0.0001 (dim.maxValue - dim.minValue)
    in
    marginTop + innerH - (value - dim.minValue) / span * innerH


pointsToPath : List ( Float, Float ) -> String
pointsToPath points =
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
