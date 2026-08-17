module Views.ParallelCoordinates exposing (view)

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
import Html exposing (Html)
import TypedSvg exposing (g, line, path, svg, text_)
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types exposing (AnchorAlignment(..))


type alias Config msg =
    { hoveredDay : Maybe String
    , selectedDay : Maybe String
    , onHoverDay : String -> msg
    , onLeave : msg
    , onSelectDay : String -> msg
    }


type alias Dimension =
    { label : String
    , read : Daily -> Float
    , minValue : Float
    , maxValue : Float
    }


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
    in
    svg
        [ SvgAttr.viewBox 0 0 chartWidth chartHeight
        , SvgAttr.class [ "chart", "parallel" ]
        ]
        [ text_ [ SvgAttr.class [ "chart-label" ], Px.x 14, Px.y 20 ] [ SvgCore.text "Parallele Koordinaten: Tagesprofile" ]
        , g [] (List.map (polyline config dims) daily)
        , g [] (List.indexedMap (axis (List.length dims)) dims)
        ]


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


polyline : Config msg -> List Dimension -> Daily -> Svg msg
polyline config dims day =
    let
        className =
            if config.selectedDay == Just day.date then
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
