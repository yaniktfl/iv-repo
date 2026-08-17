module Data exposing
    ( Daily
    , Dataset
    , Hourly
    , Meta
    , datasetDecoder
    , filterDaily
    , filterHourly
    , findDaily
    , findHourlyForDate
    , formatFloat
    , monthFullName
    , monthName
    )

{-| Datentypen und Decoder fuer den vorverarbeiteten EnergyCharts-Datensatz.

Der Datensatz kommt als Flag aus `index.html` und liegt in zwei
Aggregationsstufen vor: `Hourly` fuer die dichte Stundendarstellung und
`Daily` fuer den mehrdimensionalen Tagesvergleich. Die Tagesaggregate werden
bereits im Vorverarbeitungsskript berechnet, damit keine Ansicht bei jeder
Interaktion gruppieren muss.

-}

import Json.Decode as Decode exposing (Decoder)


type alias Dataset =
    { meta : Meta
    , hourly : List Hourly
    , daily : List Daily
    }


type alias Meta =
    { country : String
    , countryId : String
    , priceMarket : String
    , year : Int
    , source : String
    , generatedAt : String
    , notes : List String
    }


{-| Eine Stunde. `renewableShare` kann ueber 100 % liegen, wenn die erneuerbare
Erzeugung die rekonstruierte Last uebersteigt; solche Werte werden bewusst
nicht gekappt.
-}
type alias Hourly =
    { timestamp : String
    , date : String
    , month : Int
    , dayOfYear : Int
    , hour : Int
    , loadGw : Float
    , solarGw : Float
    , windOnshoreGw : Float
    , windOffshoreGw : Float
    , fossilGw : Float
    , renewableGw : Float
    , renewableShare : Float
    , netImportGw : Float
    , priceEurMwh : Maybe Float
    }


{-| Ein Tagesprofil. Solar- und Windanteil beziehen sich auf die mittlere Last
des Tages und sind damit mit `meanRenewableShare` vergleichbar.
-}
type alias Daily =
    { date : String
    , month : Int
    , dayOfYear : Int
    , meanLoadGw : Float
    , maxLoadGw : Float
    , meanSolarGw : Float
    , meanWindGw : Float
    , meanRenewableShare : Float
    , solarShare : Float
    , windShare : Float
    , meanNetImportGw : Float
    , meanPriceEurMwh : Maybe Float
    , negativePriceHours : Int
    }



-- DECODER


datasetDecoder : Decoder Dataset
datasetDecoder =
    Decode.map3 Dataset
        (Decode.field "meta" metaDecoder)
        (Decode.field "hourly" (Decode.list hourlyDecoder))
        (Decode.field "daily" (Decode.list dailyDecoder))


metaDecoder : Decoder Meta
metaDecoder =
    Decode.map7 Meta
        (Decode.field "country" Decode.string)
        (Decode.field "countryId" Decode.string)
        (Decode.field "priceMarket" Decode.string)
        (Decode.field "year" Decode.int)
        (Decode.field "source" Decode.string)
        (Decode.field "generatedAt" Decode.string)
        (Decode.field "notes" (Decode.list Decode.string))


hourlyDecoder : Decoder Hourly
hourlyDecoder =
    Decode.map8
        (\timestamp date month dayOfYear hour loadGw solarGw windOnshoreGw ->
            \windOffshoreGw fossilGw renewableGw renewableShare netImportGw priceEurMwh ->
                { timestamp = timestamp
                , date = date
                , month = month
                , dayOfYear = dayOfYear
                , hour = hour
                , loadGw = loadGw
                , solarGw = solarGw
                , windOnshoreGw = windOnshoreGw
                , windOffshoreGw = windOffshoreGw
                , fossilGw = fossilGw
                , renewableGw = renewableGw
                , renewableShare = renewableShare
                , netImportGw = netImportGw
                , priceEurMwh = priceEurMwh
                }
        )
        (Decode.field "timestamp" Decode.string)
        (Decode.field "date" Decode.string)
        (Decode.field "month" Decode.int)
        (Decode.field "dayOfYear" Decode.int)
        (Decode.field "hour" Decode.int)
        (Decode.field "loadGw" Decode.float)
        (Decode.field "solarGw" Decode.float)
        (Decode.field "windOnshoreGw" Decode.float)
        |> andMap (Decode.field "windOffshoreGw" Decode.float)
        |> andMap (Decode.field "fossilGw" Decode.float)
        |> andMap (Decode.field "renewableGw" Decode.float)
        |> andMap (Decode.field "renewableShare" Decode.float)
        |> andMap (Decode.field "netImportGw" Decode.float)
        |> andMap (Decode.field "priceEurMwh" (Decode.nullable Decode.float))


dailyDecoder : Decoder Daily
dailyDecoder =
    Decode.map8
        (\date month dayOfYear meanLoadGw maxLoadGw meanSolarGw meanWindGw meanRenewableShare ->
            \solarShare windShare meanNetImportGw meanPriceEurMwh negativePriceHours ->
                { date = date
                , month = month
                , dayOfYear = dayOfYear
                , meanLoadGw = meanLoadGw
                , maxLoadGw = maxLoadGw
                , meanSolarGw = meanSolarGw
                , meanWindGw = meanWindGw
                , meanRenewableShare = meanRenewableShare
                , solarShare = solarShare
                , windShare = windShare
                , meanNetImportGw = meanNetImportGw
                , meanPriceEurMwh = meanPriceEurMwh
                , negativePriceHours = negativePriceHours
                }
        )
        (Decode.field "date" Decode.string)
        (Decode.field "month" Decode.int)
        (Decode.field "dayOfYear" Decode.int)
        (Decode.field "meanLoadGw" Decode.float)
        (Decode.field "maxLoadGw" Decode.float)
        (Decode.field "meanSolarGw" Decode.float)
        (Decode.field "meanWindGw" Decode.float)
        (Decode.field "meanRenewableShare" Decode.float)
        |> andMap (Decode.field "solarShare" Decode.float)
        |> andMap (Decode.field "windShare" Decode.float)
        |> andMap (Decode.field "meanNetImportGw" Decode.float)
        |> andMap (Decode.field "meanPriceEurMwh" (Decode.nullable Decode.float))
        |> andMap (Decode.field "negativePriceHours" Decode.int)


{-| `Json.Decode` bietet Kombinatoren nur bis `map8`, beide Records haben mehr
Felder. Statt einer zusaetzlichen Abhaengigkeit reicht dieser Kombinator, um
eine Decoder-Kette beliebig zu verlaengern.
-}
andMap : Decoder value -> Decoder (value -> result) -> Decoder result
andMap valueDecoder functionDecoder =
    Decode.map2 (\fn value -> fn value) functionDecoder valueDecoder



-- FILTER UND SUCHE


filterHourly : Maybe Int -> List Hourly -> List Hourly
filterHourly selectedMonth hourly =
    case selectedMonth of
        Nothing ->
            hourly

        Just month ->
            List.filter (\point -> point.month == month) hourly


filterDaily : Maybe Int -> List Daily -> List Daily
filterDaily selectedMonth daily =
    case selectedMonth of
        Nothing ->
            daily

        Just month ->
            List.filter (\day -> day.month == month) daily


findDaily : String -> List Daily -> Maybe Daily
findDaily date daily =
    List.filter (\day -> day.date == date) daily |> List.head


findHourlyForDate : String -> List Hourly -> List Hourly
findHourlyForDate date hourly =
    List.filter (\point -> point.date == date) hourly



-- FORMATIERUNG


monthName : Int -> String
monthName month =
    case month of
        1 ->
            "Jan"

        2 ->
            "Feb"

        3 ->
            "Mär"

        4 ->
            "Apr"

        5 ->
            "Mai"

        6 ->
            "Jun"

        7 ->
            "Jul"

        8 ->
            "Aug"

        9 ->
            "Sep"

        10 ->
            "Okt"

        11 ->
            "Nov"

        12 ->
            "Dez"

        _ ->
            "Alle"


monthFullName : Int -> String
monthFullName month =
    case month of
        1 ->
            "Januar"

        2 ->
            "Februar"

        3 ->
            "März"

        4 ->
            "April"

        5 ->
            "Mai"

        6 ->
            "Juni"

        7 ->
            "Juli"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "Oktober"

        11 ->
            "November"

        12 ->
            "Dezember"

        _ ->
            "Alle Monate"


{-| Zahl mit fester Nachkommastellenzahl und deutschem Dezimalkomma. -}
formatFloat : Int -> Float -> String
formatFloat digits value =
    let
        factor =
            10 ^ digits

        rounded =
            toFloat (round (value * toFloat factor)) / toFloat factor
    in
    String.replace "." "," (String.fromFloat rounded)
