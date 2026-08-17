"""Abruf der EnergyCharts-Tabellen ueber den PostgREST-Endpunkt der Universitaet Halle.

Der Endpunkt ist tokengeschuetzt, deshalb wird der Abruf einmalig offline
ausgefuehrt und nicht aus der Elm-Anwendung heraus. Das Ergebnis ist eine
statische JSON-Datei, die die Anwendung zur Laufzeit per HTTP nachlaedt.
"""

import base64
import datetime as dt
import json
import math
import pathlib
import urllib.error
import urllib.request
from collections import defaultdict


BASE_URL = "https://dbs.informatik.uni-halle.de/sciencedata"
USERNAME = "demo_user"
PASSWORD = "hallo"
YEAR = 2024
PAGE_SIZE = 500


def request_json(path, payload=None, token=None):
    """POST/GET gegen den Endpunkt, mit Wiederholung bei Netzfehlern."""
    last_error = None
    for _ in range(4):
        try:
            opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
            data = None if payload is None else json.dumps(payload).encode("utf-8")
            req = urllib.request.Request(
                BASE_URL + path,
                data=data,
                method="POST" if payload is not None else "GET",
            )
            if token:
                req.add_header("Authorization", "Bearer " + token)
            req.add_header("Content-Type", "application/json")
            with opener.open(req, timeout=90) as response:
                return json.loads(response.read().decode("utf-8"))
        except (TimeoutError, urllib.error.URLError) as error:
            last_error = error
    raise last_error


def get_token():
    """Bearer-Token per Basic Authentication holen."""
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    req = urllib.request.Request(BASE_URL + "/token", method="POST")
    auth = base64.b64encode(f"{USERNAME}:{PASSWORD}".encode("ascii")).decode("ascii")
    req.add_header("Authorization", "Basic " + auth)
    with opener.open(req, timeout=30) as response:
        return json.loads(response.read().decode("utf-8"))["token"]


def get_table(token, table_name, filters=None, where_=None):
    """Tabelle seitenweise lesen.

    Zeitstempel-Bereiche funktionieren ueber `where_`, Textgleichheit
    (country_id, market_id) dagegen zuverlaessig nur ueber `filters`.
    """
    rows = []
    offset = 0
    while True:
        payload = {
            "p_table_name": table_name,
            "order_by": [{"col": "unix_seconds", "dir": "asc"}],
            "limit_val": PAGE_SIZE,
            "offset_val": offset,
        }
        if filters is not None:
            payload["filters"] = filters
        if where_ is not None:
            payload["where_"] = where_

        chunk = request_json("/rpc/user_table_get", payload, token)
        rows.extend(chunk)
        if len(chunk) < PAGE_SIZE:
            return rows
        offset += PAGE_SIZE


def utc_date(timestamp):
    return dt.datetime.fromtimestamp(timestamp, tz=dt.timezone.utc)


def value(row, key):
    """Einen Leistungswert lesen und nach GW normieren.

    Die Quellspalten heissen "..._in_gw", fuehren die deutschen Werte aber in
    MW-Skala (Solar-Mittagswerte um 43000). Ohne die Division durch 1000 waeren
    saemtliche Achsen um drei Groessenordnungen falsch beschriftet.
    """
    raw = row.get(key)
    if raw is None:
        return 0.0
    return float(raw) / 1000.0


def mean(values):
    clean = [x for x in values if x is not None and math.isfinite(x)]
    if not clean:
        return None
    return sum(clean) / len(clean)


def pct(numerator, denominator):
    if denominator == 0:
        return 0.0
    return 100.0 * numerator / denominator


def build_hourly(power_by_hour, price_by_hour):
    """Viertelstundenwerte zu Stundenmitteln verdichten.

    Die Stunde ist die feinste Aufloesung, in der Erzeugung (Viertelstunden)
    und Boersenpreis (Stundenprodukt) gemeinsam definiert sind. Eine Kopplung
    auf Viertelstundenebene wuerde den Preis dreimal wiederholen und
    Scheingenauigkeit erzeugen.
    """
    hourly = []
    for hour_key in sorted(power_by_hour):
        rows = power_by_hour[hour_key]
        solar = mean([value(row, "solar_in_gw") for row in rows]) or 0.0
        wind_onshore = mean([value(row, "wind_onshore_in_gw") for row in rows]) or 0.0
        wind_offshore = mean([value(row, "wind_offshore_in_gw") for row in rows]) or 0.0
        fossil = sum(
            mean([value(row, key) for row in rows]) or 0.0
            for key in [
                "fossil_brown_coal_lignite_in_gw",
                "fossil_hard_coal_in_gw",
                "fossil_oil_in_gw",
                "fossil_coal_derived_gas_in_gw",
                "fossil_gas_in_gw",
            ]
        )
        renewable = (
            solar
            + wind_onshore
            + wind_offshore
            + (mean([value(row, "biomass_in_gw") for row in rows]) or 0.0)
            + (mean([value(row, "hydro_run_of_river_in_gw") for row in rows]) or 0.0)
            + (mean([value(row, "hydro_water_reservoir_in_gw") for row in rows]) or 0.0)
            + (mean([value(row, "geothermal_in_gw") for row in rows]) or 0.0)
        )
        residual = mean([value(row, "residual_load_in_gw") for row in rows]) or 0.0

        # load_in_gw ist in der totalpower-Sicht fuer Deutschland durchgehend
        # leer. Die Last wird daher aus der Definition der Residuallast
        # rekonstruiert: Residuallast = Last minus dargebotsabhaengige
        # Einspeisung. Das ist eine Naeherung, weil Biomasse, Wasserkraft und
        # Geothermie dort nicht abgezogen werden.
        load = solar + wind_onshore + wind_offshore + residual

        trade = mean([value(row, "cross_border_electricity_trading_in_gw") for row in rows]) or 0.0
        price = mean(price_by_hour.get(hour_key, []))

        hourly.append(
            {
                "timestamp": hour_key.isoformat().replace("+00:00", "Z"),
                "date": hour_key.date().isoformat(),
                "month": hour_key.month,
                "dayOfYear": int(hour_key.strftime("%j")),
                "hour": hour_key.hour,
                "loadGw": round(load, 3),
                "solarGw": round(solar, 3),
                "windOnshoreGw": round(wind_onshore, 3),
                "windOffshoreGw": round(wind_offshore, 3),
                "fossilGw": round(fossil, 3),
                "renewableGw": round(renewable, 3),
                "renewableShare": round(pct(renewable, load), 2),
                "netImportGw": round(trade, 3),
                "priceEurMwh": None if price is None else round(price, 2),
            }
        )
    return hourly


def build_daily(hourly):
    """Tagesprofile aus den Stundenwerten bilden.

    Die Tagesebene ist die Analyseeinheit fuer den mehrdimensionalen Vergleich:
    Eine einzelne extreme Stunde sagt wenig, ein Tagesprofil charakterisiert
    eine Wetterlage. Solar- und Windanteil beziehen sich auf die mittlere Last
    des Tages, damit sie mit dem Erneuerbarenanteil vergleichbar bleiben.
    """
    parts = defaultdict(list)
    for point in hourly:
        parts[point["date"]].append(point)

    daily = []
    for date_key, rows in sorted(parts.items()):
        avg_price = mean([row["priceEurMwh"] for row in rows])
        load = mean([row["loadGw"] for row in rows]) or 0.0
        solar = mean([row["solarGw"] for row in rows]) or 0.0
        wind = mean([row["windOnshoreGw"] + row["windOffshoreGw"] for row in rows]) or 0.0
        daily.append(
            {
                "date": date_key,
                "month": rows[0]["month"],
                "dayOfYear": rows[0]["dayOfYear"],
                "meanLoadGw": round(load, 3),
                "maxLoadGw": round(max(row["loadGw"] for row in rows), 3),
                "meanSolarGw": round(solar, 3),
                "meanWindGw": round(wind, 3),
                "meanRenewableShare": round(mean([row["renewableShare"] for row in rows]) or 0.0, 2),
                "solarShare": round(pct(solar, load), 2),
                "windShare": round(pct(wind, load), 2),
                "meanNetImportGw": round(mean([row["netImportGw"] for row in rows]) or 0.0, 3),
                "meanPriceEurMwh": None if avg_price is None else round(avg_price, 2),
                "negativePriceHours": len(
                    [row for row in rows if row["priceEurMwh"] is not None and row["priceEurMwh"] < 0]
                ),
            }
        )
    return daily


def main():
    out_path = pathlib.Path(__file__).resolve().parents[1] / "data" / f"energycharts_de_{YEAR}.json"
    token = get_token()

    start = dt.datetime(YEAR, 1, 1, tzinfo=dt.timezone.utc)
    end = dt.datetime(YEAR + 1, 1, 1, tzinfo=dt.timezone.utc)

    power_rows = get_table(
        token,
        "energycharts_totalpower",
        where_=[
            {"col": "unix_seconds", "op": ">=", "val": int(start.timestamp()), "logic": "and"},
            {"col": "unix_seconds", "op": "<", "val": int(end.timestamp()), "logic": "and"},
        ],
    )
    price_rows = get_table(token, "energycharts_price", {"market_id": "DE-LU"})

    price_by_hour = defaultdict(list)
    for row in price_rows:
        instant = utc_date(int(row["unix_seconds"]))
        if start <= instant < end:
            price_by_hour[instant.replace(minute=0, second=0, microsecond=0)].append(row.get("price"))

    power_by_hour = defaultdict(list)
    for row in power_rows:
        instant = utc_date(int(row["unix_seconds"]))
        if start <= instant < end:
            power_by_hour[instant.replace(minute=0, second=0, microsecond=0)].append(row)

    hourly = build_hourly(power_by_hour, price_by_hour)
    daily = build_daily(hourly)

    payload = {
        "meta": {
            "country": "Germany",
            "countryId": "de",
            "priceMarket": "DE-LU",
            "year": YEAR,
            "source": "University Halle ScienceData PostgREST mirror of EnergyCharts",
            "generatedAt": dt.datetime.now(dt.timezone.utc).isoformat().replace("+00:00", "Z"),
            "notes": [
                "Stundenwerte sind Mittel ueber die 15-Minuten-Zeilen von totalpower.",
                "Leistungswerte der Quelle liegen in MW-Skala vor und werden nach GW normiert.",
                "Die Last ist als solar + wind_onshore + wind_offshore + residual_load "
                "rekonstruiert, weil load_in_gw in totalpower fuer Deutschland leer ist.",
                "cross_border_electricity_trading_in_gw wird als vorzeichenbehafteter "
                "Nettohandelswert der Quelle uebernommen.",
                "Fehlende Stunden der Quelle werden nicht interpoliert.",
            ],
        },
        "hourly": hourly,
        "daily": daily,
    }
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print(f"{len(hourly)} Stundenwerte und {len(daily)} Tagesprofile nach {out_path} geschrieben")


if __name__ == "__main__":
    main()
