"""Abruf der EnergyCharts-Tabellen ueber den PostgREST-Endpunkt der Universitaet Halle.

Der Endpunkt ist tokengeschuetzt, deshalb wird der Abruf einmalig offline
ausgefuehrt und nicht aus der Elm-Anwendung heraus.
"""

import base64
import datetime as dt
import json
import urllib.error
import urllib.request


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


def main():
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

    print(f"totalpower: {len(power_rows)} Zeilen")
    print(f"price: {len(price_rows)} Zeilen")


if __name__ == "__main__":
    main()
