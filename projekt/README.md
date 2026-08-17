# Projekt: EnergyCharts Visual Analytics

Visual-Analytics-Anwendung in Elm zu den EnergyCharts-Daten für Deutschland 2024.

Leitfrage: Wie hängen Solar- und Windmuster mit der Deckung durch erneuerbare Energien,
dem Börsenpreis und dem grenzüberschreitenden Handel zusammen?

Drei interaktiv verbundene Visualisierungen aus drei verschiedenen Bereichen:

| Ansicht | Bereich | Rolle |
| --- | --- | --- |
| Zeitreihen-Übersicht der Monate | Zeitreihen-Diagramme | Einstieg und Monatsfilter |
| Stunden-Heatmap (Tag × Stunde) | Icon- und Pixel-orientierte Techniken | alle 8690 Stundenwerte gleichzeitig |
| Parallele Koordinaten der Tagesprofile | Mehrdimensionale Darstellungen | Tagesvergleich über sieben Attribute |

Alle drei lesen denselben Interaktionszustand aus `Main.elm`: Monatsfilter (Mehrfachauswahl),
überfahrener Tag, ausgewählte Tage und die Achsenfilter der parallelen Koordinaten.

## Verzeichnisse

```
projekt/
├── index.html      Lädt den Datensatz per fetch und startet Elm
├── main.js         Ausgabe von elm make
├── data/           Der vorverarbeitete Datensatz
├── scripts/        Abruf und Vorverarbeitung der Rohdaten
└── src/
    ├── Main.elm                        Model, Msg, update, Komposition
    ├── Data.elm                        Typen, Decoder, Filter, Formatierung
    └── Views/
        ├── TimeSeries.elm              Visualisierung Eins
        ├── Heatmap.elm                 Visualisierung Zwei
        ├── ParallelCoordinates.elm     Visualisierung Drei
        └── Layout.elm                  Rahmen, Kennzahlen, Detailpanel, Tooltip, CSS
```

## Daten

Quelle sind die EnergyCharts-Daten über den PostgREST-Endpunkt der Universität Halle
(`https://dbs.informatik.uni-halle.de/sciencedata`). Verwendet werden zwei Sichten:

- `energycharts_totalpower` — Erzeugung je Energieträger in 15-Minuten-Auflösung
- `energycharts_price` — Börsenpreise der Gebotszone `DE-LU`

Der Endpunkt ist tokengeschützt, deshalb läuft der Abruf einmalig offline:

```bash
python3 scripts/fetch_energycharts.py
```

Das Skript schreibt `data/energycharts_de_2024.json` (8690 Stundenwerte, 364 Tagesprofile).
Wesentliche Verarbeitungsschritte, jeweils im Skript kommentiert:

- Leistungswerte der Quelle liegen trotz Spaltenname `..._in_gw` in MW-Skala vor und werden
  nach GW normiert.
- Viertelstundenwerte werden zu Stundenmitteln verdichtet, weil der Preis ein Stundenprodukt ist.
- `load_in_gw` ist für Deutschland leer; die Last wird aus der Definition der Residuallast
  rekonstruiert.
- Der Quelle fehlen 94 Stunden (23.05. und 12.10., jeweils 47 h). Sie werden **nicht**
  interpoliert, sondern in der Heatmap als graue Zellen sichtbar gehalten.

## Übersetzen und starten

```bash
elm make src/Main.elm --output=main.js
python3 -m http.server 8000
# http://localhost:8000/index.html
```

Ein Webserver ist nötig: `index.html` lädt den Datensatz per `fetch`, was der Browser über
`file://` blockiert. Zum Veröffentlichen genügt es, `index.html`, `main.js` und `data/`
nebeneinander auf einen beliebigen statischen Webserver zu legen.

## Bedienung

- **Monat filtern** — Schaltflächen oben oder Klick auf einen Balken der Zeitreihe.
  Mehrfachauswahl möglich, „Alle" hebt den Filter auf.
- **Überfahren** — hebt den Tag in Heatmap *und* parallelen Koordinaten hervor und zeigt
  einen Tooltip mit den konkreten Werten.
- **Klicken** — nimmt den Tag in die Auswahl auf; erneuter Klick entfernt ihn. Das
  Detailpanel listet die Auswahl und zeigt Kennzahlen samt Stundenprofil.
- **Achsen-Brushing** — in den parallelen Koordinaten auf einer Achse einen Wertebereich
  aufziehen. Mehrere Bereiche werden UND-verknüpft; Klick auf eine Achse löscht ihren Filter.
