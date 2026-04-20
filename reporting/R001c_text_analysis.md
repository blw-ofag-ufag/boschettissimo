> [!IMPORTANT]
> Hinweis: Dies ist eine vorläufige Version. Die Analyse ist noch nicht abgeschlossen und wird weiter überarbeitet.

## Analyse der existierende Datensätze

Idealerweise würde man einen existierenden Datensatz nützen, um die Einzelbäume zu identifizieren.

### swissTLM3D (v 2025) tlm_bb_einzelbaum_gebuesch : 
Auf der Karte _TLM - Einzelbaum & Gebuesch_

Das Produkt swissTLM3D enthält einen Datensatz, der Einzelbäume > 5m Höhe darstellt. Die Einzelbäume werden bei der Krone oder beim Wipfel in 3D erfasst.

### TLM nicht filtrierte Einzelbäume (nicht publiziert): 
Auf der Karte _TLM - Einzelbaum & Gebuesch (raw data)_

Dieses Produkt haben wir lokal an der WSL, enthält die unfiltrierte Einzelbäume.

### Habitat Map Einzelbaum und Gebüsche v1.0 : 
Auf der Karte _Habitat Map - Einzelbaum & Gebuesche_

Im Rahmen des Projekts „Erstellung einer Lebensraumkarte der Schweiz“ wurde ein zusätzlicher Layer für Einzelbaum und Gebüsche ausserhalb des Waldes erstellt, der die TypoCH-Klassifikation berücksichtigt. Dieser Layer ergänzt die bestehende Lebensraumkarte, indem er Einzelbaum (TypoCH: STR6000) und Gebüsche (TypoCH: 5.3) ausserhalb des Waldes flächendeckend erfasst.
Die Ableitung erfolgte regelbasiert in eCognition unter Verwendung von SwissSurface3D LiDAR-Daten (Vegetationshöhenmodelle), sowie räumlichen Nachbarschaftsbeziehungen.

Die Klassifikation unterscheidet:

•	Einzelbaum (Höhe ≥ 3 m, Segmentfläche ≥ 6 m² oder, wenn in TLM von swisstopo TLM_Einzelbaum kartiert),
•	Gebüsche (Höhe zwischen 0.5 m und 3 m).

Zusätzlich wurden Attribute wie Kronenfläche, maximale und mittlere Höhe, Volumen oberhalb 3m und Vegetationskomplexitätsindex (VCI) berechnet.

## Neu produzierte Datensätze

Da die existierende Datensätze ein paar Einschränkungen haben, die in diesem Projekt problematisch sein könnten (z.B. ziemlich hoher Wert für die mindest Höhe eines Baums (TLM 5m, HM 3m)) wird in den ersten Schritten auch getestet, ob es Sinn machen würde, einen eigenen Datensatz zu erstellen. Die Issue https://github.com/blw-ofag-ufag/boschettissimo/issues/36 stellt die soweit untersuchte algorithmen vor.

### Segmentierung mit dem Watershed algorithmus: 
Auf der Karte _NEW - Segmentierte Baume (watershed)_

Dieser Datensatz enthält die segmentierte Bäume, die anhand des Watershed algorithm definiert wurden (Basis ist das VHM). Hier ist der parameter `ext=2` gesetzt, was zu einer bekannte "oversegmentation" führt.

## Erste Analysen

Über die verschiedene analysierten Untersuchungsgebiete konnten folgende Elemente entdeckt werden:

- **Unterschiede zwischen die TLM Produkte** Der tlm_bb_einzelbaum_gebuesch Datensatz übereinstimmt nicht perfekt mit dem TLM nicht filtrierte Einzelbäume Datensatz, und auch nicht mit der aktuelle Landeskarte von swisstopo. Die Punkte von den verschiedene Sourcen sind nämlich manchmal ein bisschen verschoben. Ein paar Punkte sind auch entweder auf der Landeskarte aber nicht in die zwei TLM Datensätze, und vice-versa. Auf der Landeskarte sind auch kleinere Bäume als isolierte Bäume dargezeigt, sind aber nicht in die zwei TLM Datensätze sichtbar. Wahrscheinlich sind diese 3 Quellen (tlm_bb_einzelbaum_gebuesch, nicht filtrierte Einzelbäume und die Landeskarte) auf verschiedene referenz Jahren aufgabaut, das scheint aber nicht alle Unterschiede zu erklären.

- **Geometrie Problem der TLM Produkte** Da die TLM Produkten Punkte sind, sind die Bäume immer noch nicht segmentiert. Das heisst, das falls einer diesen Datensätze gewählt sein sollte, müsste man immer noch sich für ein Segmentierung Algorithm entscheiden um die erwünschte Kennzahlen abbilden zu können.

- **Verschiedene Referenz Jahren zwischen den Produkten und Untersuchungsgebiete** Es kann manchmal sein, dass z.B Punkte die im tlm Datensatz dargestellt sind (und auch auf der Landeskarte), nicht (mehr?) sichtbar sind, wenn man auf das Luftbild schaut. Dies ist vorallem in Siedlungen sehr klar sichtbar, aber auch auf einzelne landwirtschaftliche Flächen wie z.B. rechts von Beckelswilen im Untersuchungsgebiet Engelswilen. Für dieses Beispiel sieht man auf map.geo.admin (2'728'980.44, 1'272'275.80) mit dem Produkt SWISSIMAGE Zeitreise dass diese Bäume auf die Luft Bildern von 2022-2024 noch sichtbar waren, aber dann auf das Bild von 2025 nicht mehr sichtbar sind, was jedoch im tlm Datensatz von 2025 noch nicht angepasst wurde.

- **Habitat Map Basis vs Habitat Map EBuG** siehe Untersuchungsgebiet Engelswilen Winggelagger (2728979.57,1272941.39). Da sieht man, das ein Teil der Einzelbäume rausfiltriert wurde, weil die Basis Karte der Habitat Map da die Delarze Klasse 6.2.3 Waldmeister-Buchenwald sieht. Generell ist die Waldmaske Filtrierung nicht ideal, viele Silverpolygone sind sichtbar entland fast alle Waldrände. Dies wird aber in einere spätere Version von der Habitat Map gelöst werden.

- **Neue Segmentierte Layer** Die Resultate sind ziemlich ähnlich zu was man in der Habitat Map EBuG layer sieht. Wahrscheinlich ist das Referenzjahr der genutzte VHM für diese beide Datensätze unterschiedlich. Darum sind neue gepflanzte Bäume auch sichtbar (westlich von Rüüti, Untersuchungsgebiet Engelswilen (2'728'171.59, 1'272'716.68)). Dies ist vielleicht auch weil hier Bäume ab 2m hoch im Datensatz reinkommen, gegen 3m für die Habitat Map. Problematischer sind die Obstanlagen (Apfel, Beeren etc) die im Moment auch noch dargestellt sind, manchmal als sehr lange lineare Polygone (siehe Untersuchungsgebiet Rickenbach Nord-Ost). Der Wald ist auch immer noch im Datensatz. Diese Layer soll noch filtriert werden, in dem wir nur die gewünschte landwirtschaftliche Nützungsfläche behalten werden. 
