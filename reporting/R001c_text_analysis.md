> [!IMPORTANT]
> Hinweis: Dies ist eine vorläufige Version. Die Analyse ist noch nicht abgeschlossen und wird weiter überarbeitet.



## Analyse der existierende Datensätze

### swissTLM3D (v 2025) - tlm_bb_einzelbaum_gebuesch

Das Produkt swissTLM3D enthält einen Datensatz, der Einzelbäume > 5m Höhe darstellt. Die Einzelbäume werden bei der Krone oder beim Wipfel in 3D erfasst.

Über die verschiedene analysierten Untersuchungsgebiete konnten folgende Elemente entdeckt werden:

**tlm_bb_einzelbaum_gebuesch vs Landeskarte**: Der tlm_bb_einzelbaum_gebuesch Datensatz übereinstimmt nicht mit der aktuelle Landeskarte von swisstopo. Die Punkte vom tlm Datensatz sind nämlich manchmal ein bisschen verschoben verglichen mit der Landeskarte. Ein paar Punkte sind auch entweder auf der Landeskarte aber nicht im tlm_datensatz, und vice-versa. Auf der Landeskarte sind auch kleinere Bäume als isolierte Bäume dargezeigt, sind aber nicht im TLM Datensatz sichtbar. In welchen Datensatz sind diese kleinere Bäume vorhanden?

**tlm_bb_einzelbaum_gebuesch vs VHM**: Dasselbe gilt auch hier. Vom VHM sind ein paar wahrscheinlich hohe Baume zu sehen, die nicht im tlm Datensatz vorkommen, und vice-versa.

**tlm_bb_einzelbaum_gebuesch vs SwissImage**: Mehrere Punkte sind im tlm Datensatz dargestellt (und auch auf der Landeskarte), wenn man aber auf das Luftbild schaut, merkt man, dass mehrere von diese Bäume nicht (mehr?) sichtbar sind. Dies ist vorallem in Siedlungen sehr klar sichtbar, aber auch auf einzelne landwirtschaftliche Flächen wie z.B. rechts von Beckelswilen im Untersuchungsgebiet Engelswilen. Für dieses Beispiel sieht man auf map.geo.admin (2'728'980.44, 1'272'275.80) mit dem Produkt SWISSIMAGE Zeitreise dass diese Bäume auf die Luft Bildern von 2022-2024 noch sichtbar waren, aber dann auf das Bild von 2025 nicht mehr sichtbar sind, was jedoch im tlm Datensatz von 2025 noch nicht angepasst wurde.

**SwissImage vs VHM**: Diese beide Datensätze scheinen besser übereinzustimmen.
