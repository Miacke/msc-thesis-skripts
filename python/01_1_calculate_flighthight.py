# ============================================================
# 01_1_calculate_flighthight.py
# ============================================================
# Beschreibung:
#   Berechnet absolute Flughöhen pro Fluglinie. Spezifische Eigenschaften der verwendeten Metadaten sind hard gecoded. Bei einigen Linien können die Flughöhen aufgrund mangelhafter Datengrundlage nicht mehr rekonstruiert werden.
#
# Vorgehen:
#   - Iteriert durch alle Line-Features aller Streifen-FC in allen Datasets des angegebenen Workspace
#   - Linien ohne z-Werte werden nicht berücksichtigt, da die Höhe aufgrund fehlender Grundlagendaten nicht mehr rekonstruiert werden kann
#   - Setzt bei allen Linien 4 Punkte (20%, 40%, 60%, 80% der Länge) und weist ihnen die Linienattribute per Spatial Join zu
#   - Für jeden Punkt wird über eine REST-Schnittstelle die Terrainhöhe abgefragt
#   - Die Terrainhöhen werden zum Z-Wert der Linie addiert und gespeichert
#   - Pro Linie wird die mittlere Höhe und der Range in ein eigenes Feld geschrieben
#
# Input:
#   - file-Geodatabase mit Linien
#
# Output:
#   - Modifiziert die Inputdaten, und zwar alle Streifen-FC mit gültigen z-werten
#
# Autor:       Mirco Ackermann
# Datum:       17.06.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

import arcpy
import requests
import os
import utils

arcpy.env.workspace = r"A:\11_MasterThesis\01_DefStruktur\02_Data\SURVEY_AREA_COMPLETE.gdb"

for dataset in arcpy.ListDatasets(feature_type="Feature"):  # Durch alle FeatureDatasets iterieren
    print("-----------------------------------------------------------------------------------------------------")
    print(f"Start in FeatureDataset {dataset}")

    with arcpy.EnvManager(workspace=os.path.join(arcpy.env.workspace, dataset)):  # Workspace zu dataset ändern
        for feature_class in arcpy.ListFeatureClasses():  # Durch alle FC in dataset iterieren
                desc = arcpy.Describe(feature_class)
                
                if desc.shapeType == "Polyline":
                    print(f"Start bei Feature Class {feature_class}")
                    
                    # Punkte entlang der Linie generieren
                    print("generiere Punkte entlang der Fluglinien...")
                    temp_pointsalongline = r"in_memory\temp_pointsalongline"  # Punkte ins memory
                    temp_pointswithatt = r"in_memory\temp_pointswithatt"  # Punkte mit Linienattributen ins memory

                    arcpy.management.GeneratePointsAlongLines(  # 4 gleichmässig verteilte Punkte pro Linie berechnen
                        Input_Features=desc.name,
                        Output_Feature_Class=temp_pointsalongline,
                        Point_Placement="PERCENTAGE",
                        Distance=None,
                        Percentage=20,
                        Include_End_Points="NO_END_POINTS",
                        Add_Chainage_Fields="NO_CHAINAGE",
                        Distance_Field=None,
                        Distance_Method="PLANAR"
                    )
                     
                    arcpy.analysis.SpatialJoin(
                        target_features=temp_pointsalongline,
                        join_features=desc.name,
                        out_feature_class=temp_pointswithatt,
                        join_operation="JOIN_ONE_TO_ONE",
                        join_type="KEEP_ALL",
                        match_option="INTERSECT",
                        search_radius=None,
                        distance_field_name="",
                        match_fields=None
                    )


                    
                    # Für jeden Punkt muss die absolute Geländehöhe mittels REST-Abfrage bei voibos.rechenraum.com (geoland.at) abgefragt werden
                    print("Lese Höhen aus...")
                    https_prefix = r"https://voibos.rechenraum.com/voibos/voibos?name=hoehenservice&Koordinate="
                    https_suffix = fr"&CRS={desc.spatialReference.factoryCode}"  # der EPSG-Code des CRS steht im Describe-Objekt
                    hoehen_dict = {}  # Dictionary, wo pro Linie alle Höhendaten gespeichert werden

                    # Zähler für Fortschrittkontrolle
                    counter = 0
                    point_count = int(arcpy.management.GetCount(temp_pointswithatt)[0])

                    # Inputdaten haben teilweise keine z-werte, dann wird geskippt
                    if arcpy.Describe(temp_pointsalongline).hasZ:  # einige Linien sind nicht in 3D
                        with arcpy.da.SearchCursor(temp_pointswithatt, ["SHAPE@XYZ", "STR_BEZ"]) as cursor:
                            counter = 0
                            for row in cursor:

                                # Fortschrittkontrolle
                                counter = counter + 1
                                if counter % 200 == 0 or counter == point_count:
                                    print(f"Fortschritt: {counter}/{point_count}")

                                # Zeilen entpacken
                                x, y, z = row[0]
                                str_bez = row[1]

                                # url für DTM-Abfrage generieren
                                http_url = fr"{https_prefix}{x},{y}{https_suffix}"

                                # Teilweise haben 3D-Linien einen z-wert = 0. Diese werden ausgelassen                              
                                if z is None or z == 0:
                                    continue

                                # Request ausführen
                                try:
                                    response = requests.get(http_url, timeout=10)
                                    response.raise_for_status()  # Fehler wenn HTTP-Status nicht = 200 (200 = OK)

                                    data = response.json()  # Rückgabe im JSON-Format

                                    # Geländehöhe auslesen, Flughöhe addieren und in dict speichern
                                    hoehe_dtm = data["hoeheDTM"]
                                    try:  # Einige Linien liegen ausserhalb von Österreich, wo voibox.rechenraum.at keine Daten zur Verfügung stellt. Antwort = "n/a".
                                        hoehe_dtm = float(hoehe_dtm)
                                        if str_bez not in hoehen_dict:
                                            hoehen_dict[str_bez] = []
                                        hoehen_dict[str_bez].append(hoehe_dtm + z)
                                    except (ValueError, TypeError):  # Der entsprechende Punkt wo keine Höhenwerte bestehen wird einfach übersprungen. Wenn keine gültigen Werte für die Fluglinie bestehen macht updateCursor später NULL
                                        print(f"{str_bez}: keine gültige Geländehöhe gefunden, wahrscheinlich ausserhalb AT")


                                except requests.exceptions.RequestException as e:
                                    print(f"{str_bez}: Fehler bei Request → {e}")

                        # Werte für Flughöhe pro Linie berechnen und als Attribut bei der Linie hinzufügen
                        # Felder erstellen
                        print(f"Felder für Flughöhen in {feature_class} erstellen...")
                        utils.delete_add_field(feature_class, "flug_h_mean", "DOUBLE")
                        utils.delete_add_field(feature_class, "flug_h_range", "DOUBLE")

                        # Dictionary zum Zwischenspeichern erstellen
                        flug_h_dict = {}

                        # Pro Linie durchschnittliche Flughöhe und Range berechnen
                        print("Durchschnitt und Range der Flughöhen berechnen...")
                        for str_bez in hoehen_dict:
                            flug_h_mean = sum(hoehen_dict[str_bez]) / len(hoehen_dict[str_bez])
                            flug_h_range = max(hoehen_dict[str_bez]) - min(hoehen_dict[str_bez])
                            if str_bez not in flug_h_dict:
                                flug_h_dict[str_bez] = []
                            flug_h_dict[str_bez].append(flug_h_mean)  # 1. Wert im Dict = flug_h_mean
                            flug_h_dict[str_bez].append(flug_h_range)  # 2. Wert im Dict = flug_h_range
                        
                        print(f"Werte der Flughöhen in {feature_class} speichern")
                        with arcpy.da.UpdateCursor(feature_class, ["STR_BEZ", "flug_h_mean", "flug_h_range"]) as cursor:
                            for row in cursor:
                                str_bez = row[0]
                                if str_bez in flug_h_dict:
                                    row[1] = flug_h_dict[str_bez][0]  # durchschn. Flughöhe in das vorher angelegte Feld schreiben
                                    row[2] = flug_h_dict[str_bez][1]  # Range in das vorher angelegte Feld schreiben
                                    
                                    cursor.updateRow(row)

                        print(f"Feature Class {feature_class} fertig!")
                    
                    else:
                        print(f"Feature Class {feature_class} hat keine Z-Werte. Flughöhen wurden NICHT berechnet!")

                    # Memory löschen
                    for temp in [temp_pointsalongline, temp_pointswithatt]:
                        if arcpy.Exists(temp):
                            arcpy.management.Delete(temp)



                             
                            



                
