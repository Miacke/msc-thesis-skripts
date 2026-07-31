# ============================================================
# 02_find_crossings.py
# ============================================================
# Beschreibung:
#   Ausscheidung der Untersuchungsgebiete für die Master-Thesis, aufbauend auf 00_copy_fc.py, 01_load_and_detect_blocks.py und
#   01_1_calculate_flighthight.py. Es werden Footprints mit derselben Überlappungsanzahl (innerhalb der Parallelstreifen) verschiedener
#   sich überlappender Blöcke ausgegeben, welche bezüglich Aufnahmezeitpunkt, Flughöhe und -Geschwindigkeit innerhalb einer gegebenen Toleranz liegen
#   und bezüglich Sensor identisch sind.
#
# Vorgehen:
#   1. Verbindung der Footprints mit den Trajektorien (Join) und Feldnamen vereinheitlichen
#   2. Fluggeschwindigkeit aus Liniengeometrie und Zeitstempel (Start und Ende) neu berechnen
#   3. Blöcke finden, welche sich bezüglich Ausrichtungswinkel "genügend" unterscheiden und sich räumlich überlappen (keine disjunkten Blöcke) -> generiert Blockpaare
#       für die spätere Überprüfung
#   4. Überlappungsflächen innerhalb eines Blocks suchen (Überlappung der Parallelstreifen) und Anzahl bestimmen
#   5. Pro Blockpaar (Schritt 3) werden Überlappungsflächen nach Anzahl gruppiert.
#   6. Hat ein Block eines Blockpaars eine Fläche mit einer bestimmten Überlappungsanzahl, welche die Fläche des "Partners" mit derselben Überlappungszahl überschneidet, 
#       werden die folgenden Erhebungsparameter verglichen:
#       a. Flughöhen der beteiligten Linien müssen innerhalb Toleranz liegen
#       b. Fluggeschwindigkeit der beteilitgten Linien muss innerhalb Toleranz liegen
#       c. Sensortyp muss identisch sein
#       d. Vegetationsphase muss identisch sein (im Sommer und Winter wird nur innerhalb desselben Jahres verglichen, In der Wachstumsphase nur innerhalb von 3 Tagen)
#   7. Wenn in einem Blockpaar alles i.O. -> Schnittfläche in survey schreiben und mit Attributen beschreiben
#   8. Logfile schreiben, welches Parameterwahl und Ergebnis festhält
#
# Output:
#   - Polygon-FC "survey_area_<dataset>_<version>" mit den gefundenen Untersuchungsgebieten und Attributen zu Blöcken, Winkel-, Höhen- und 
#       Geschwindigkeitsdifferenzen, Mittelwerten, Sensor und beteiligten Fluglinien (str_bez_list).
#   - Log-Datei mit den verwendeten Parametern und der Gesamtzahl gefundener Untersuchungsgebiete.
#
# Autor:       Mirco Ackermann
# Datum:       14.05.2026
# Projekt:     UNIGIS MasterThesis
# ============================================================
#%%
import arcpy
import os
from collections import defaultdict
from datetime import datetime
import utils

# Input-Values setzen
arcpy.env.workspace     = r"A:\11_MasterThesis\01_DefStruktur\02_Data\02_Survey_areas\SURVEY_AREA_COMPLETE.gdb"
used_datasets           = ["EPSG_25833"]  # Datasets in welchen alle FeatureKlassen berechnet werden sollen, wenn leer werden alle Datasets berechnet
footprint_namesuffix    = "_Deckung"  # Endung der FC, welche die Footprints beinhealten
trajectory_namesuffix   = "_Streifen"  # Endung der FC, welche die Flugtrajektorien beinhalten
join_field              = "STR_BEZ"  # Joinfeld für join zwischen Linien und Flächen
flug_h_field            = "flug_h_mean"  # Feld in dem Flughöhe gespeichert ist
flug_v_field            = "FLUG_V"  # Feld in dem Fluggeschwindigkeit gespeichert ist
sensor_name_field       = "SENSOR"  # Feld indem die Sensorspezifikation gespeichert ist
f_date_field            = "DATUM"  # Feld indem das Flugdatum gespeichert ist. ACHTUNG: Chance gering, dass immer als date-feld abgespeichert!
line_starttime_field    = "GNSSTIME_A"  # Start-GNSS-Zeit der Fluglinie
line_endtime_field      = "GNSSTIME_E"  # End-GNSS-Zeit der Fluglinie
version                 = "8"  # Versionsnummer die gesetzt werden kann, wenn mehrere Varianten gerechnet werden sollen. Datensatz bekommt Versionsnummer in Namen. ACHTUNG: Nur für fc-namen erlaubte zeichen a-z, A-Z, 0-9, _

# Parameter setzen
angle_cross_tol = 30  # Winkelunterschied in Grad, bei welchem zwei Flugtrajektorien als unterschiedlich gelten. Die Forschung legt nahe, dass der Unterschied nicht so wichtig ist, sondern eher die zweite Richtung. Winkel kann also tief gehalten werden.
flight_h_tol = 150  # Tolerierbarer Unterschied in der Flughöhe in Meter. ganzer Wert angeben (700 - 800 -> Toleranz = 100)
flight_v_tol = 100  # Tolerierbarer Unterschied in der Fluggeschwindigkeit in m/s. ganzer Wert angeben (60 - 70 -> Toleranz = 10)
sensor_test = True  # Wenn True: Es wird überprüft, ob genau der gleiche Sensor verwendet wurde
date_test = True  # Wenn True: Es wird überprüft ob innerhalb desselben Stadiums im Vegetationszyklus geflogen wurde
speed_calc = True  # Wenn True: Echte Fluggeschwindikgeit wird berechnet

# Variablen initialisieren
block_angles     = defaultdict(list)
block_mean_angle = {}  # block_id -> [angle1, angle2, ...]
total_surveys = 0  # Counter für log-file

# %%
# ----------------------------------------------------------------------------------------------------------------------
# Flächen (footprints) mit Trajektorien joinen
# ----------------------------------------------------------------------------------------------------------------------

# Fortschritt überwachen
starttime = datetime.now()
print(f"Starte um {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")


# Dataset anwählen und temp. Workspace setzen
for dataset in arcpy.ListDatasets(feature_type="Feature"):  # Durch alle FeatureDatasets iterieren
    if not used_datasets or dataset in used_datasets:  # Entweder die angegebenen oder alles Datasets in fGDB behandeln
        print(f"Start in FeatureDataset {dataset}")
        with arcpy.EnvManager(workspace=os.path.join(arcpy.env.workspace, dataset)):
            
            # -------------------------------------------------------------------------
            # diverse Vorbereitungen / Einrichtungen für später

            out_survey_fc = os.path.join(arcpy.env.workspace, f"survey_area_{dataset}_{version}")  # Output-Feature Class in die alle gefundenen Untersuchungsgebiete geschrieben werden
            if arcpy.Exists(out_survey_fc):
                arcpy.management.Delete(out_survey_fc)

            # Referenzsystem aus Dataset holen
            desc_ds = arcpy.Describe(arcpy.env.workspace)
            sr = desc_ds.spatialReference

            # Output_FC erstellen
            arcpy.management.CreateFeatureclass(  # Erstellen der out_survey_fc, in die alle Untersuchungsgebiete geschrieben werden ()
                out_path=os.path.join(arcpy.env.workspace),
                out_name=f"survey_area_{dataset}_{version}",
                geometry_type="POLYGON",
                spatial_reference = sr
            )

            # Felder hinzufügen, welche benötigt werden um die relevanten Linien zu identifizieren
            arcpy.management.AddField(out_survey_fc, "block_a",      "LONG")  # Flugblock welcher ein Teil der Linien zum Untersuchungsgebiet beiträgt.
            arcpy.management.AddField(out_survey_fc, "block_b",      "LONG")  # Flugblock welcher ein Teil der Linien zum Untersuchungsgebiet beiträgt.
            arcpy.management.AddField(out_survey_fc, "count",        "LONG")  # Anzahl der Überlappunten
            arcpy.management.AddField(out_survey_fc, "str_bez_list", "TEXT", field_length=5000)  # Eindeutige Bezeichnung der Fluglinie
            arcpy.management.AddField(out_survey_fc, "fc_name", "TEXT", field_length=200)  # Bezeichnung der Ursprungs-FC
            arcpy.management.AddField(out_survey_fc, "angle_diff", "LONG")  # Winkeldifferenz zw. den beiden Flugblocks
            arcpy.management.AddField(out_survey_fc, "delta_v", "DOUBLE")  # Unterschied Fluggeschwindigkeit der beiden BLocks
            arcpy.management.AddField(out_survey_fc, "delta_h", "DOUBLE")  # Unterschied Flughöhe der beiden BLocks
            arcpy.management.AddField(out_survey_fc, "mean_v", "DOUBLE")  # Durchschnittsgeschwindigkeit aller an Überlappung beteiligten Linien
            arcpy.management.AddField(out_survey_fc, "mean_h", "DOUBLE")  # Durchschnittshöhe aller an Überlappung beteiligten Linien
            arcpy.management.AddField(out_survey_fc, "sensor", "TEXT")  # Sensortyp

            with arcpy.da.InsertCursor(out_survey_fc, ["SHAPE@", "block_a", "block_b", "count", "str_bez_list", "fc_name", "angle_diff", "delta_v", "delta_h", "mean_v", "mean_h", "sensor"]) as ins_cursor:
                
                join_pairs = []
                footprint_fc = None
                traj_fc = None
                prog_total = len(arcpy.ListFeatureClasses())
                prog_count = 0

                # Als Erstes: Überprüfen ob Datumsfeld in allen Feature-Klassen konsistent ist!
                for feature_class in arcpy.ListFeatureClasses():
                    print(f"Überprüfe Datumsfeld in FC = {feature_class}")
                    if feature_class.endswith(trajectory_namesuffix):
                        # Überprüfen ob Datumsfeld vorhanden
                        field_names = [f.name for f in arcpy.ListFields(feature_class)]
                        if f_date_field not in field_names:
                            raise ValueError(f"Datumsfeld nicht gefunden!")
                        
                        # Jeden Wert im Datumsfeld prüfen 
                        with arcpy.da.SearchCursor(feature_class, [f_date_field, "OID@"]) as cursor:
                            for row in cursor:
                                value = row[0]
                                oid = row[1]
                            
                                utils.parse_date(value, oid)

                # Für den Join eine Fläche nehmen und dann die Linie (mit dem gleichen Präfix) joinen
                for feature_class_fp in arcpy.ListFeatureClasses():
                    desc_fp = arcpy.Describe(feature_class_fp)


                    # Fortschritt überwachen
                    prog_count = prog_count + 1
                    print("--------------------------------------------------------------------------------------------------------------------------------")
                    print(f"Starte Verarbeitung der Feature Klasse {feature_class_fp}")
                    print(f"Nr {prog_count} / {prog_total}")



                    if desc_fp.name.endswith(footprint_namesuffix):
                        # Zurücksetzen für jede neue FC
                        block_angles     = defaultdict(list)
                        block_mean_angle = {}
                        cross_pairs      = []
                        block_overlap_fc = {}

                        # Feature Klassen und Namenkonvention Variablen zuweisen
                        footprint_fc = feature_class_fp
                        fc_prefix = desc_fp.name.removesuffix(footprint_namesuffix)  # Name der FC's ohne Endung
                        traj_fc = fc_prefix + trajectory_namesuffix

                        # ----------------------------------------------------------
                        # Fluggeschwindigkeit neu berechnen, da in den Daten manchmal die durschn.-geschw. pro Linie und manchmal pro Mission angegeben ist
                        # Kopie der Trajektorien-FC erstellen (Original nicht verändern)
                        temp_traj = r"in_memory\temp_traj"
                        if arcpy.Exists(temp_traj):
                            arcpy.management.Delete(temp_traj)
                        arcpy.management.CopyFeatures(traj_fc, temp_traj)

                        # Fluggeschwindigkeit in der KOPIE berechnen (echte Fluglinienlänge!)
                        if speed_calc: 
                            with arcpy.da.UpdateCursor(temp_traj, [flug_v_field, line_starttime_field, line_endtime_field, "SHAPE@LENGTH"]) as cursor:
                                for row in cursor:
                                    v_val, gnsstime_a, gnsstime_e, line_length = row
                                    if gnsstime_a is None or gnsstime_e is None:
                                        print(f"Start- oder Endzeit leer in {traj_fc}")
                                        continue
                                    time_diff = gnsstime_e - gnsstime_a
                                    if time_diff > 0:
                                        row[0] = line_length / time_diff   # v = s / t [m/s]
                                        cursor.updateRow(row)
                                    else:
                                        print(f"Zeitdifferenz <= 0 in {traj_fc}")
                                        continue

                        # -------------------------------------------------------   
                        # Datentypen der Join-Felder überprüfen und ggf. ändern zu TEXT
                        # Falls feld nicht Text wird "{feld.name}_temp" geschrieben und für den join verwendet.

                        join_field_fp = arcpy.ListFields(footprint_fc, join_field)[0]
                        join_field_traj = arcpy.ListFields(temp_traj, join_field)[0]

                        join_field_fp = utils.field_type_to_text(footprint_fc, join_field_fp)  # join_field einfach überschreiben
                        join_field_traj = utils.field_type_to_text(temp_traj, join_field_traj)  # join_field einfach überschreiben

                        # -------------------------------------------------------   
                        # Join
                        # Temp Layer für Join erstellen
                        join_layer_fp = f"{fc_prefix}_footprint_lyr"
                        join_layer_traj = f"{fc_prefix}_traj_lyr"

                        utils.make_layer(footprint_fc, join_layer_fp)
                        utils.make_layer(temp_traj, join_layer_traj)

                        # Alle Felder der Linien an den Footprint joinen
                        arcpy.management.AddJoin(
                            in_layer_or_view=join_layer_fp,
                            in_field=join_field_fp,
                            join_table=join_layer_traj,
                            join_field=join_field_traj,
                            join_type="KEEP_ALL"
                        )

                        # Temp Memory-FC erstellen (falls schon exisitert, löschen)
                        temp_fc = r"in_memory\temp_footprint"
                        if arcpy.Exists(temp_fc):
                            arcpy.management.Delete(temp_fc)
                        arcpy.management.CopyFeatures(join_layer_fp, temp_fc)
                        
                        # -------------------------------------------------------
                        # Felder umbenennen / löschen
                        # Nach Join sind die Felder datensatzspezifisch -> automatischer Zugriff auf
                        # Felder ist erschwert -> deshalb vereinheitlichen

                        # Präfixe der Feldnamen nach Join dynamisch zusammensetzen
                        traj_basename = os.path.basename(temp_traj)   # "temp_traj"
                        traj_field_prefix = f"{traj_basename}_"

                        # Felder die umbenannt werden sollen: {alter Name: neuer Name}
                        if speed_calc:
                            rename_fields = {
                                traj_field_prefix + "block_id"              : "block_id",
                                traj_field_prefix + "angle"                 : "angle",
                                traj_field_prefix + flug_h_field            : flug_h_field,
                                traj_field_prefix + flug_v_field            : flug_v_field,
                                traj_field_prefix + f_date_field            : f_date_field,
                                traj_field_prefix + join_field              : join_field,
                                traj_field_prefix + sensor_name_field       : sensor_name_field,
                                traj_field_prefix + line_starttime_field    : line_starttime_field,
                                traj_field_prefix + line_endtime_field      : line_endtime_field
                            }
                        else:
                                                        rename_fields = {
                                traj_field_prefix + "block_id"              : "block_id",
                                traj_field_prefix + "angle"                 : "angle",
                                traj_field_prefix + flug_h_field            : flug_h_field,
                                traj_field_prefix + flug_v_field            : flug_v_field,
                                traj_field_prefix + f_date_field            : f_date_field,
                                traj_field_prefix + join_field              : join_field,
                                traj_field_prefix + sensor_name_field       : sensor_name_field,
                            }
                                                        
                        # Felder umbenennen
                        for old_name, new_name in rename_fields.items():
                            arcpy.management.AlterField(temp_fc, old_name, new_name)

                        # Alle nicht benötigten Felder löschen
                        # f.required schützt OID, Shape, Shape_Area, Shape_Length automatisch
                        keep_fields   = list(rename_fields.values())
                        delete_fields = [
                            f.name for f in arcpy.ListFields(temp_fc)
                            if f.name not in keep_fields and not f.required
                        ]

                        if delete_fields:
                            arcpy.management.DeleteField(temp_fc, delete_fields)
                        # -------------------------------------------------------

                        # Temp Join entfernen, da ins memory kopiert wurde -> temp_fc
                        arcpy.management.RemoveJoin(join_layer_fp)



                        # %%
                        # ----------------------------------------------------------------------------------------------------------------------
                        # Durchschnittswinkel berechnen pro Block und Blockpaare finden, welche sich räumlich überlappen
                        # ----------------------------------------------------------------------------------------------------------------------

                        print("-------------------------------------------------------")
                        print("Berechne Durchschnittswinkel pro Block und suche Blockpaare")

                        # für alle block_id die block-angles in eine Liste speichern
                        with arcpy.da.SearchCursor(temp_fc, ["block_id", "angle"]) as cursor:
                            for row in cursor:
                                block_id, angle = row
                                if block_id is None:
                                    arcpy.AddWarning("block_id ist NULL: Join-Key zwischen Linien und Flächen nicht sauber!")
                                elif block_id != -1:
                                    block_angles[block_id].append(angle)

                        # Mittelwert berechnen in dictionary
                        block_mean_angle  = {bid: sum(a) / len(a) for bid, a in block_angles.items()}

                        block_ids = list(block_mean_angle.keys())

                        # Überprüfen ob Winkel genügend ähnlich ist
                        for i in range(len(block_ids)):
                            for j in range(i + 1, len(block_ids)):  # startet immer eine id nach i. so gibt es keine doppelten paare

                                bid_a = block_ids[i]
                                bid_b = block_ids[j]
                                
                                # Ähnlichkeit Winkel überprüfen
                                if utils.check_angle_similarity(block_mean_angle[bid_a], block_mean_angle[bid_b], angle_cross_tol):
                                    continue

                                # Räumliche Überlappung prüfen -> gibt es Footprints von block 1 welche Footprints von block 2 überlappen?
                                layer_a = f"block_{bid_a}_lyr"
                                layer_b = f"block_{bid_b}_lyr"

                                utils.make_layer(temp_fc, layer_a, f"block_id = {bid_a}")
                                utils.make_layer(temp_fc, layer_b, f"block_id = {bid_b}")

                                # Überlappungen selektieren -> overlaps = layer_a mit Selektion
                                overlaps = arcpy.management.SelectLayerByLocation(
                                    in_layer=layer_a,
                                    overlap_type="INTERSECT",
                                    select_features=layer_b
                                )

                                # Wenn Anzahl selektierter Features grösser als 0 -> Overlap vorhanden -> Block-Paar schreiben
                                if int(arcpy.management.GetCount(overlaps)[0]) > 0:
                                    cross_pairs.append({
                                        "block_a"       : bid_a,
                                        "block_b"       : bid_b,
                                        "fc_prefix"     : fc_prefix
                                    })

                                # Selektion aufheben bevor Layer gelöscht wird
                                arcpy.management.SelectLayerByAttribute(layer_a, "CLEAR_SELECTION")

                        #%%
                        # ----------------------------------------------------------------------------------------------------------------------
                        # Overlaps innerhalb eines Blocks zählen
                        # ----------------------------------------------------------------------------------------------------------------------

                        print("-------------------------------------------------------")
                        print("Overlaps innerhalb eines Blocks zählen")

                        block_overlap_fc = {}  # block_id -> Pfad zur overlap_count FC im Memory
                        for pair in cross_pairs:  # cross_pairs hat Paare von Blöcken mit ähnlichen Winkeln welche sich überlappen
                            for bid in [pair["block_a"], pair["block_b"]]:

                                # wenn bereits berechnet überspringen
                                if bid in block_overlap_fc:
                                    continue
                                
                                # Layer aus temp_fc erstellen
                                layer_block = f"block_{bid}_lyr"
                                utils.make_layer(temp_fc, layer_block, f"block_id = {bid}")

                                # Output in in_memory
                                out_fc = rf"in_memory\overlap_block_{bid}"
                                if arcpy.Exists(out_fc):
                                    arcpy.management.Delete(out_fc)
                                
                                # Count Overlapping Features
                                arcpy.analysis.CountOverlappingFeatures(  # Erzeugt neues Set an Polygonen -> Gegenseitige Überlappungen innerhalb eines Blocks für nächsten Schritt
                                    in_features       = layer_block,
                                    out_feature_class = out_fc,
                                    min_overlap_count = 2
                                )

                                block_overlap_fc[bid] = out_fc

                        #%%
                        # ----------------------------------------------------------------------------------------------------------------------
                        # Overlaps zwischen verpaarten Blocks überprüfen und ggf. in survey_fc schreiben
                        # ----------------------------------------------------------------------------------------------------------------------
                        print("-------------------------------------------------------")
                        print(f"Start: Overlaps zwischen verpaarten Blocks überprüfen und ggf. in survey_fc schreiben")

                        survey_counter = 0

                        # Pro Paar
                        for pair in cross_pairs:
                            print(f"Start bei {pair} von insgesamt {len(cross_pairs)} Verpaarungen")
                            
                            # Block-IDs und memory_fc mit overlap variablen zuweisen
                            bid_a = pair["block_a"]
                            bid_b = pair["block_b"]
                            overlaps_a = block_overlap_fc[bid_a]
                            overlaps_b = block_overlap_fc[bid_b]

                            # Footprint-Layer pro Block erstellen
                            layer_fp_a = f"fp_{bid_a}_lyr"
                            layer_fp_b = f"fp_{bid_b}_lyr"

                            utils.make_layer(temp_fc, layer_fp_a, f"block_id = {bid_a}")
                            utils.make_layer(temp_fc, layer_fp_b, f"block_id = {bid_b}")

                            # Footprint-Daten einmal pro Paar Laden. SelectLayerByLocation soll so umgangen werden
                            fp_data_a = [(row[0], row[1], row[2], row[3], row[4], row[5]) for row in arcpy.da.SearchCursor(layer_fp_a, [join_field, flug_h_field, flug_v_field, f_date_field, sensor_name_field, "SHAPE@"])]
                            fp_data_b = [(row[0], row[1], row[2], row[3], row[4], row[5]) for row in arcpy.da.SearchCursor(layer_fp_b, [join_field, flug_h_field, flug_v_field, f_date_field, sensor_name_field, "SHAPE@"])]

                            # Alle Polygone eines Blocks nach Anzahl Überlappung gruppieren. key: Anzahl Überlappung, value: Shape; Shape kommt von Overlap-Features
                            polys_a = defaultdict(list)
                            polys_b = defaultdict(list)

                            with arcpy.da.SearchCursor(overlaps_a, ["COUNT_", "SHAPE@"]) as cursor:
                                for row in cursor:
                                    polys_a[row[0]].append(row[1])

                            with arcpy.da.SearchCursor(overlaps_b, ["COUNT_", "SHAPE@"]) as cursor:
                                for row in cursor:
                                    polys_b[row[0]].append(row[1])
                            
                            # Polygone mit gleicher Überlappungsanzahl werden verglichen
                            for overlap_count in set(polys_a.keys()) & set(polys_b.keys()):  # Auswahl von SHAPE@'s mit gleicher Anzahl Überlappung
                                for geom_a in polys_a[overlap_count]:  # Durch jedes SHAPE@ iterieren
                                    for geom_b in polys_b[overlap_count]:

                                        if geom_a.disjoint(geom_b):
                                            continue
                                            
                                        # Überprüfung der Footprints auf Eignung als Untersuchungsgebiet. Variablen welche überprüft werden müssen, werden unabhängig des Blocks in "denselben" Topf gelegt
                                        str_bez_list = []  # Wird bspw. die str_bez aller beteiligten Linien enthalten
                                        flug_h_list  = []
                                        flug_v_list  = []
                                        date_list    = []
                                        sensor_list  = []

                                        # Welche Footprints von Block A liegen in den überlappenden Layern?
                                        for str_bez, h_val, v_val , date_val, sensor_val, geom_fp in fp_data_a:
                                            if not geom_fp.disjoint(geom_a):
                                                str_bez_list.append(str_bez)
                                                flug_h_list.append(h_val)
                                                flug_v_list.append(v_val)
                                                date_list.append(date_val)
                                                sensor_list.append(sensor_val)

                                        # Welche Footprints von Block B liegen in den überlappenden Layern?
                                        for str_bez, h_val, v_val, date_val, sensor_val, geom_fp in fp_data_b:
                                            if not geom_fp.disjoint(geom_b):
                                                str_bez_list.append(str_bez)
                                                flug_h_list.append(h_val)
                                                flug_v_list.append(v_val)
                                                date_list.append(date_val)
                                                sensor_list.append(sensor_val)
                                                                                    
                                        # Wenn Flughöhenunterschiede grösser als festgelegte Toleranz, überpringen (höchster - tiefster Wert aller am Overlap beteiligten Fluglinien)
                                        if max(flug_h_list) - min(flug_h_list) > flight_h_tol:
                                            continue

                                        # Wenn Fluggeschwindigkeitsunterschied grösser als festgelegte Toleranz, überspringen (höchster - tiefster Wert aller am Overlap beteiligten Fluglinien)
                                        if max(flug_v_list) - min(flug_v_list) > flight_v_tol:
                                            continue

                                        # Überprüfe ob Sensortyp identisch ist
                                        if sensor_test == True:
                                            if len(set(sensor_list)) > 1:
                                                continue

                                        # überprüfen ob an Verlgeichspaar beteiligte Fluglinien in verschiedenen Vegetationsperioden aufgenommen wurden
                                        if date_test == True: 

                                            # Hilfsvariablen zuweisen
                                            parsed_dates = [utils.parse_date(d) for d in date_list]  # Alle Daten umwandeln
                                            min_date = min(parsed_dates)
                                            max_date = max(parsed_dates)

                                            # Für jedes beteiligte Datum wird die Vegetationsphase bestimmt.
                                            periods = set(utils.get_leaf_class(d) for d in parsed_dates)  # durch "set" bleiben nur unterschiedliche Klassen übrig
                                            if len(periods) > 1:
                                                continue
                                            
                                            period = periods.pop()  # wenn Code bis hierher kommt hat periods genau 1 Element -> period
                                            if period in ("leaf-on", "leaf-off"):  # When leaf-on oder leaf-off -> nur innerhalb desselben Jahres vergleichen
                                                if max_date.year != min_date.year:
                                                    continue
                                            elif period in ("spring", "fall"):  # Wenn in Wachstumsperiode: nur innerhalb 3 Tagen vergleichen
                                                if abs((max_date - min_date).days) > 3:
                                                    continue

                                        survey_counter = survey_counter + 1

                                        # Intersect der beiden Überlappungsflächen generieren
                                        intersection = geom_a.intersect(geom_b, 4)  # 4 = Polygon

                                        # Untersuchungsgebiet schreiben
                                        ins_cursor.insertRow((
                                            intersection,  # Shape
                                            bid_a,
                                            bid_b,
                                            overlap_count,
                                            ", ".join(str_bez_list), # Streifenbezeichnung
                                            fc_prefix,  # FC-Name
                                            abs(block_mean_angle[bid_a]-block_mean_angle[bid_b]),  # Winkeldifferenz
                                            max(flug_v_list) - min(flug_v_list),  # Differenz Fluggeschwindigkeit
                                            max(flug_h_list) - min(flug_h_list),  # Differenz Flughöhe
                                            sum(flug_v_list)/len(flug_v_list),  # Durchschn. Fluggeschwindigkeit
                                            sum(flug_h_list)/len(flug_h_list),  # Durchschn. Fluggeschwindigkeit
                                            sensor_list[0]
                                        ))
                        print(f"{survey_counter} Untersuchungsgebiete gefunden in Feature Class {feature_class_fp}")
                        total_surveys = total_surveys + survey_counter
                    endtime = datetime.now()
                    time_diff = endtime - starttime
                    print(f"Nr {prog_count} / {prog_total}, nach {int((time_diff.total_seconds())/60)} min")

# ---------------------------------------------------------------------
# Log-File schreiben
# ---------------------------------------------------------------------

out_survey_name = os.path.basename(out_survey_fc)
log_path = os.path.join(r"A:\11_MasterThesis\01_DefStruktur\02_Data", f"02_find_crossings_log_version_{out_survey_name}.txt")

with open(log_path, "w") as log:
    log.write("=" * 50 + "\n")
    log.write("02_find_crossings.py - Parameter Log\n")
    log.write("=" * 50 + "\n")
    log.write(f"Datum/Zeit:                     {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
    log.write(f"Laufzeit:                       {int((datetime.now() - starttime).total_seconds() / 60)} min\n")
    log.write("\n--- Input ---\n")
    log.write(f"Workspace:                      {arcpy.env.workspace}\n")
    log.write(f"Datasets:                       {used_datasets if used_datasets else 'alle'}\n")
    log.write(f"Footprint-Suffix:               {footprint_namesuffix}\n")
    log.write(f"Streifen-Suffix:                {trajectory_namesuffix}\n")
    log.write(f"Join-Feld:                      {join_field}\n")
    log.write("\n--- Parameter ---\n")
    log.write(f"angle_cross:                    {angle_cross_tol}°\n")
    log.write(f"flight_H_tol:                   {flight_h_tol}m\n")
    log.write(f"flight_v_tol:                   {flight_v_tol}m/s\n")
    log.write(f"sensor_test:                    {sensor_test}\n")
    log.write(f"date_test:                      {date_test}\n")
    log.write(f"speed_calc:                     {speed_calc}\n")
    log.write("\n--- Output ---\n")
    log.write(f"survey_fc:                      {out_survey_fc}\n")
    log.write(f"Untersuchungsgebiete gefunden:  {total_surveys}")

print(f"Log geschrieben: {log_path}")

endtime = datetime.now()
time_diff = endtime - starttime
print(f"Fertig in {time_diff}")