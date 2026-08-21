# ============================================================
# 01_load_and_detect_blocks.py
# ============================================================
# Beschreibung:
#   Benachbarte und +/- parallele Linien derselben Flugkampagne werden erkannt, gruppiert und erhalten eine Block-ID
#
# Vorgehen:
#   1. Benötigte Attributfelder anlegen
#   2. Pro Fluglinie Ausrichtungswinkel und Puffer um ein mittleres Liniensegment berechnet.
#   3. Algorithmus sucht schrittweise über Puffer und Winkelverlgeich nach Linien desselben "Blocks"
#   4. Jede Gruppe mit mehr als drei Linien wird eine Block-ID und Winkel für die abs. horizontale Ausrichtung zugwiesen
#
# Input:
#   fGDB mit Trajektorien einer ALS-Befliegung (baut auf 00_copy_fc.py auf)
#
# Output:
#   Aktualisierte Feature-Klassen mit den neuen Feldern Block-ID und Winkel der abs. horizontalen Ausrichtung der Parallellinien
#
# Autor:       Mirco Ackermann
# Datum:       14.05.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

import arcpy
import utils
import os
from datetime import datetime

arcpy.env.workspace = r"A:\11_MasterThesis\01_DefStruktur\02_Data\SURVEY_AREA_COMPLETE.gdb"

# Parameter
angle_parallel  = 5  # Winkelunterschied in Grad, bei welchem zwei Flugtrajektorien noch als parallel gelten
line_buffer     = 700  # Abstand innerhalb welchem zwei Fluglinien noch zum selben Flugblock gezählt werden in Meter
used_datasets   = ["EPSG_25833"]  # Datasets in welchen alle FeatureKlassen berechnet werden sollen, wenn leer werden alle Datasets berechnet

# Datenspezifische Bezeichnungen
bid_fieldname = "block_id"  # Evtl. besteht bereits das Feld block_id -> entweder umbenennen, sonst wird das ursprüngliche Feld gelöscht und ein neues erstellt.

Traj_list = []  # Liste mit Flugtrajektorien

# Startzeit angeben
print(f"Skript Start: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
start_time = datetime.now()

# ------------------------------------------------------------------------------------------------------------
# Berechne Werte für jedes Feature, welche für die Überprüfung der Nachbarschaftsbeziehung benötigt werden
# ------------------------------------------------------------------------------------------------------------

for dataset in arcpy.ListDatasets(feature_type="Feature"):  # Durch alle FeatureDatasets iterieren
    if not used_datasets or dataset in used_datasets:  # Entweder die angegebenen oder alle Datasets in fGDB behandeln
        print("-----------------------------------------------------------------------------------------------------")
        print(f"Start in FeatureDataset {dataset}")
        with arcpy.EnvManager(workspace=os.path.join(arcpy.env.workspace, dataset)):  # Workspace zu dataset ändern
            for feature_class in arcpy.ListFeatureClasses():
                desc = arcpy.Describe(feature_class)
                if desc.shapeType == "Polyline":  # Alle Linienobjekte mit z-wert berücksichtigen

                    # Iteration durch jedes Feature um benötigte Werte in eine Liste zu schreiben
                    with arcpy.da.SearchCursor(feature_class, ["OID@", "SHAPE@"]) as cursor:
                        for row in cursor:

                            #Berechnung eines Segmentes für die Erstellung des Puffers
                            geom = row[1]
                            segment = utils.segment_from_line(geom, 0.25, 0.75)

                            # Werte schreiben
                            Traj_list.append({
                                "oid"       : row[0],
                                "geometry"  : row[1],
                                "fc"        : feature_class,
                                "angle"     : utils.calc_angle(row[1]),
                                "buffer"    : segment.buffer(line_buffer),  # Puffer um jedes feature
                                "block_id"  : None
                            })
    print(f"Puffer und Winkel für alle Feature Klassen in Dataset {dataset} berechnet")

# ------------------------------------------------------------------------------------------------------------
# Erkenne einzelne Befliegungsblöcke, mit in sich parallelen Flugtrajektorien und kennzeichne einzelne 
# Line-Features mit der block_id
# ------------------------------------------------------------------------------------------------------------
print("-----------------------------------------------------------------------------------------------------")
print(f"Starte Zuweisung zu Blocks mit parallelen Fluglinien. Toleranz Winkelunterschied: {angle_parallel}°")

# block_id intialisieren
block_id = 0

# Alle Fluglinien in Missionen gruppieren. Dies verhindert, dass alle Fluglinien miteinander verglichen werden, sondern nur
# jene innerhalb derselben Flugmission
traj_by_mission = utils.make_list_by_key(Traj_list, "fc")

# Fortschrittskontrolle intialisieren
prog_total = len(Traj_list)
prog_done = 0

for traj in Traj_list:

    # Alle 100 Linien Fortschritt ausgeben
    prog_done = prog_done + 1
    if prog_done % 100 == 0:
        print(f"{prog_done}/{prog_total} Linien verarbeitet")

    # Falls Flugtrajektorie schon einem Block zugewiesen, wird sie übersprungen
    if traj.get("block_id") is not None:
        continue

    # Warteschlange initialisieren
    queue = [traj]
    block = [traj]
    block_start_angle = traj.get("angle")
    
    # Alle Fluglinien derselben Mission werden in same_mission_trajs gespeichert
    same_mission_trajs = traj_by_mission[traj.get("fc")]

    while queue:
        # erstes Element aus Warteschlange entfernen und in variable speichern, Puffer initialisieren
        current = queue.pop(0)
        current_buffer = current.get("buffer")

        # durch Liste potenzieller Nachbaren derselben Mission iterieren
        for neighbor in same_mission_trajs:
            # Bedingungen für Nachbarschaft definieren
            already_in_block    = neighbor in block
            similar_angle       = utils.check_angle_similarity(neighbor.get("angle"), block_start_angle, angle_parallel)
            in_buffer           = not current_buffer.disjoint(neighbor.get("geometry"))  # Linien welche nicht disjunkt vom Puffer sind
            in_existing_block   = neighbor.get("block_id") is not None

            # Bedingungen abfragen
            if not already_in_block and similar_angle and in_buffer and not in_existing_block:
                block.append(neighbor)
                queue.append(neighbor)
    
    if len(block) < 3:
        for f in block:
            f["block_id"] = -1
    else:
        for f in block:
            f["block_id"] = block_id
        block_id = block_id + 1

# ------------------------------------------------------------------------------------------------------------
# block_id in feature_class schreiben
# ------------------------------------------------------------------------------------------------------------
print("-----------------------------------------------------------------------------------------------------")
print("Schreibe block_id in Feature Klassen")
for dataset in arcpy.ListDatasets(feature_type="Feature"):  # Durch alle FeatureDatasets iterieren
    if not used_datasets or dataset in used_datasets:  # Entweder die angegebenen oder alles Datasets in fGDB behandeln
        with arcpy.EnvManager(workspace=os.path.join(arcpy.env.workspace, dataset)):  # Workspace zu dataset ändern
            for feature_class in arcpy.ListFeatureClasses():
                desc = arcpy.Describe(feature_class)

                # Alle Linienobjekte berücksichtigen
                if desc.shapeType == "Polyline":

                    # Feld "block_id" erstellen
                    utils.delete_add_field(feature_class, bid_fieldname, "LONG")
                    utils.delete_add_field(feature_class, "angle", "DOUBLE")

                    # Jede Zeile aktualisieren
                    with arcpy.da.UpdateCursor(feature_class, ["OID@", bid_fieldname, "angle"]) as cursor:
                        for row in cursor:
                            for traj in Traj_list:
                                if traj.get("oid") == row[0] and traj.get("fc") == feature_class:  # wenn dieselbe ObjectID und FC wird block_id geschrieben
                                    row[1] = traj.get("block_id")
                                    row[2] = traj.get("angle")
                                    cursor.updateRow(row)
                                    break

# Zeitangabe für Endzeit
needed_time = datetime.now() - start_time
print("-----------------------------------------------------------------------------------------------------")
print(f"Skript Ende: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}; Benötigte Zeit: {int(needed_time.total_seconds()/60)}")