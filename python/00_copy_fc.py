# ============================================================
# 00_copy_fc.py
# ============================================================
# Beschreibung:
#   Kopie von Feature-Klassen (Trajektorien und Footprints) in eine Output fGDB und Sortierung nach Koordinatensystem in
#   separaten Feature-Datasets (Als Ausgangslage standen mehrere Datensätze mit mehreren Koordinatensystemen zur Verfügung)
#
# Vorgehen:
#   1. Pro EPSG-Code der Feature-Klasse eine Dataset erstellen
#   3. Kampagnen ohne rekonstruierbare Flughöhe überspringen.
#   4. Feature-Klasse in das passende Dataset kopieren
#   5. Geometrien prüfen (CheckGeometry) und ggf. reparieren (RepairGeometry).
#
# Input:
#   Trajektorien und Footprints von ALS-Erhebungskampagne
#
# Output:
#   fGDB mit Dataset pro EPSG-Code und geprüften Trajektorien und Footprints
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
import os
from collections import defaultdict
from datetime import datetime

# Fortschritt überwachen
starttime = datetime.now()
print(f"Starte um {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

arcpy.env.workspace = r"A:\11_MasterThesis\MT_Datenbearbeitung\ALS_DATA_ATOST.gdb"
output_workspace = r"A:\11_MasterThesis\01_DefStruktur\02_Data\SURVEY_AREA_COMPLETE.gdb"
ohne_z = ("A2024550_FLUG_", "A2024160_FLUG_", "A2021150_FLUG_", "A2023150_FLUG_")  # alle Linien bei welchen die Grundlagendaten nicht ausreichen um die Flughöhe korrekt zu rekonstruieren

# FC-Suffixe die verarbeitet werden; SUFFIX: shapeType
fc_types = {
    "Streifen" : "Polyline",
    "Deckung"  : "Polygon"
}

for feature_class in arcpy.ListFeatureClasses():
    desc = arcpy.Describe(feature_class)

    # Nur die in fc_types definierten FeatureKlassen kopieren
    fc_suffix = None
    for suffix, geom_type in fc_types.items():
        if feature_class.endswith(suffix) and desc.shapeType == geom_type:
            fc_suffix = suffix
            break
    if fc_suffix is None:
        continue  # nächste Feature Klasse aus for-Schleife
    
    
    # EPSG-Codes ermitteln
    epsg_code = desc.spatialReference.factoryCode
    dataset_name = f"EPSG_{epsg_code}"

    # Feature Dataset erstellen falls noch nicht vorhanden
    ds_path = os.path.join(output_workspace, dataset_name)
    if not arcpy.Exists(ds_path):
        arcpy.management.CreateFeatureDataset(output_workspace, dataset_name, desc.spatialReference)
        print(f"Dataset {dataset_name} erstellt")
    
    # Feature Klassen bei welchen die Flughöhe ermittelt werden kann in output_workspace löschen falls vorhanden und erneut hineinkopieren
    output_fc = os.path.join(ds_path, feature_class)

    # Kampagnen ohne rekonstruierbare Flughöhe auslassen
    if feature_class.startswith(ohne_z):
        continue
    
    # Feature Klassen in output_workspace löschen falls vorhanden und erneut hineinkopieren
    if arcpy.Exists(output_fc):
        arcpy.management.Delete(output_fc)
    arcpy.management.CopyFeatures(feature_class, output_fc)

    # Check und Repair Geometries
    check_output = r"in_memory\geometry_check"
    if arcpy.Exists(check_output):
        arcpy.management.Delete(check_output)
    arcpy.management.CheckGeometry(output_fc, check_output)
    err_count = int(arcpy.management.GetCount(check_output)[0])
    if err_count > 0:
        with arcpy.da.SearchCursor(check_output, ["FEATURE_ID", "PROBLEM"]) as cursor:
            error_dict = defaultdict(int)
            for row in cursor:
                error_dict[row[1]] += 1
            for problem, count in error_dict.items():
                print (f"{feature_class} hat {count} fehlerhaft Features -> {problem}")
        print("Repariere Geometrien")
        arcpy.management.RepairGeometry(output_fc)
    else:
        print(f"{feature_class}: Geometrien i.O., Kopiert in fGDB")
    

endtime = datetime.now()
timediff = endtime - starttime
print(f"Fertig um {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} nach {int(timediff.total_seconds()/60)} min")