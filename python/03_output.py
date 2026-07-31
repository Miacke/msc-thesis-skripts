import arcpy
import os
import csv

arcpy.env.workspace = r"A:\11_MasterThesis\01_DefStruktur\02_Data\SURVEY_AREA_COMPLETE.gdb"
used_dataset   = "EPSG_25833"  # Datasetname - Nur 1 Dataset pro durchlauf
min_area = 10000 # minimale Fläche, welche als Survey_area dient
kamp_name = "fc_name"  # Name der Flugkampagne (Input Feature Class der vorherigen Skripts)
str_name = "str_bez_list"

output_csv = os.path.join(os.path.dirname(arcpy.env.workspace), f"survey_areas_{used_dataset}.csv")
str_list = []

# Dataset anwählen und temp. Workspace setzen
print(f"Start in FeatureDataset {used_dataset}")
with arcpy.EnvManager(workspace=os.path.join(arcpy.env.workspace, used_dataset)):
    survey_area_fc = f"survey_area_{used_dataset}"
    with open(output_csv, "w", newline="", encoding="utf-8") as csvfile:
        
        # writer-Objekt initialisieren
        writer = csv.writer(csvfile, delimiter=";")

        # Header schreiben
        writer.writerow(["Flug", "Streifen"])

        with arcpy.da.SearchCursor(survey_area_fc, [str_name, kamp_name, "SHAPE@AREA"]) as cursor:
            for row in cursor:
                str_bez_list = row[0]
                fc_name      = row[1]
                area         = row[2]

                # Mindestfläche prüfen
                if area < min_area:
                    continue

                # STR_BEZ sind kommasepariert → aufteilen
                for str_bez in str_bez_list.split(","):
                    str_bez = str_bez.strip()
                    
                    # Streifennamen nicht doppelt schreiben
                    if str_bez in str_list:
                        continue
                    str_list.append(str_bez)

                    # Zeile schreiben
                    writer.writerow([fc_name, str_bez.strip()])