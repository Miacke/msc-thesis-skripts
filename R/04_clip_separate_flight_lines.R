# ============================================================
# 03_clip_separate_flight_lines.R
# ============================================================
# Beschreibung:
#   Schneidet pro .laz-file (welche jeweils eine Fluglinie repräsentieren), die survey_area aus
#
# Input:
#   - Untersuchungsgebiete
#   - ordner mit laz-files
#
# Output:
#   - auf Untersuchungsgebiete zugeschnittene laz-files
#
# To do:
#   - zweimal fast identische Schleife in Loop überführen
#
# Autor:       Mirco Ackermann
# Datum:       18.08.2026
# Projekt:     UNIGIS MasterThesis
# ============================================================

library(sf)
library(dplyr)
library(lidR)

# ------------------------------------------------------------
# Pfade setzen und files einlesen
# ------------------------------------------------------------

# laz-files
laz_folder_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data)"

# Fluglinienzuordnung
flight_line_table_path <- r"(A:\11_MasterThesis\01_DefStruktur\03_Arbeitsunterlagen\Fluglinien_pro_Untersuchungsgebiet.csv)"
flight_line_table <- read.csv(flight_line_table_path, sep = ";")

# Untersuchungsgebiet
survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\06_GIS\MasterThesis_Datenanalyse\MasterThesis_Datenanalyse.gdb)"
survey_area_fcname <- r"(survey_area_WALD_MANUELL)"
survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# ------------------------------------------------------------
# Fluglinienzuordnung in Liste speichern
# ------------------------------------------------------------

flight_line_table_lists <- flight_line_table |> 
  group_by(Survey_area_ObjID) |> 
  summarize(richtung_1_list = list(Richtung_1), richtung_2_list = list(Richtung_2))

# ------------------------------------------------------------
# Fluglinien mit richtiger survey area clippen, normalisieren und speichern
# ------------------------------------------------------------

laz_files <- list.files(path = laz_folder_path, pattern = r"(\.laz$)", recursive = TRUE, full.names = TRUE)

for (sa in flight_line_table_lists$Survey_area_ObjID) {  # Jede Survey_area einzeln behandeln
  print(paste0("Untersuchungsgebiet ",sa," von 37"))
  for (fl_richtung_1 in unlist(filter(flight_line_table_lists, Survey_area_ObjID == sa)$richtung_1_list)){  # durch jedes Fluglinienkürzel der Richtung_1 in der gewählten survey_area iterieren
    print("richtung 1")
    # Leere Kürzel abfangen
    if (fl_richtung_1 == ""){
      print(paste0("Fluglinienkürzel bei Survey_area Nr. ", sa, " ist leer. Linie ausgelassen"))
      next
    }
    
    special_fl <- (grep(fl_richtung_1, laz_files, value = TRUE))  # Pfad des laz-files der Fluglinie
    
    # Fluglinienkürzel muss eindeutig sein
    if (length(special_fl) != 1){
      print("Fehler: Fluglinien-kürzel in Tabelle Fluglinien_pro_Untersuchungsgebiet.csv sind nicht eindeutig!")
      stop()
    }
      
    special_sa <- filter(survey_area, Nummer_Untersuchungsgebiet == sa)  # sf-Objekt der gewählten survey_area
    
    # Tabelle nicht aktuell, deshalb fehlende survey_areas überspringen
    if (nrow(special_sa) == 0) {
      print(paste0("Untersuchungsgebiet ", sa, " fehlt in der fGDB. Übersprungen."))
      next
    }
    
    special_sa_buffered <- st_buffer(special_sa, dist = 30)  # Normalisierung weist Randprobleme auf (extrapolation), deshalb buffer
    print("laz einlesen und mit Puffer clippen...")
    laz <- readLAScatalog(special_fl)  # entsprechendes laz-file einlesen
    laz_clipped <- clip_roi(laz, special_sa_buffered)  # laz-file clippen
    
    # Ausgabe ordner anlegen
    folder <- file.path(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_clipped)",sa,"Richtung_1")
    if (!dir.exists(folder)){
      dir.create(folder, recursive = TRUE)
    }
    
    # laz_clipped mit Puffer normalisieren und wieder auf Ursprungsgrösse clippen
    print("laz normalisieren und auf Ursprungsgrösse zurückschneiden")
    lazn_clipped <- normalize_height(laz_clipped, knnidw())
    lazn_clipped <- clip_roi(lazn_clipped, special_sa)
    
    # laz_clipped schreiben
    writeLAS(lazn_clipped, file.path(folder, paste0("clipped_normalized_", basename(special_fl))))
    print("file geschrieben")
    
  }
  
  for (fl_richtung_2 in unlist(filter(flight_line_table_lists, Survey_area_ObjID == sa)$richtung_2_list)){  # durch jedes Fluglinienkürzel der Richtung_2 in der gewählten survey_area iterieren
    print("richtung 2")
    # Leere Kürzel abfangen
    if (fl_richtung_2 == ""){
      print(paste0("Fluglinienkürzel bei Survey_area Nr. ", sa, " ist leer. Linie ausgelassen"))
      next
    }
    
    special_fl <- (grep(fl_richtung_2, laz_files, value = TRUE))  # Pfad des laz-files der Fluglinie
    
    # Fluglinienkürzel muss eindeutig sein
    if (length(special_fl) != 1){
      print("Fehler: Fluglinien-kürzel in Tabelle Fluglinien_pro_Untersuchungsgebiet.csv sind nicht eindeutig!")
      stop()
    }
    
    special_sa <- filter(survey_area, Nummer_Untersuchungsgebiet == sa)  # sf-Objekt der gewählten survey_area
    
    # Tabelle nicht aktuell, deshalb fehlende survey_areas überspringen
    if (nrow(special_sa) == 0) {
      print(paste0("Untersuchungsgebiet ", sa, " fehlt in der fGDB. Übersprungen."))
      next
    }
    
    special_sa_buffered <- st_buffer(special_sa, dist = 30)  # Normalisierung weist Randprobleme auf (extrapolation), deshalb buffer
    print("laz einlesen und mit Puffer clippen...")
    laz <- readLAScatalog(special_fl)  # entsprechendes laz-file einlesen
    laz_clipped <- clip_roi(laz, special_sa_buffered)  # laz-file clippen
    
    # Ausgabe ordner anlegen
    folder <- file.path(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_clipped)",sa,"Richtung_2")
    if (!dir.exists(folder)){
      dir.create(folder, recursive = TRUE)
    }
    
    # laz_clipped mit Puffer normalisieren und wieder auf Ursprungsgrösse clippen
    print("laz normalisieren und auf Ursprungsgrösse zurückschneiden")
    lazn_clipped <- normalize_height(laz_clipped, knnidw())
    lazn_clipped <- clip_roi(lazn_clipped, special_sa)
    
    # laz_clipped schreiben
    writeLAS(lazn_clipped, file.path(folder, paste0("clipped_normalized_", basename(special_fl))))
    print("file geschrieben")
    
  }
  
}



