# ============================================================
# 02_clip_and_normalize.R
# ============================================================
# Beschreibung:
#   - Schneidet pro gepuffertem Untersuchungsgebiet die überlappenden .laz-files aus
#   - Löscht die Klassifizierung und reklassifiziert neu
#   - Normalisiert die Höhe in den ausgeschnittenen laz-files
#   - Schneidet die normalisierten Höhen auf das ursprüngliche Untersuchungsgebiet
#   - Schreibt die für die Auswertung benötigten Fluglinien nach Ausrichtung separiert als .las-file ab
#
# Input:
#   - Untersuchungsgebiete
#   - ordner mit laz-files
#
# Output:
#   - auf Untersuchungsgebiete zugeschnittene und normalisierte laz-files
#
# Autor:       Mirco Ackermann
# Datum:       19.08.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

library(lidR)
library(sf)
library(future)
library(lubridate)
library(dplyr)

start_time <- now()
print(paste0("Start: ", start_time))

# ============================================================
# Parallelisierung und andere Optionen
# ============================================================

plan(multisession, workers = 4)  # max. 3 workers
options(lidR.progress = FALSE)

# ============================================================
# Pfade setzen
# ============================================================

# laz-files
laz_parent_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data)"
laz_files <- list.files(laz_parent_folder, pattern = r"(\.laz$)", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

# Fluglinienzuordnung
flight_line_table_path <- r"(A:\11_MasterThesis\01_DefStruktur\03_Arbeitsunterlagen\Fluglinien_pro_Untersuchungsgebiet.csv)"
flight_line_table <- read.csv(flight_line_table_path, sep = ";", colClasses="character")

# Untersuchungsgebiet
survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\06_GIS\MasterThesis_Datenanalyse\MasterThesis_Datenanalyse.gdb)"
survey_area_fcname <- r"(survey_area_WALD_MANUELL)"
survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# Zielordner für Clips pro Untersuchungsgebiet
output_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_ClipROI_Normalized)"

# ============================================================
# Funktionen
# ============================================================

write_flightlines <- function(ps_ids, dir_vector, output_folder, sa_number, direction_string, las_file){
  
  # Prüft ob PointSourceID der geclippten und ausgeschnittenen für die Auswertung benötigt wird. Falls ja wird das File als .laz in einem ggf. noch zu erstellenden Ordner abgespeichert.
  # Achtung: dir_vector darf nur Fluglinien mit unterschiedlicher integer_pointsourceID enthalten!
  
  for (id in ps_ids){  # alle vorhandenen PointSourceID's im nlaz_clip-file durchiterieren
    
    # Suchmuster initialisieren
    pattern <- paste0(id, "_")
    if (any(grepl(pattern, dir_vector))) {  # falls id für Auswertung benötigt wird
      
      # Ordner erstellen, falls dieser noch nicht existieret
      folder <- file.path(output_folder, sa_number, direction_string)
      ifelse(!dir.exists(folder), dir.create(folder, recursive=TRUE), "Directory Exists")
      file_ending <- paste0(grep(pattern,dir_vector, value=TRUE), ".laz")
      
      # LAS-Punkte filtern
      fl <- filter_poi(las_file, PointSourceID == id)
      writeLAS(fl, file.path(folder, file_ending))
    }
  }
}

# ============================================================
# Untersuchungsgebiete Puffern
# (verhindert Randeffekte bei der Normalisierung)
# ============================================================

survey_area_buffered <- st_buffer(survey_area, dist = 50)  # Puffer von 50m

# ============================================================
# Clip, Reclassify and Normalize
# ============================================================

las_ctg <- readLAScatalog(laz_files)  # Laz-files als las-catalog einlesen
opt_progress(las_ctg) <- FALSE

# Prüfen ob Untersuchungsgebiet-ID eindeutig ist
if (nrow(survey_area_buffered) != length(unique(survey_area_buffered$Nummer_Untersuchungsgebiet))){
  stop("Nummer_Untersuchungsgebiet nicht eindeutig. Programmstopp")
}

for (i in survey_area_buffered$Nummer_Untersuchungsgebiet){  # for schleife iteriert durch spalten, deshalb zuerst alle Nummer_Untersuchungsgebiete iterieren und danach sf-Objekt mit index aufrufen
  start_time_loop <- now()
  cat(paste0("Start bei Untersuchungsgebiet Nr. ", i, "..."))
  sa <- filter(survey_area_buffered, Nummer_Untersuchungsgebiet == i)
  
  # # Alle Ergebnisse auf Festplatte speichern um RAM zu schonen
  # opt_output_files(las_ctg) <- file.path(output_folder, i)
  
  # Gepuffertes Untersuchungsgebiet ausschneiden
  laz_bufferedclip <- clip_roi(las_ctg, sa)
  cat("...geclippt ")
  
  # Klassifizierung löschen
  laz_bufferedclip$Classification <- 0L
  
  # Boden und Noise neu Klassifizieren
  # Bodenklassifizierung: 
  #   - res = Zellengrösse um Seed-Punkte zu finden. 20 empfohlen für Wald
  #   - ptd() kann mit outliern umgehen, weshalb Noise-Klassifizierung erst nach der Bodenklassifikation ausgeführt wird
  #   - angle/distance/spacing wurden beim Standard (30,2,0.25) belassen
  laz_bufferedclip <- classify_ground(laz_bufferedclip, ptd(res = 20))
  cat("...Boden klassifiziert")
  
  # Noise klassifizieren und herausfiltern
  laz_bufferedclip <- classify_noise(laz_bufferedclip, sor())
  laz_bufferedclip <- filter_poi(laz_bufferedclip, Classification != 18L & Classification != 7L)
  cat("...Noise klassifiziert und gefiltert")
  
  # Normalisieren
  nlaz_bufferedclip <- normalize_height(laz_bufferedclip, knnidw())
  cat("...Höhe normalisiert")
  
  # Normalisiertes laz-file zurückschneiden
  nlaz_clip <- clip_roi(nlaz_bufferedclip, filter(survey_area, Nummer_Untersuchungsgebiet==i))
  
# ============================================================
# Separate Flight Lines
# ============================================================  
  
  # Point Source ID's auslesen
  ps_ids <- unique(nlaz_clip$PointSourceID)
  
  # Benötigte Fluglinien pro Richtung anhand flight_line_table auslesen
  direction_1 <- filter(flight_line_table, Survey_area_ObjID==i)$Richtung_1
  direction_2 <- filter(flight_line_table, Survey_area_ObjID==i)$Richtung_2
  
  # Für die Auswertung benötigte Fluglinien in separaten Ordnern abspeichern
  write_flightlines(ps_ids, direction_1, output_folder, i, "Richtung_1", nlaz_clip)
  write_flightlines(ps_ids, direction_2, output_folder, i, "Richtung_2", nlaz_clip)

  
  end_time_loop <- now()
  cat(paste0("...fertig mit Gebiet ", i, ": ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min\n"))
}

end_time <- now()
print(paste0("Endzeit: ", end_time))
print(paste0("Benötigte Zeit: ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " min"))