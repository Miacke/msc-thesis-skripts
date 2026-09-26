# ============================================================
# 02_separate_flightlines.R
# ============================================================
# Beschreibung:
#   - Schreibt die für die Auswertung benötigten Fluglinien nach Ausrichtung separiert als .las-file ab
#
# Input:
#   - Untersuchungsgebiete
#   - ordner mit laz-files
#
# Output:
#   - separierte fluglinien pro Untersuchungsgebiet und Flugrichtung
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

plan(multisession, workers = 3)  # max. 3 workers
options(lidR.progress = FALSE)

# ============================================================
# Pfade setzen
# ============================================================

# laz-files
laz_parent_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_01)"
laz_files <- list.files(laz_parent_folder, pattern = r"(\.laz$)", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

# Untersuchungsgebiet
survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\02_Survey_areas\SURVEY_AREA_MSCTHESIS.gdb)"
survey_area_fcname <- r"(survey_area_final)"
survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# Zielordner für Clips pro Untersuchungsgebiet
output_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_02)"

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
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)  # dir.create überschreibt nicht
      file_ending <- paste0(grep(pattern,dir_vector, value=TRUE), ".laz")
      
      # LAS-Punkte filtern
      fl <- filter_poi(las_file, PointSourceID == id)
      writeLAS(fl, file.path(folder, file_ending))
    }
  }
}

# ============================================================
# Separate Flight Lines
# ============================================================  

for (laz_file in laz_files){
  start_time_loop <- now()
  
  # Nummer des Untersuchungsgebietes aus laz-file-pfad auslesen
  filename_parts <- strsplit(basename(laz_file), "_")[[1]]  # unterteilt den filenamen (nicht den ganzen pfad) in einzelne Teile welche durch "_" getrennt sind
  i <- strsplit(filename_parts[length(filename_parts)], "\\.")[[1]][1]  # nimmt den letzten teil, separiert nach "." und gibt von dieser liste den ersten vektor und von dort den ersten teil aus (= Untersuchungsgebiet)
  print(paste("Start in Untersuchungsgebiet Nr.", i))
  
  # laz-file einlesen
  laz <- readLAS(laz_file)
  
  # survey area auswählen
  sa <- dplyr::filter(survey_area, SA_Nr == i)
  
  # Point Source ID's auslesen
  ps_ids <- unique(laz$PointSourceID)
  
  # Benötigte Fluglinien pro Richtung anhand flight_line_table auslesen
  direction_1 <- strsplit(sa$strbez_dir1, ",")[[1]]
  direction_2 <- strsplit(sa$strbez_dir2, ",")[[1]]
  
  # Für die Auswertung benötigte Fluglinien in separaten Ordnern abspeichern
  write_flightlines(ps_ids, direction_1, output_folder, i, "Richtung_1", laz)
  write_flightlines(ps_ids, direction_2, output_folder, i, "Richtung_2", laz)

  
  end_time_loop <- now()
  cat(paste0("...fertig mit Gebiet ", i, ": ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min\n"))
}

end_time <- now()
print(paste0("Endzeit: ", end_time))
print(paste0("Benötigte Zeit: ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " min"))