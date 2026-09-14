# ============================================================
# 03_calculate_metrics.R
# ============================================================
# Beschreibung:
#   Füllt Untersuchungsgebiete mit Rasterzellen
#
# Input:
#   - Untersuchungsgebiete
#
# Output:
#   - Rasterzellen innerhalb Untersuchungsgebieten
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

library(sf)
library(spatialEco)
library(lidR)
library(lubridate)
library(dplyr)

options(lidR.progress = FALSE)
start_time <- now()
print(paste0("Start: ", start_time))

# =============================================================
# pfade setzen
# =============================================================

# Untersuchungsgebiete

survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\06_GIS\MasterThesis_Datenanalyse\MasterThesis_Datenanalyse.gdb)"
survey_area_fcname <- r"(survey_area_WALD_HUELLE_MANUELL)"
survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# LAZ-Files

laz_parent_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_ClipROI_Normalized)"

# dataframe - output

grid_02_completedata <- data.frame()
grid_05_completedata <- data.frame()
grid_1_completedata <- data.frame()

# =============================================================
# Funktionen
# =============================================================

calc_hmax <- function(las, grid, tile_size){
  # Berechnet hmax pro Zelle eines grids und gibt das grid zurück, hmax ist dabei das 99% Höhenperzentil
  
  pm_hmax <- polygon_metrics(las, ~quantile(Z, probs = 0.99),geometry = grid)  # Berechne h_99 perzentil
  grid[[paste0("hmax_", tile_size)]] <- pm_hmax$V1  # schreibe in Spalte
  
  return(grid)
}

calc_ground_penetration <- function(las, grid, column_prefix){
  # Berechnet die Penetrationsrate zum Boden pro Zelle eines grids und gibt das grid zurück, braucht spalten-Präfix um die einzelnen Flugrichtungen auseinanderhalten zu können
  
  # Punkte filtern
  nlas_gp <- filter_ground(las)  # Nur Bodenpunkte 
  nlas_lr <- filter_last(las)  # Nur Last Returns
  
  # Punkte zählen
  pm_gp <- polygon_metrics(nlas_gp, ~length(Z), geometry = grid)  # Zähle Bodenpunkte
  pm_lr <- polygon_metrics(nlas_lr, ~length(Z), geometry = grid)  # Zähle alle last returns
  
  # Bodenpenetrationsrate berechnen
  
  grid[[paste0(column_prefix, "_gp_rate")]] <- pm_gp$V1/pm_lr$V1
  
  return(grid)
}

calc_understory_penetration <- function(las, grid, hmax_attribute, column_prefix){
  # Berechnet die Penetrationsrate in den unteren Kronenbereich pro Zelle eines grids und gibt das grid zurück
  # Braucht hmax_attribute, die Spalte wo im las-file der hmax-wert für das entsprechende grid gespeichert ist. Ausserdem noch spalten-Präfix (siehe calc_ground_penetration)
  
  # Punkte filtern
  nlas_lr <- filter_last(las)  # Nur Last Returns
  pm_lr <- polygon_metrics(nlas_lr, ~length(Z), geometry = grid)  # Gesamtanzahl der last returns
  
  # hmax muss in eine fest benannte Spalte im LAS-File zwischengespeichert werden, da polygon_metrics den Spaltennamen nicht als variable übernehmen kann
  nlas_lr <- add_attribute(nlas_lr, nlas_lr[[hmax_attribute]], "hmax_temp")
  
  # Penetrationsrate berechnen
  pm_up <- polygon_metrics(nlas_lr, ~sum(Classification != 2L & Z < 0.3 * hmax_temp), geometry = grid)  # das query wird auf jeden Punkt angewandt und gibt eine Vektor mit TRUE/FALSE zurück. Die TRUE's werden von sum() addiert
  grid[[paste0(column_prefix, "_up_rate")]] <- pm_up$V1/pm_lr$V1
  
  return(grid)
}

# =============================================================
# Gitterzellen für Untersuchungsgebiete erstellen
# =============================================================

for (i in survey_area$Nummer_Untersuchungsgebiet){
  
  start_time_loop <- now()
  print("----------------------------------------------------------------------------------------------------------------------")
  print(paste0("Start bei Gebiet Nr.", i))
  
  # Check ob Ordner überhaupt existiert
  if (!file.exists(file.path(laz_parent_folder, i))){
    print(paste0("Ordner ", file.path(laz_parent_folder, i), "existiert nicht. Berechnungen für dieses Gebiet übersprungen"))
    next
  }
  
  # Nach Programmabbruch muss folgender Code angepasst ggf. an einer bestimmten Stelle eingesetzt werden
  # if (i != 6 & i != 7){
  #   print(paste0("Gebiet Nr.", i, " übersprungen"))
  #   next
  # }
  
  
  current_survey_area <- dplyr::filter(survey_area, Nummer_Untersuchungsgebiet == i)
  
  # Gitter bilden (ergibt sfc-objekt -> reine Geometrie, ohne Attributentabelle)
  grid_1_sfc <- st_make_grid(current_survey_area, cellsize = 1)  
  grid_05_sfc <- st_make_grid(current_survey_area, cellsize = 0.5)
  grid_02_sfc <- st_make_grid(current_survey_area, cellsize = 0.2)
  
  # grid-Zellen selektieren die sich komplett in der survey_area befinden
  grid_1_sfc_selected <- spatial.select(current_survey_area, grid_1_sfc, predicate = "contains")
  grid_05_sfc_selected <- spatial.select(current_survey_area, grid_05_sfc, predicate = "contains")
  grid_02_sfc_selected <- spatial.select(current_survey_area, grid_02_sfc, predicate = "contains")
  
  # Umwandlung der Gitterzellen in sf-Objekte
  grid_1 <- st_sf(geometry = grid_1_sfc_selected)
  grid_05 <- st_sf(geometry = grid_05_sfc_selected)
  grid_02 <- st_sf(geometry = grid_02_sfc_selected)
  
  # Untersuchungsgebiet (i) jeder Zelle als neues Attribut zuweisen
  grid_1$Nummer_Untersuchungsgebiet <- current_survey_area$Nummer_Untersuchungsgebiet
  grid_05$Nummer_Untersuchungsgebiet <- current_survey_area$Nummer_Untersuchungsgebiet
  grid_02$Nummer_Untersuchungsgebiet <- current_survey_area$Nummer_Untersuchungsgebiet
  
  # Eindeutiger Zellenidentifikator als neues Attribut zuweisen: "[Nummer_Untersuchungsgebiet]_[Forlaufende Nummerierung der Zellen]"
  grid_1$ID <- paste0(current_survey_area$Nummer_Untersuchungsgebiet, "_", seq_len(nrow(grid_1)))
  grid_05$ID <- paste0(current_survey_area$Nummer_Untersuchungsgebiet, "_", seq_len(nrow(grid_05)))
  grid_02$ID <- paste0(current_survey_area$Nummer_Untersuchungsgebiet, "_", seq_len(nrow(grid_02)))
  
  print("...Gitterzellen erstellt")
  
  # =============================================================
  # hmax als fixen Referenzwert aus allen verfügbaren ALS-Punkten berechnen und in der Punktwolke abspeichern
  # =============================================================
  
  # Pfade für beide Ausrichtungen setzen
  dir_1_path <- file.path(laz_parent_folder, i, "Richtung_1")
  dir_2_path <- file.path(laz_parent_folder, i, "Richtung_2")
  dir_1_filelist <- list.files(dir_1_path, full.names = TRUE, pattern = "\\.laz$")
  dir_2_filelist <- list.files(dir_2_path, full.names = TRUE, pattern = "\\.laz$")
  
  # Alle verfügbaren LAS-Files der Survey Area ins Memory einlesen
  nlaz_total <- readALSLAS(c(dir_1_filelist, dir_2_filelist))
  
  # hmax pro Grid berechnen
  grid_02 <- calc_hmax(nlaz_total, grid_02, "02")
  grid_05 <- calc_hmax(nlaz_total, grid_05, "05")
  grid_1 <- calc_hmax(nlaz_total, grid_1, "1")
  
  # Nach hmax-Kalkulation nlaz_total löschen und Memory freigeben
  rm(nlaz_total)
  gc()
  
  print("... hmax berechnet")
  
  # Um als fixer Referenzwert zur Verfügung zu stehen, wird hmax für jede grid-Grösse direkt in die pro Fluglinie separat abgelegte Punktwolke geschrieben
  # Jedes laz-file hat so pro Punkt noch die hmax-Werte des 0.1m-, 0.5m-, und 1m-Gitters gespeichert. Dies wird benötigt, weil spätere Kalkulationen nur auf Attribute
  # innerhalb der Punktwolke zugreifen können
  for (file in c(dir_1_filelist,dir_2_filelist)){
    temp_laz <- readALSLAS(file)
    temp_laz <- merge_spatial(temp_laz, grid_02, "hmax_02")
    temp_laz <- merge_spatial(temp_laz, grid_05, "hmax_05")
    temp_laz <- merge_spatial(temp_laz, grid_1, "hmax_1")
    
    # Um im laz-file verfügbar zu sein, muss das Attribut auch in den header geschrieben werden
    temp_laz <- add_lasattribute(temp_laz, name = "hmax_02", desc = "99th height percentile, 0.2m tile")
    temp_laz <- add_lasattribute(temp_laz, name = "hmax_05", desc = "99th height percentile, 0.5m tile")
    temp_laz <- add_lasattribute(temp_laz, name = "hmax_1", desc = "99th height percentile, 1m tile")
    
    # Direkt in das eingelesene File schreiben
    writeLAS(temp_laz, file)
  }
  
  # nach Kalkulation temp_laz löschen und Memory freigeben
  rm(temp_laz)
  gc()
  print("... und geschrieben")
  # =============================================================
  # Auswahl Fluglinien und Berechnung der Metriken
  # =============================================================
  
  # -------------------------------------------------------------
  # Für Parallelflüge
  # laz-files einlesen, die jetzt hmax-werte pro Punkt und Grid-grösse gespeichert
  direction_1 <- dir_1_filelist
  direction_2 <- dir_2_filelist
  nlaz_parallel_1 <- readALSLAS(direction_1)
  nlaz_parallel_2 <- readALSLAS(direction_2)
  
  # Ground Penetration Parallel 1 berechnen
  grid_02 <- calc_ground_penetration(nlaz_parallel_1, grid_02, "parallel_1")
  grid_05 <- calc_ground_penetration(nlaz_parallel_1, grid_05, "parallel_1")
  grid_1 <- calc_ground_penetration(nlaz_parallel_1, grid_1, "parallel_1")
  
  # Ground Penetration Parallel 2 berechnen
  grid_02 <- calc_ground_penetration(nlaz_parallel_2, grid_02, "parallel_2")
  grid_05 <- calc_ground_penetration(nlaz_parallel_2, grid_05, "parallel_2")
  grid_1 <- calc_ground_penetration(nlaz_parallel_2, grid_1, "parallel_2")
  
  print("...ground penetration rate für Parallelflüge berechnet")
  
  # Understory Penetration Parallel 1 berechnen
  grid_02 <- calc_understory_penetration(nlaz_parallel_1, grid_02, "hmax_02", "parallel_1")
  grid_05 <- calc_understory_penetration(nlaz_parallel_1, grid_05, "hmax_05", "parallel_1")
  grid_1 <- calc_understory_penetration(nlaz_parallel_1, grid_1, "hmax_1", "parallel_1")
  
  # Understory Penetration Parallel 2 berechnen
  grid_02 <- calc_understory_penetration(nlaz_parallel_2, grid_02, "hmax_02", "parallel_2")
  grid_05 <- calc_understory_penetration(nlaz_parallel_2, grid_05, "hmax_05", "parallel_2")
  grid_1 <- calc_understory_penetration(nlaz_parallel_2, grid_1, "hmax_1", "parallel_2")
  
  print("...understory penetration rate für Parallelflüge berechnet")
  
  # -------------------------------------------------------------
  # Für Kreuzflüge
  
  # Kreuzpaare zusammensetzen
  if (length(direction_1) == 2){
    cross_1 <- c(direction_1[1], direction_2[2])
    cross_2 <- c(direction_1[2], direction_2[1])
  } else if (length(direction_1) == 4){
    cross_1 <- c(direction_1[1], direction_1[3], direction_2[2], direction_2[4])
    cross_2 <- c(direction_1[2], direction_1[4], direction_2[1], direction_2[3])
  } else {
    print(paste0("Fehler in Untersuchungsgebiet ", i, ". Anzahl Linien nicht 2 oder 4"))
    next
  }
  
  # laz-files einlesen, die jetzt hmax-werte pro Punkt aufweisen
  nlaz_cross_1 <- readALSLAS(cross_1)
  nlaz_cross_2 <- readALSLAS(cross_2)
  
  # Ground Penetration Kreuz 1 berechnen
  grid_02 <- calc_ground_penetration(nlaz_cross_1, grid_02, "kreuz_1")
  grid_05 <- calc_ground_penetration(nlaz_cross_1, grid_05, "kreuz_1")
  grid_1 <- calc_ground_penetration(nlaz_cross_1, grid_1, "kreuz_1")
  
  # Ground Penetration Kreuz 2 berechnen
  grid_02 <- calc_ground_penetration(nlaz_cross_2, grid_02, "kreuz_2")
  grid_05 <- calc_ground_penetration(nlaz_cross_2, grid_05, "kreuz_2")
  grid_1 <- calc_ground_penetration(nlaz_cross_2, grid_1, "kreuz_2")
  
  print("...ground penetration rate für Kreuzflüge berechnet")
  
  # Understory Penetration Kreuz 1 berechnen
  grid_02 <- calc_understory_penetration(nlaz_cross_1, grid_02, "hmax_02", "kreuz_1")
  grid_05 <- calc_understory_penetration(nlaz_cross_1, grid_05, "hmax_05", "kreuz_1")
  grid_1 <- calc_understory_penetration(nlaz_cross_1, grid_1, "hmax_1", "kreuz_1")
  
  # Understory Penetration Kreuz 2 berechnen
  grid_02 <- calc_understory_penetration(nlaz_cross_2, grid_02, "hmax_02", "kreuz_2")
  grid_05 <- calc_understory_penetration(nlaz_cross_2, grid_05, "hmax_05", "kreuz_2")
  grid_1 <- calc_understory_penetration(nlaz_cross_2, grid_1, "hmax_1", "kreuz_2")
  
  print("...understory penetration rate für Kreuzflüge berechnet")
  
  # =============================================================
  # Output pro Zellengrösse in data.frame schreiben
  # =============================================================
  
  grid_02_completedata <- rbind(grid_02_completedata, grid_02)
  grid_05_completedata <- rbind(grid_05_completedata, grid_05)
  grid_1_completedata <- rbind(grid_1_completedata, grid_1)
  
  # Für den Fall eines Programmabbruchs wird zusätzlich jedes File einzeln geschrieben
  save(grid_02, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", paste0(i, "_grid_02.Rda")))
  save(grid_05, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", paste0(i, "_grid_05.Rda")))
  save(grid_1, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", paste0(i, "_grid_1.Rda")))
  
  end_time_loop <- now()
  print(paste0("...fertig mit Gebiet Nr. ", i, " in ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min"))
  
}

# =============================================================
# Dataframe auf Festplatte speichern
# =============================================================

save(grid_02_completedata,file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", "00_complete_grid_02.Rda"))
save(grid_05_completedata,file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", "00_complete_grid_05.Rda"))
save(grid_1_completedata,file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", "00_complete_grid_1.Rda"))

end_time <- now()
print(paste0("Endzeit: ", end_time))
print(paste0("Benötigte Zeit: ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " min"))
