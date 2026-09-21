# ============================================================
# 03_calculate_metrics.R
# ============================================================
# Beschreibung:
#   Füllt Untersuchungsgebiete mit Rasterzellen, Berechnet ob eine Rasterzelle ein Punkt enthält. Gibt data.frame aus mit Geometrie und spalten bei welcher Linienkombination ein Bodenpunkt getroffen wurde und bei welcher nicht.
#
# Input:
#   - Untersuchungsgebiete
#   - Pfade der LAS-Files
#
# Output:
#   - Rasterzellen innerhalb Untersuchungsgebieten mit Information ob Bodenpunkt vorhanden oder nicht
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
rm(list = ls())
gc()
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

# =============================================================
# Funktionen
# =============================================================

calc_ground_hits <- function(las, grid, column_prefix){
  # Berechnet die Penetrationsrate zum Boden pro Zelle eines grids und gibt das grid zurück, braucht spalten-Präfix um die einzelnen Flugrichtungen auseinanderhalten zu können
  
  # Bodenpunkte filtern
  nlas_gp <- filter_ground(las)
  
  # Punkte zählen
  pm_gp <- polygon_metrics(nlas_gp, ~length(Z), geometry = grid)  # Zähle Bodenpunkte
  pm_gp$V1[is.na(pm_gp$V1)] <- 0
  
  # in binäre Daten umwandeln -> Wenn Anzahl Bodenpunkte > 0 dann: TRUE
  grid[[paste0(column_prefix, "_groundhit")]] <- pm_gp$V1 > 0
  
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
    print(paste0("Ordner ", file.path(laz_parent_folder, i), " existiert nicht. Berechnungen für dieses Gebiet übersprungen"))
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
  # Auswahl Fluglinien und Berechnung der Metriken
  # =============================================================
  
  # -------------------------------------------------------------
  # Für Parallelflüge
  # Pfade für beide Ausrichtungen setzen
  dir_1_path <- file.path(laz_parent_folder, i, "Richtung_1")
  dir_2_path <- file.path(laz_parent_folder, i, "Richtung_2")
  
  direction_1_path <- list.files(dir_1_path, full.names = TRUE, pattern = "\\.laz$")
  direction_2_path <- list.files(dir_2_path, full.names = TRUE, pattern = "\\.laz$")
  
  # laz-files einlesen
  nlaz_parallel_1 <- readALSLAS(direction_1_path)
  nlaz_parallel_2 <- readALSLAS(direction_2_path)
  
  # Ground Hits Parallel 1 berechnen
  grid_02 <- calc_ground_hits(nlaz_parallel_1, grid_02, "parallel_1")
  grid_05 <- calc_ground_hits(nlaz_parallel_1, grid_05, "parallel_1")
  grid_1 <- calc_ground_hits(nlaz_parallel_1, grid_1, "parallel_1")
  
  # Ground Hits Parallel 2 berechnen
  grid_02 <- calc_ground_hits(nlaz_parallel_2, grid_02, "parallel_2")
  grid_05 <- calc_ground_hits(nlaz_parallel_2, grid_05, "parallel_2")
  grid_1 <- calc_ground_hits(nlaz_parallel_2, grid_1, "parallel_2")
  
  print("...ground hits für Parallelflüge berechnet")
  
  # Memory freigeben
  rm(nlaz_parallel_1,nlaz_parallel_2)
  gc()
  
  # -------------------------------------------------------------
  # Für Kreuzflüge mit 2 Linien pro Richtung (jede Kombination 1 mal)
  if (length(direction_1_path) == 2){
    counter_path_1 <- 0
    for (path_1 in direction_1_path){
      counter_path_1 <- counter_path_1 + 1
      counter_path_2 <- 0
      for (path_2 in direction_2_path){
        counter_path_2 <- counter_path_2 + 1
        
        cross <- c(path_1, path_2)  # Kreuzpaar zusammenstellen
        
        nlaz_cross <- readALSLAS(cross)  # las-files einlesen
        
        # Ground Hits Kreuz berechnen
        grid_02 <- calc_ground_hits(nlaz_cross, grid_02, paste0("kreuz_",counter_path_1,"_",counter_path_2))
        grid_05 <- calc_ground_hits(nlaz_cross, grid_05, paste0("kreuz_",counter_path_1,"_",counter_path_2))
        grid_1 <- calc_ground_hits(nlaz_cross, grid_1, paste0("kreuz_",counter_path_1,"_",counter_path_2))
      }
    }
  }
  
  # Memory freigeben
  
  
  # Für Kreuzflüge mit 4 Linien pro Richtung (jede Kombination 1 mal)
  if (length(direction_1_path) == 4){
    
    berechnet <- c()  # Vektor für bereits berechnete Kombinationen (Linie 1/2 == Linie 2/2)
    
    for (a1 in seq_along(direction_1_path)){
      for (a2 in seq_along(direction_1_path)){
        
        if (a1 == a2){  # gleiche Linien ausschliessen
          next
        }
        
        for (b1 in seq_along(direction_2_path)){
          for (b2 in seq_along(direction_2_path)){
            
            if (b1 == b2){  # gleiche Linien ausschliessen
              next
            }
            
            
            # prüfen ob Kombination bereits berechnet
            schluessel <- paste0(paste(sort(c(a1, a2)), collapse = ""), "_",
                                 paste(sort(c(b1, b2)), collapse = ""))
            
            if (schluessel %in% berechnet){  # Wenn Kombination bereits berechnet, ausschliessen
              next
            }
            berechnet <- c(berechnet, schluessel)
            
            cross <- c(direction_1_path[c(a1, a2)], direction_2_path[c(b1, b2)])  # Kreuzpaar zusammenstellen
            
            nlaz_cross <- readALSLAS(cross)  # las-files einlesen
            
            # Ground Hits Kreuz berechnen
            grid_02 <- calc_ground_hits(nlaz_cross, grid_02, paste0("kreuz_",schluessel))
            grid_05 <- calc_ground_hits(nlaz_cross, grid_05, paste0("kreuz_",schluessel))
            grid_1 <- calc_ground_hits(nlaz_cross, grid_1, paste0("kreuz_",schluessel))
          }
        }
      }
    }
  }
  
  # Memory freigeben
  rm(nlaz_cross)
  gc()
  
  print("...ground hits für Kreuzflüge berechnet")
  
  # =============================================================
  # Output pro Zellengrösse in data.frame schreiben
  # =============================================================
  
  # output file als data.frame speichern
  save(grid_02, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", paste0(i, "_grid_02.Rda")))
  save(grid_05, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", paste0(i, "_grid_05.Rda")))
  save(grid_1, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)", paste0(i, "_grid_1.Rda")))
  
  # Memory freigeben
  rm(grid_02, grid_05, grid_1, grid_02_sfc, grid_05_sfc, grid_1_sfc, grid_02_sfc_selected, grid_05_sfc_selected, grid_1_sfc_selected)
  gc()
  
  end_time_loop <- now()
  print(paste0("...fertig mit Gebiet Nr. ", i, " in ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min"))
  
}

end_time <- now()
print(paste0("Endzeit: ", end_time))
print(paste0("Benötigte Zeit: ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " min"))
