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
survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\02_Survey_areas\SURVEY_AREA_MSCTHESIS.gdb)"
survey_area_fcname <- "survey_area_final"
survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# LAZ-Files
laz_parent_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_02)"

# Fluglinien
flightlines <- st_read(survey_area_fgdb_path, layer = "Fluglinien")

# =============================================================
# Funktionen
# =============================================================

calc_line_metrics <- function(las,grid,column_prefix,cellsize){
  
  # metriken pro polygon berechnen und in sf-Objekt (pm) speichern
  pm <- polygon_metrics(las, ~list(
    sa_min = min(ScanAngle),
    sa_max = max(ScanAngle),
    n = length(Z),
    n_gp = sum(Classification == 2L)
  ), geometry = grid)
  
  pm$n[is.na(pm$n)] <- 0  # NA zu 0
  pm$n_gp[is.na(pm$n_gp)] <- 0
  
  grid[[paste0(column_prefix, "_sa_min")]]    <- pm$sa_min
  grid[[paste0(column_prefix, "_sa_max")]]    <- pm$sa_max
  grid[[paste0(column_prefix, "_dichte")]]    <- pm$n / (cellsize^2)
  grid[[paste0(column_prefix, "_groundhit")]] <- pm$n_gp > 0
  
  return(grid)
}

# =============================================================
# Gitterzellen für Untersuchungsgebiete erstellen
# =============================================================

for (i in survey_area$SA_Nr){

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
  
  
  current_survey_area <- dplyr::filter(survey_area, SA_Nr == i)
  start_time_grid <- now()
  # Gitter bilden (ergibt sfc-objekt -> reine Geometrie, ohne Attributentabelle)
  grid_1_sfc <- st_make_grid(current_survey_area, cellsize = 1)  
  grid_05_sfc <- st_make_grid(current_survey_area, cellsize = 0.5)

  # grid-Zellen selektieren die sich komplett in der survey_area befinden
  grid_1_sfc_selected <- spatial.select(current_survey_area, grid_1_sfc, predicate = "contains")
  grid_05_sfc_selected <- spatial.select(current_survey_area, grid_05_sfc, predicate = "contains")
  
  # Umwandlung der Gitterzellen in sf-Objekte
  grid_1 <- st_sf(geometry = grid_1_sfc_selected)
  grid_05 <- st_sf(geometry = grid_05_sfc_selected)

  # Untersuchungsgebiet (i) jeder Zelle als neues Attribut zuweisen
  grid_1$SA_Nr <- current_survey_area$SA_Nr
  grid_05$SA_Nr <- current_survey_area$SA_Nr

  # Eindeutiger Zellenidentifikator als neues Attribut zuweisen: "[SA_Nr]_[Forlaufende Nummerierung der Zellen]"
  grid_1$ID <- paste0(current_survey_area$SA_Nr, "_", seq_len(nrow(grid_1)))
  grid_05$ID <- paste0(current_survey_area$SA_Nr, "_", seq_len(nrow(grid_05)))
  
  # Zellzentren einmal pro Gebiet berechnen
  zentren_05 <- st_centroid(st_geometry(grid_05))
  zentren_1 <- st_centroid(st_geometry(grid_1))

  print(paste("...Gitterzellen erstellt in", round(as.numeric(difftime(now(), start_time_grid, units = "mins")), 2), "min"))
  
  # =============================================================
  # pfade für Flugrichtungen setzen
  # =============================================================
  
  # Pfade für beide Ausrichtungen setzen
  dir_1_path <- file.path(laz_parent_folder, i, "Richtung_1")
  dir_2_path <- file.path(laz_parent_folder, i, "Richtung_2")
  
  direction_1_path <- list.files(dir_1_path, full.names = TRUE, pattern = "\\.laz$")
  direction_2_path <- list.files(dir_2_path, full.names = TRUE, pattern = "\\.laz$")
  
  # =============================================================
  # Berechnung der Metriken pro Fluglinie
  # =============================================================
  
  start_time_scanangles <- now()
  for (fl in c(direction_1_path, direction_2_path)){
    
    # las file einer einzelnen linie einlesen
    las <- readALSLAS(fl)
    
    # str_bez herauslesen (eindeutig über alle fluglinien)
    linie <- tools::file_path_sans_ext(basename(fl))
    
    grid_05 <- calc_line_metrics(las, grid_05, paste0("L", linie), 0.5)
    grid_1  <- calc_line_metrics(las, grid_1,  paste0("L", linie), 1)
    rm(las); gc()
    
    # Abstand zur Fluglinie berechnen (da einzelne Zellen keine PUnkte haben und bei Scanangles dann NA steht)
    geom_line <- flightlines[flightlines$STR_BEZ == linie, ]
    grid_05[[paste0("L", linie, "_abstand")]] <- as.numeric(st_distance(zentren_05, geom_line))
    grid_1[[paste0("L", linie, "_abstand")]]  <- as.numeric(st_distance(zentren_1,  geom_line))
    
    print(paste("... polygon-metrics berechnet für Linie", fl, "in", round(as.numeric(difftime(now(), start_time_scanangles, units = "mins")), 2), "min"))
  }
  

  # =============================================================
  # Pro Parallel und Kreuz-flug Metriken berechnen
  # =============================================================
  
  # -------------------------------------------------------------
  # Parallelflüge
  start_time_parallel <- now()
  
  # fluglinienkombinationen für Parallelflüge
  p1_fl <- paste(tools::file_path_sans_ext(basename(direction_1_path)), collapse = ",")
  p2_fl <- paste(tools::file_path_sans_ext(basename(direction_2_path)), collapse = ",")
  
  # fluglinienkombinationen für parallelflüge ins grid schreiben
  grid_05$parallel_1_fl <- p1_fl
  grid_1$parallel_1_fl  <- p1_fl
  grid_05$parallel_2_fl <- p2_fl
  grid_1$parallel_2_fl  <- p2_fl
  
  # groundhits für parallelflüge vereinigen und ins grid schreiben
  grid_05$parallel_1_groundhit <- grid_05[[paste0("L",strsplit(p1_fl, ",")[[1]][1],"_groundhit")]] | grid_05[[paste0("L",strsplit(p1_fl, ",")[[1]][2],"_groundhit")]]  # groundhits pro fluglinie sind vorhanden, diese werden nur noch zusammengezählt
  grid_05$parallel_2_groundhit <- grid_05[[paste0("L",strsplit(p2_fl, ",")[[1]][1],"_groundhit")]] | grid_05[[paste0("L",strsplit(p2_fl, ",")[[1]][2],"_groundhit")]]
  grid_1$parallel_1_groundhit <- grid_1[[paste0("L",strsplit(p1_fl, ",")[[1]][1],"_groundhit")]] | grid_1[[paste0("L",strsplit(p1_fl, ",")[[1]][2],"_groundhit")]]
  grid_1$parallel_2_groundhit <- grid_1[[paste0("L",strsplit(p2_fl, ",")[[1]][1],"_groundhit")]] | grid_1[[paste0("L",strsplit(p2_fl, ",")[[1]][2],"_groundhit")]]
  
  print(paste("Parallelflüge berechnet in", round(as.numeric(difftime(now(), start_time_parallel, units = "mins")), 2), "min"))
  
  # -------------------------------------------------------------
  # Kreuzlflüge
  start_time_cross <- now()
  counter_path_1 <- 0
  for (path_1 in direction_1_path){
    counter_path_1 <- counter_path_1 + 1
    counter_path_2 <- 0
    for (path_2 in direction_2_path){
      counter_path_2 <- counter_path_2 + 1
      
      # Kreuzpaar zusammenstellen
      cross <- c(path_1, path_2)
      
      # Fluglinienkombination ins grid schreiben
      cross_col <- paste0("kreuz_", counter_path_1, "_", counter_path_2)
      cross_fl <- paste(tools::file_path_sans_ext(basename(cross)), collapse = ",")
      grid_05[[paste0(cross_col, "_fl")]] <- cross_fl
      grid_1[[paste0(cross_col, "_fl")]]  <- cross_fl
      
      # groundhits für kreuzflüge vereinigen und ins grid schreiben
      grid_05[[paste0(cross_col,"_groundhit")]] <- grid_05[[paste0("L",strsplit(cross_fl, ",")[[1]][1],"_groundhit")]] | grid_05[[paste0("L",strsplit(cross_fl, ",")[[1]][2],"_groundhit")]]
      grid_1[[paste0(cross_col,"_groundhit")]] <- grid_1[[paste0("L",strsplit(cross_fl, ",")[[1]][1],"_groundhit")]] | grid_1[[paste0("L",strsplit(cross_fl, ",")[[1]][2],"_groundhit")]]
      
    }
  }
  print(paste("Kreuzflüge berechnet", round(as.numeric(difftime(now(), start_time_cross, units = "mins")), 2), "min"))
  
  
  # =============================================================
  # Output pro Zellengrösse in data.frame schreiben
  # =============================================================
  
  # output file als data.frame speichern
  save(grid_05, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\03_calculate_metrics)", paste0(i, "_grid_05.Rda")))
  save(grid_1, file=file.path(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\03_calculate_metrics)", paste0(i, "_grid_1.Rda")))
  
  # Memory freigeben
  rm(grid_05, grid_1, grid_05_sfc, grid_1_sfc, grid_05_sfc_selected, grid_1_sfc_selected)
  gc()
  
  end_time_loop <- now()
  print(paste0("...fertig mit Gebiet Nr. ", i, " in ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min"))
}

end_time <- now()
print(paste0("Endzeit: ", end_time))
print(paste0("Benötigte Zeit: ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2), " min"))
