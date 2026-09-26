# ============================================================
# 04_calculate_statistics.R
# ============================================================
# Beschreibung:
#   - Statistische Auswertungen
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

library(dplyr)
library(sf)
library(lubridate)

start_time <- now()
print(paste0("Start: ", start_time))

# Pfad zu den Daten
path <- r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\03_calculate_metrics)"
out_path <- r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\04_calculate_statistics)"

# Daten einlesen und in Liste speichern
grids_path <- list.files(path, pattern = "\\.Rda$", full.names = TRUE)
grids <- list()
for (g in grids_path){
  g_name <- tools::file_path_sans_ext(basename(g))
  grids[[g_name]] <- g
}

# jedes Grid der grid-Liste bearbeiten
for (name in names(grids)){
  start_time_loop <- now()
  print(paste0("Start mit ", name, ". Zeit: ", start_time))
    
  # Daten laden und Geometrie aus performanzgründen weglassen
  objname <- load(grids[[name]])  # ladet als grid_05 oder grid_1
  grid_withgeometry <- get(objname)  # weist Variabel zu
  rm(list = objname)  # grid_05/grid_1 wieder löschen
  grid <- st_drop_geometry(grid_withgeometry)  # Geometrie löschen
  n <- nrow(grid)  # Zeilenzahl
  
  set.seed(100)
  
  # Kombinationen zufällig auswählen
  parallel_sample <- sample(c("parallel_1_groundhit", "parallel_2_groundhit"), n, replace = TRUE)
  cross_sample4 <- sample(c("kreuz_1_1_groundhit", "kreuz_1_2_groundhit", "kreuz_2_1_groundhit", "kreuz_2_2_groundhit"), n, replace = TRUE)
  cross_sample2 <- sample(c("kreuz_1_1_groundhit", "kreuz_2_2_groundhit"), n, replace = TRUE)
  
  # ------------------------------------------------------------------------------
  # Zellen mit div. Auswertungen befüllen
  
  # Neue Spalten anlegen
  grid$gh_change_k2 <- NA_character_
  grid$gh_change_k4 <- NA_character_
  grid$vergleichspaar_p_k2 <- NA_character_
  grid$vergleichspaar_p_k4 <- NA_character_
  grid$parallel_sa_mean <- NA_real_
  grid$parallel_sa_range <- NA_real_
  grid$kreuz2_sa_mean <- NA_real_
  grid$kreuz2_sa_range <- NA_real_
  grid$kreuz4_sa_mean <- NA_real_
  grid$kreuz4_sa_range <- NA_real_
  grid$parallel_abstand_mean <- NA_real_
  grid$parallel_abstand_range <- NA_real_
  grid$kreuz2_abstand_mean <- NA_real_
  grid$kreuz2_abstand_range  <- NA_real_
  grid$kreuz4_abstand_mean <- NA_real_
  grid$kreuz4_abstand_range <- NA_real_
  grid$parallel_pointdensity <- NA_real_
  grid$kreuz2_pointdensity <- NA_real_
  grid$kreuz4_pointdensity <- NA_real_
  
  # Werte der zufällig gewählten Spalten abgreifen
  alle_cols <- unique(c(parallel_sample, cross_sample2,cross_sample4))
  m <- as.matrix(grid[, alle_cols, drop = FALSE])  # wählt im grid alle Spalten aus, welche in alle_cols stehen und wandelt sie in eine Matrix um
  
  p <- m[cbind(seq_len(n), match(parallel_sample, colnames(m)))]  # erstellt matrix mit 2 spalten, SA-Nr und Position der parallelauswahl in der Matrix m. parallel_1 -> 1, parallel_2 -> 2
  k2 <- m[cbind(seq_len(n), match(cross_sample2,    colnames(m)))]  # erstellt matrix mit 2 spalten, SA-Nr und Position der parallelauswahl in der Matrix m. kreuz_1_1 -> 3, kreuz_1_2 -> 4 etc.
  k4 <- m[cbind(seq_len(n), match(cross_sample4,    colnames(m)))]  # erstellt matrix mit 2 spalten, SA-Nr und Position der parallelauswahl in der Matrix m. kreuz_1_1 -> 3, kreuz_1_2 -> 4 etc.
  
  # Position der McNemartabelle in daten schreiben (2 Kreuzkombinationen)
  grid$gh_change_k2 <- ifelse(!p & !k2, "a",  # Wenn weder parallel noch kreuz trifft, dann Position "a", sonst nächstes ifelse...
                           ifelse(!p &  k2, "b", # Wenn parallel nicht trifft, kreuz aber schon, dann Position "b", sonst nächstes ifelse...
                                  ifelse( p & !k2, "c", "d")))
  
  # Position der McNemartabelle in daten schreiben (4 Kreuzkombinationen)
  grid$gh_change_k4 <- ifelse(!p & !k4, "a",  # Wenn weder parallel noch kreuz trifft, dann Position "a", sonst nächstes ifelse...
                              ifelse(!p &  k4, "b", # Wenn parallel nicht trifft, kreuz aber schon, dann Position "b", sonst nächstes ifelse...
                                     ifelse( p & !k4, "c", "d")))
  
  # Vergleichspaar (Welche Kombination der Linien für Parallel- und Kreuzflug) in Daten schreiben
  grid$vergleichspaar_p_k2 <- paste0(parallel_sample, ",", cross_sample2)
  grid$vergleichspaar_p_k4 <- paste0(parallel_sample, ",", cross_sample4)
  
  # Berechnung der Metriken des Parallelfluges
  for (pcol in unique(parallel_sample)){  # es wird durch jede mögliche Fluglinienkombination iteriert
    
    mask <- parallel_sample == pcol  # vektor mit n Zeilen, Inhalt: TRUE/FALSE. 
    auswahl <- sub("_groundhit$", "", pcol)  # zB "parallel_1" je nach pcol
    
    fl <- strsplit(grid[[paste0(auswahl, "_fl")]][1], ",")[[1]]  # vektor der beteiligten fluglinien der auswahl
    
    # scan-angles
    sa_m <- abs(as.matrix(grid[mask, c(paste0("L", fl, "_sa_max"), paste0("L", fl, "_sa_min")), drop = FALSE]))  # grid[x,y] wählt zuerst zeilen (true/false maske, logisches indizieren) und nach dem komma spalten (sa_max und sa_min für jede Fluglinie). das ganze wird in einer matrix gespeichert. absolute werte da egal ob -15 oder +15°
    abstand_m <- as.matrix(grid[mask, paste0("L", fl, "_abstand"), drop = FALSE])
    
    grid$parallel_sa_mean[mask]  <- rowMeans(sa_m)
    grid$parallel_sa_range[mask] <- apply(sa_m, 1, max) - apply(sa_m, 1, min)
    grid$parallel_pointdensity[mask] <- rowSums(as.matrix(grid[mask, paste0("L", fl, "_dichte"), drop = FALSE]))
    grid$parallel_abstand_mean[mask] <- rowMeans(abstand_m)
    grid$parallel_abstand_range[mask] <-  apply(abstand_m, 1, max) - apply(abstand_m, 1, min)
  }
  
  for (kcol in unique(cross_sample2)){
    
    mask <- cross_sample2 == kcol
    auswahl <- sub("_groundhit$", "", kcol)          # "kreuz_1_1"
    
    fl <- trimws(strsplit(grid[[paste0(auswahl, "_fl")]][1], ",")[[1]])
    
    sa_m <- abs(as.matrix(grid[mask, c(paste0("L", fl, "_sa_max"), paste0("L", fl, "_sa_min")), drop = FALSE]))
    abstand_m <- as.matrix(grid[mask, paste0("L", fl, "_abstand"), drop = FALSE])
    
    grid$kreuz2_sa_mean[mask]  <- rowMeans(sa_m)
    grid$kreuz2_sa_range[mask] <- apply(sa_m, 1, max) - apply(sa_m, 1, min)
    grid$kreuz2_pointdensity[mask] <- rowSums(as.matrix(grid[mask, paste0("L", fl, "_dichte"), drop = FALSE]))
    grid$kreuz2_abstand_mean[mask] <- rowMeans(abstand_m)
    grid$kreuz2_abstand_range[mask] <- apply(abstand_m, 1, max) - apply(abstand_m, 1, min)
  }
  
  for (kcol in unique(cross_sample4)){
    
    mask <- cross_sample4 == kcol
    auswahl <- sub("_groundhit$", "", kcol)          # "kreuz_1_1"
    
    fl <- trimws(strsplit(grid[[paste0(auswahl, "_fl")]][1], ",")[[1]])
    
    sa_m <- abs(as.matrix(grid[mask, c(paste0("L", fl, "_sa_max"), paste0("L", fl, "_sa_min")), drop = FALSE]))
    abstand_m <- as.matrix(grid[mask, paste0("L", fl, "_abstand"), drop = FALSE])
    
    grid$kreuz4_sa_mean[mask]  <- rowMeans(sa_m)
    grid$kreuz4_sa_range[mask] <- apply(sa_m, 1, max) - apply(sa_m, 1, min)
    grid$kreuz4_pointdensity[mask] <- rowSums(as.matrix(grid[mask, paste0("L", fl, "_dichte"), drop = FALSE]))
    grid$kreuz4_abstand_mean[mask] <- rowMeans(abstand_m)
    grid$kreuz4_abstand_range[mask] <- apply(abstand_m, 1, max) - apply(abstand_m, 1, min)
  }
  
  # Ergebnisse zurück an das sf-Objekt hängen
  neue_spalten <- c("gh_change_k2","gh_change_k4","vergleichspaar_p_k2","vergleichspaar_p_k4","parallel_sa_mean","parallel_sa_range","kreuz2_sa_mean","kreuz2_sa_range","kreuz4_sa_mean","kreuz4_sa_range","parallel_abstand_mean","parallel_abstand_range","kreuz2_abstand_mean","kreuz2_abstand_range","kreuz4_abstand_mean","kreuz4_abstand_range","parallel_pointdensity","kreuz2_pointdensity","kreuz4_pointdensity")  
  grid_withgeometry[neue_spalten] <- grid[neue_spalten]
  
  # abspeichern
  st_write(grid_withgeometry, file.path(out_path, paste0(name, ".gpkg")),layer = name, delete_layer = TRUE)
  saveRDS(grid_withgeometry, file.path(out_path, paste0(name, ".rds")))
  
  rm(grid, grid_withgeometry)
  gc()
  
  end_time_loop <- now()
  print(paste0("...fertig mit ", name, " in ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min"))
}

print(paste0("...fertig nach ", round(as.numeric(difftime(now(), start_time, units = "mins")), 2), " min"))

