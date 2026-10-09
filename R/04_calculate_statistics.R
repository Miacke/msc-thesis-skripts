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
path <- r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\03_calculate_metrics\02_outside_flightlines)"
out_path <- r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\04_calculate_statistics\02_outside_flightlines)"
fgdb_path <- file.path(out_path, "04_calculate_statistics.gdb")
if (dir.exists(fgdb_path)) unlink(fgdb_path, recursive = TRUE)  # fGDB aus früheren Durchläufen löschen

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
  print(paste0("Start mit ", name, ". Zeit: ", start_time_loop))
  
  # Daten laden und Geometrie aus performanzgründen weglassen
  objname <- load(grids[[name]])  # ladet als grid_05 oder grid_1
  grid_withgeometry <- get(objname)  # weist Variabel zu
  rm(list = objname)  # grid_05/grid_1 wieder löschen
  
  # TEMPORAER. Spaltennamen aus Skript 03 ins Englische übersetzen 
  cn <- names(grid_withgeometry)
  cn <- sub("_dichte$",  "_density",  cn)
  cn <- sub("_abstand$", "_distance", cn)
  cn <- sub("^kreuz_",   "cross_",    cn)
  names(grid_withgeometry) <- cn
  
  grid <- st_drop_geometry(grid_withgeometry)  # Geometrie löschen
  n <- nrow(grid)  # Zeilenzahl
  
  set.seed(100)
  
  # Kombinationen zufällig auswählen
  parallel_sample <- sample(c("parallel_1_groundhit", "parallel_2_groundhit"), n, replace = TRUE)
  cross_sample <- sample(c("cross_1_1_groundhit", "cross_1_2_groundhit", "cross_2_1_groundhit", "cross_2_2_groundhit"), n, replace = TRUE)
  
  # ------------------------------------------------------------------------------
  # Zellen mit div. Auswertungen befüllen
  # ------------------------------------------------------------------------------
  
  # Neue Spalten anlegen
  grid$gh_change <- NA_character_
  grid$comparison_p_c <- NA_character_
  grid$parallel_sa_mean <- NA_real_
  grid$parallel_sa_range <- NA_real_
  grid$cross_sa_mean <- NA_real_
  grid$cross_sa_range <- NA_real_
  grid$parallel_distance_mean <- NA_real_
  grid$parallel_distance_range <- NA_real_
  grid$parallel_distance_sum <- NA_real_
  grid$cross_distance_mean <- NA_real_
  grid$cross_distance_range <- NA_real_
  grid$cross_distance_sum <- NA_real_
  grid$parallel_pointdensity <- NA_real_
  grid$cross_pointdensity <- NA_real_
  grid$penetration_parallel <- NA_real_
  grid$penetration_cross    <- NA_real_
  grid$pr_cminusp_diff <- NA_real_
  grid$distance_cminusp_diff <- NA_real_
  
  # Werte der zufällig gewählten Spalten abgreifen
  all_cols <- unique(c(parallel_sample, cross_sample))
  m <- as.matrix(grid[, all_cols, drop = FALSE])  # wählt im grid alle Spalten aus, welche in all_cols stehen und wandelt sie in eine Matrix um. Die Werte stammen direkt aus dem grid
  
  p <- m[cbind(seq_len(n), match(parallel_sample, colnames(m)))]  # erstellt logischer Vektor mit jenen werten von m welche gemäss parallel_sample angewählt werden sollen
  k <- m[cbind(seq_len(n), match(cross_sample,    colnames(m)))]
  
  # Position der McNemartabelle in daten schreiben (4 crosskombinationen)
  grid$gh_change <- ifelse(!p & !k, "a",  # Wenn weder parallel noch kreuz trifft, dann Position "a", sonst nächstes ifelse...
                              ifelse(!p &  k, "b", # Wenn parallel nicht trifft, kreuz aber schon, dann Position "b", sonst nächstes ifelse...
                                     ifelse( p & !k, "c", "d")))
  
  # Vergleichspaar (Welche Kombination der Linien für Parallel- und Kreuzflug) in Daten schreiben
  grid$comparison_p_c <- paste0(parallel_sample, ",", cross_sample)
  
  # Penetrationsrate Differenz in daten schreiben
  pr_parallel <- sub("_groundhit$", "_pr", parallel_sample)  # sample umschreiben -> alle "_groundhit" suchen und durch "_pr" ersetzen
  pr_cross <- sub("_groundhit$", "_pr", cross_sample)
  all_pr_colls <- unique(c(pr_parallel, pr_cross))
  m_pr <- as.matrix(grid[, all_pr_colls, drop=FALSE])
  p_pr <- m_pr[cbind(seq_len(n), match(pr_parallel, colnames(m_pr)))]
  k_pr <- m_pr[cbind(seq_len(n), match(pr_cross, colnames(m_pr)))]
  grid$pr_cminusp_diff <- k_pr - p_pr
  grid$penetration_parallel <- p_pr
  grid$penetration_cross <- k_pr
  
  # -------------------------------------------------------------------------
  # Metriken für Vergleiche (Linienabstand, Scanangle, etc.)
  # -------------------------------------------------------------------------
  
  # Berechnung der Metriken des Parallelfluges
  for (pcol in unique(parallel_sample)){  # es wird durch jede mögliche Fluglinienkombination iteriert
    
    mask <- parallel_sample == pcol  # vektor mit n Zeilen, Inhalt: TRUE/FALSE.
    auswahl <- sub("_groundhit$", "", pcol)  # zB "parallel_1" je nach pcol
    
    fl <- strsplit(grid[[paste0(auswahl, "_fl")]][1], ",")[[1]]  # vektor der beteiligten fluglinien der auswahl
    
    # scan-angles
    sa_m <- abs(as.matrix(grid[mask, c(paste0("L", fl, "_sa_max"), paste0("L", fl, "_sa_min")), drop = FALSE]))  # grid[x,y] wählt zuerst zeilen (true/false maske, logisches indizieren) und nach dem komma spalten (sa_max und sa_min für jede Fluglinie). das ganze wird in einer matrix gespeichert. absolute werte da egal ob -15 oder +15°
    distance_m <- as.matrix(grid[mask, paste0("L", fl, "_distance"), drop = FALSE])
    
    grid$parallel_sa_mean[mask]  <- rowMeans(sa_m)
    grid$parallel_sa_range[mask] <- apply(sa_m, 1, max) - apply(sa_m, 1, min)
    grid$parallel_pointdensity[mask] <- rowSums(as.matrix(grid[mask, paste0("L", fl, "_density"), drop = FALSE]))
    grid$parallel_distance_mean[mask] <- rowMeans(distance_m)
    grid$parallel_distance_range[mask] <-  apply(distance_m, 1, max) - apply(distance_m, 1, min)
    grid$parallel_distance_sum[mask] <- rowSums(distance_m)
  }
  
  # Berechnung der Metriken des Kreuzfluges
  for (kcol in unique(cross_sample)){
    
    mask <- cross_sample == kcol
    auswahl <- sub("_groundhit$", "", kcol)          # "kreuz_1_1"
    
    fl <- trimws(strsplit(grid[[paste0(auswahl, "_fl")]][1], ",")[[1]])
    
    sa_m <- abs(as.matrix(grid[mask, c(paste0("L", fl, "_sa_max"), paste0("L", fl, "_sa_min")), drop = FALSE]))
    distance_m <- as.matrix(grid[mask, paste0("L", fl, "_distance"), drop = FALSE])
    
    grid$cross_sa_mean[mask]  <- rowMeans(sa_m)
    grid$cross_sa_range[mask] <- apply(sa_m, 1, max) - apply(sa_m, 1, min)
    grid$cross_pointdensity[mask] <- rowSums(as.matrix(grid[mask, paste0("L", fl, "_density"), drop = FALSE]))
    grid$cross_distance_mean[mask] <- rowMeans(distance_m)
    grid$cross_distance_range[mask] <- apply(distance_m, 1, max) - apply(distance_m, 1, min)
    grid$cross_distance_sum[mask] <- rowSums(distance_m)
  }
  
  # Differenz des mittleren Abstands in die Daten schreiben
  grid$distance_cminusp_diff <- grid$cross_distance_mean - grid$parallel_distance_mean
  
  # Ergebnisse zurück an das sf-Objekt hängen
  new_col <- c("gh_change", "distance_cminusp_diff", "pr_cminusp_diff", "penetration_parallel", "penetration_cross", "comparison_p_c",
               "parallel_sa_mean","parallel_sa_range","cross_sa_mean","cross_sa_range","parallel_distance_mean", "parallel_distance_sum",
               "parallel_distance_range","cross_distance_mean", "cross_distance_sum","cross_distance_range","parallel_pointdensity","cross_pointdensity")
  grid_withgeometry[new_col] <- grid[new_col]
  
  # abspeichern
  layer_name <- paste0("SA_", name)   # Layername darf nicht mit einer Ziffer beginnen
  st_write(grid_withgeometry,dsn = fgdb_path, layer = layer_name, driver = "OpenFileGDB", delete_layer = TRUE, quiet = TRUE)
  saveRDS(grid_withgeometry, file.path(out_path, paste0(name, ".rds")))
  
  rm(grid, grid_withgeometry)
  gc()
  
  end_time_loop <- now()
  print(paste0("...fertig mit ", name, " in ", round(as.numeric(difftime(end_time_loop, start_time_loop, units = "mins")), 2), " min"))
}

print(paste0("...fertig nach ", round(as.numeric(difftime(now(), start_time, units = "mins")), 2), " min"))