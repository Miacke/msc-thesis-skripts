# ============================================================
# 04_statistical_analysis.R
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

# Pfad zu den Daten
path <- r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\output_03_calculate_metrics)"

# Daten einlesen
path_grid_02 <- file.path(path, "4_grid_02.Rda")
path_grid_05 <- file.path(path, "4_grid_05.Rda")
path_grid_1 <- file.path(path, "4_grid_1.Rda")


load(path_grid_02)
load(path_grid_05)
load(path_grid_1)

grids <- list(grid_02 = grid_02, grid_05 = grid_05, grid_1 = grid_1)
results <- list()
p_value_vec <- c()

# zufällige Kombinationen
for (name in names(grids)){
  for (i in 1:50){
    set.seed(i)
    # grid <- grids[[name]]
    n <- nrow(grid)
    
    parallel_sample <- sample(c("parallel_1_groundhit", "parallel_2_groundhit"), n, replace = TRUE)
    cross_sample <- sample(c("kreuz_1_1_groundhit", "kreuz_1_2_groundhit", "kreuz_2_1_groundhit", "kreuz_2_2_groundhit"), n, replace = TRUE)
    
    # ------------------------------------------------------------------------------
    # McNemars Test:
    
    nemar_a <- 0  # parallel und kreuz kein Bodenpunkt vorhanden
    nemar_b <- 0  # parallel kein Bodenpunkt vorhanden, kreuz aber schon
    nemar_c <- 0  # parallel ein Bodenpunkt vorhanden, kreuz aber nicht
    nemar_d <- 0  # parallel und kreuz ein Bodenpunkt vorhanden
    
    for (row in seq_len(n)){
      parallel_col <- parallel_sample[row]
      cross_col <- cross_sample[row]
      
      # Zuweisung in der Nemar-Tabelle
      if (grid[[parallel_col]][row] == FALSE & grid[[cross_col]][row] == FALSE){  # parallel und kreuz kein Bodenpunkt vorhanden
        nemar_a <- nemar_a + 1
      }
      if (grid[[parallel_col]][row] == FALSE & grid[[cross_col]][row] == TRUE){  # parallel kein Bodenpunkt vorhanden, kreuz aber schon
        nemar_b <- nemar_b + 1
      }
      if (grid[[parallel_col]][row] == TRUE & grid[[cross_col]][row] == FALSE){  # parallel ein Bodenpunkt vorhanden, kreuz aber nicht
        nemar_c <- nemar_c + 1
      }
      if (grid[[parallel_col]][row] == TRUE & grid[[cross_col]][row] == TRUE){  # parallel und kreuz ein Bodenpunkt vorhanden
        nemar_d <- nemar_d + 1
      }
      
      if ((row %% 100000)==0){
        progress <- round((row/n) * 100)
        print(paste("Fortschritt: ", progress, "%"))
      }
    }
  
    # nemars_Matrix erstellen
    nemars_table <- matrix(c(0, 0, 0, 0), nrow = 2,
                           dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                           "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
    
    # Nemars-Matrix befüllen
    nemars_table[1] <- nemar_a
    nemars_table[3] <- nemar_b
    nemars_table[2] <- nemar_c
    nemars_table[4] <- nemar_d
    
    # Statistik rechnen
    mc_nemar_statistic <- mcnemar.test(nemars_table, correct = TRUE)
    p_value_vec <- append(p_value_vec, mc_nemar_statistic$p.value)
  }
}


st_write(grid_02, file.path(path, "4_grids.gpkg"), layer = "grid_02", delete_layer = TRUE)
st_write(grid_05, file.path(path, "4_grids.gpkg"), layer = "grid_05", delete_layer = TRUE)
st_write(grid_1,  file.path(path, "4_grids.gpkg"), layer = "grid_1",  delete_layer = TRUE)