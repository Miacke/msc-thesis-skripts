# ============================================================
# 04_statistical_analysis.R
# ============================================================
# Beschreibung:
#   - Statistische Auswertungen
#
# Input:

#
# Output:

#
# Autor:       Mirco Ackermann
# Datum:       25.09.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

library(sf)
library(dplyr)

# data frame für statistische Resultate
stat_df <- data.frame()

# RDS-Files einlesen
rds_files <- list.files(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\04_calculate_statistics)", pattern = "\\.rds$", full.names = TRUE)

for (file_path in rds_files){
  
  # Zufallsgenerator
  set.seed(100)
  
  # full data
  file <- readRDS(file_path)
  file <- st_drop_geometry(file)
  
  # sample data (5000)
  row_sample <- sample(seq_len(nrow(file)), 500)
  file_sample <- file[row_sample,]
  # ---------------------------------------------------
  # McNemar - alle Zellen
  
  # nemars_Matrizen erstellen (Auswertung über alle Zellen "full" und über sample "sample")
  mcnemar_table_full <- matrix(c(0, 0, 0, 0), nrow = 2,
                         dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                         "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
  mcnemar_table_sample <- matrix(c(0, 0, 0, 0), nrow = 2,
                               dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                               "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
  
  # Nemars-Matrizen fbefüllen
  mcnemar_table_full[1] <- sum(file$gh_change == "a")
  mcnemar_table_full[3] <- sum(file$gh_change == "b")
  mcnemar_table_full[2] <- sum(file$gh_change == "c")
  mcnemar_table_full[4] <- sum(file$gh_change == "d")
  mcnemar_table_sample[1] <- sum(file_sample$gh_change == "a")
  mcnemar_table_sample[3] <- sum(file_sample$gh_change == "b")
  mcnemar_table_sample[2] <- sum(file_sample$gh_change == "c")
  mcnemar_table_sample[4] <- sum(file_sample$gh_change == "d")
  

  # Zellen mit Statistikwerten befüllen
  stat_df <- rbind(stat_df, data.frame(
    
    # Identifikatoren
    grid = basename(file_path),
    SA_nr = as.integer(strsplit(basename(file_path), "_")[[1]][1]),  # Nummer Untersuchungsgebiet
    
    # McNemar Statistik
    mcnemar_full_pvalue = mcnemar.test(mcnemar_table_full, correct = FALSE)$p.value,  # p-wert
    mcnemar_full_anteilb = mcnemar_table_full[3] / (mcnemar_table_full[3] + mcnemar_table_full[2]),  # b / (b+c)  # anteil von b -> wenn > 0.5 dann effekt in richtung kreuz
    mcnemar_full_anteil_veraenderung = (mcnemar_table_full[3] + mcnemar_table_full[2]) / nrow(file),  # anteil der veränderten zellen (b,c) an der gesamtzahl an zellen
    mcnemar_sample_pvalue = mcnemar.test(mcnemar_table_sample, correct = FALSE)$p.value,  # p-wert
    mcnemar_samplel_anteilb = mcnemar_table_sample[3] / (mcnemar_table_sample[3] + mcnemar_table_sample[2]),  # b / (b+c)  # anteil von b -> wenn > 0.5 dann effekt in richtung kreuz
    mcnemar_sample_anteil_veraenderung = (mcnemar_table_sample[3] + mcnemar_table_sample[2]) / nrow(file_sample)  # anteil der veränderten zellen (b,c) an der gesamtzahl an zellen
  ))
  
}
