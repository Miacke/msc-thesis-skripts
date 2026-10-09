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
library(lubridate)
library(ggplot2)

# ============================================================
# Berechnungen
# ============================================================

start_time <- now()
print(paste0("Start: ", start_time))

# data frame für statistische Resultate
stat_df <- data.frame()

# RDS-Files einlesen
rds_files <- list.files(r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\04_calculate_statistics)", pattern = "\\.rds$", full.names = TRUE)

for (file_path in rds_files[1]){
  start_time_loop <- now()
  print(paste0("Start mit ", basename(file_path), ": ", start_time))
  
  
  # Zufallsgenerator
  set.seed(100)
  
  # full data
  file <- readRDS(file_path)
  file <- st_drop_geometry(file)
  
  # sample data (5000)
  row_sample <- sample(seq_len(nrow(file)), 1000)
  file_sample <- file[row_sample,]
  
  # data mit gleicher Punktdichte
  file_density <- dplyr::filter(file, parallel_pointdensity == cross_pointdensity) 
  
  # data mit gleichem durchschnittlichem Linienabstand
  file_distance <- dplyr::filter(file, abs(distance_cminusp_diff) <= 5)
  
  # ---------------------------------------------------
  # t-test
  
  ttest <- t.test(file$penetration_cross, file$penetration_parallel, paired = TRUE, alternative = "greater")
  ttest_sample <- t.test(file_sample$penetration_cross, file_sample$penetration_parallel, paired = TRUE, alternative = "greater")
  
  ttest_pointdensity <- t.test(file$cross_pointdensity, file$parallel_pointdensity, paired = TRUE, alternative = "two.sided")

  # ---------------------------------------------------
  # McNemar
  # nemars_Matrizen erstellen (Auswertung über alle Zellen "full" und über sample "sample")
  mcnemar_table_full <- matrix(c(0, 0, 0, 0), nrow = 2,
                         dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                         "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
  mcnemar_table_sample <- matrix(c(0, 0, 0, 0), nrow = 2,
                               dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                               "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
  mcnemar_table_equalpointdensity <- matrix(c(0, 0, 0, 0), nrow = 2,
                                            dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                                            "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
  mcnemar_table_equaldistance <- matrix(c(0, 0, 0, 0), nrow = 2,
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
  
  mcnemar_table_equalpointdensity[1] <- sum(file_density$gh_change == "a")
  mcnemar_table_equalpointdensity[3] <- sum(file_density$gh_change == "b")
  mcnemar_table_equalpointdensity[2] <- sum(file_density$gh_change == "c")
  mcnemar_table_equalpointdensity[4] <- sum(file_density$gh_change == "d")
  
  mcnemar_table_equaldistance[1] <- sum(file_distance$gh_change == "a")
  mcnemar_table_equaldistance[3] <- sum(file_distance$gh_change == "b")
  mcnemar_table_equaldistance[2] <- sum(file_distance$gh_change == "c")
  mcnemar_table_equaldistance[4] <- sum(file_distance$gh_change == "d")
  
  # ---------------------------------------------------
  # Redundanz / Komplementarität zweier Linien pro Kombination verlgeichen mit Yule's Q
  
  fl_cols <- grep("_fl$", names(file), value = TRUE)
  yules_q_list <- list()
  
  for (fl_col in fl_cols){
    combination <- sub("_fl$", "", fl_col)
    lines <- strsplit(file[[fl_col]], ",")[[1]]
    line_a <- lines[1]
    line_b <- lines[2]
    
    x <- file[[paste0("L", line_a, "_groundhit")]]
    y <- file[[paste0("L", line_b, "_groundhit")]]
    
    sum_a <- as.numeric(sum(!x & !y))
    sum_b <- as.numeric(sum(!x & y))
    sum_c <- as.numeric(sum(x & !y))
    sum_d <- as.numeric(sum(x & y))
    
    yules_q <- ((sum_a * sum_d) - (sum_b * sum_c)) / ((sum_a * sum_d) + (sum_b * sum_c))
    yules_q_list[[combination]] <- yules_q
  }
  

  # ---------------------------------------------------
  # Ergebnisse in dataframe schreiben
  stat_df <- rbind(stat_df, data.frame(
    
    # Identifikatoren
    grid = basename(file_path),
    SA_nr = as.integer(strsplit(basename(file_path), "_")[[1]][1]),  # Nummer Untersuchungsgebiet
    
    # McNemar Statistik
    mcnemar_full_pvalue = mcnemar.test(mcnemar_table_full, correct = FALSE)$p.value,  # p-wert
    mcnemar_full_shareb = mcnemar_table_full[3] / (mcnemar_table_full[3] + mcnemar_table_full[2]),  # b / (b+c)  # anteil von b -> wenn > 0.5 dann effekt in richtung kreuz
    mcnemar_full_share_change = (mcnemar_table_full[3] + mcnemar_table_full[2]) / nrow(file),  # anteil der veränderten zellen (b,c) an der gesamtzahl an zellen
    
    mcnemar_sample_pvalue = mcnemar.test(mcnemar_table_sample, correct = FALSE)$p.value,  # p-wert
    mcnemar_sample_shareb = mcnemar_table_sample[3] / (mcnemar_table_sample[3] + mcnemar_table_sample[2]),  # b / (b+c)  # anteil von b -> wenn > 0.5 dann effekt in richtung kreuz
    mcnemar_sample_share_change = (mcnemar_table_sample[3] + mcnemar_table_sample[2]) / nrow(file_sample),  # anteil der veränderten zellen (b,c) an der gesamtzahl an zellen
    
    mcnemar_equalpointdensity_pvalue = mcnemar.test(mcnemar_table_equalpointdensity, correct = FALSE)$p.value,  # p-wert
    mcnemar_equalpointdensity_shareb = mcnemar_table_equalpointdensity[3] / (mcnemar_table_equalpointdensity[3] + mcnemar_table_equalpointdensity[2]),  # b / (b+c)  # anteil von b -> wenn > 0.5 dann effekt in richtung kreuz
    mcnemar_equalpointdensity_share_change = (mcnemar_table_equalpointdensity[3] + mcnemar_table_equalpointdensity[2]) / nrow(file_density),  # anteil der veränderten zellen (b,c) an der gesamtzahl an zellen
    
    mcnemar_equaldistance_pvalue = mcnemar.test(mcnemar_table_equaldistance, correct = FALSE)$p.value,  # p-wert
    mcnemar_equaldistance_shareb = mcnemar_table_equaldistance[3] / (mcnemar_table_equaldistance[3] + mcnemar_table_equaldistance[2]),  # b / (b+c)  # anteil von b -> wenn > 0.5 dann effekt in richtung kreuz
    mcnemar_equaldistance_share_change = (mcnemar_table_equaldistance[3] + mcnemar_table_equaldistance[2]) / nrow(file_distance),  # anteil der veränderten zellen (b,c) an der gesamtzahl an zellen
    
    # t-test für penetrationsraten
    ttest_full_pvalue = ttest$p.value,
    ttest_sample_pvalue = ttest_sample$p.value,
    ttest_pointdensity = ttest_pointdensity$p.value,
    
    # yules q für Redundanz/Komplementarität
    yules_q_p1 = yules_q_list["parallel_1"][[1]],
    yules_q_p2 = yules_q_list["parallel_2"][[1]],
    yules_q_c11 = yules_q_list["cross_1_1"][[1]],
    yules_q_c12 = yules_q_list["cross_1_2"][[1]],
    yules_q_c21 = yules_q_list["cross_2_1"][[1]],
    yules_q_c22 = yules_q_list["cross_2_2"][[1]],
    yules_parallel_mean = (yules_q_list["parallel_1"][[1]] + yules_q_list["parallel_2"][[1]])/2,
    yules_cross_mean = (yules_q_list["cross_1_1"][[1]] + yules_q_list["cross_1_2"][[1]] + yules_q_list["cross_2_1"][[1]] + yules_q_list["cross_1_1"][[1]])/4
    
  ))
  
  # ============================================================
  # Tabellen
  # ============================================================
  
  
  groundhit_share_p1 <- sum(file$parallel_1_groundhit) / length(file$parallel_1_groundhit)
  groundhit_share_p2 <- sum(file$parallel_2_groundhit) / length(file$parallel_2_groundhit)
  groundhit_share_k1_1 <- sum(file$cross_1_1_groundhit) / length(file$cross_1_1_groundhit)
  groundhit_share_k1_2 <- sum(file$cross_1_2_groundhit) / length(file$cross_1_2_groundhit)
  groundhit_share_k2_1 <- sum(file$cross_2_1_groundhit) / length(file$cross_2_1_groundhit)
  groundhit_share_k2_2 <- sum(file$cross_2_2_groundhit) / length(file$cross_2_2_groundhit)
  
  
  
  # ============================================================
  # Grafiken 
  # ============================================================
  
  # ------------------------------------------------------------
  # Werte und Abhängigkeiten pro Fluglinie
  # ------------------------------------------------------------
  
  # Rasterweite und Gebietsnummer aus dem Dateinamen lesen
  parts <- strsplit(tools::file_path_sans_ext(basename(file_path)), "_")[[1]]
  i <- parts[1]
  cell_size <- switch(parts[length(parts)], "05" = 0.5, "1" = 1, "5" = 5, NA_real_)  # "05" muss in 0.5 umgewandelt werden
  
  # Alle Liniennamen speichern
  line_vec <- unique(sub("_distance$", "", grep("^L.*_distance$", names(file), value = TRUE)))
  
  # Dataframe bilden wo jede Zeile eine Zelle pro Linie ist und mit benötigten Attributen befüllen
  long_df <- do.call(rbind, lapply(line_vec, function(l){
    data.frame(
      line          = sub("^L", "", l),
      distance        = file_sample[[paste0(l, "_distance")]],
      density         = file_sample[[paste0(l, "_density")]],
      groundhit      = as.integer(file_sample[[paste0(l, "_groundhit")]]),
      n_groundpoints = file_sample[[paste0(l, "_n_gp")]]
    )
  }))
  
  # Liniendiagramm: Abhängigkeit Abstand - mittlere Punktdichte
  print(
    ggplot(long_df, aes(x = distance, y = density, colour = line)) +
      stat_summary_bin(fun = mean, bins = 10, geom = "line") +
      labs(title    = "Zusammenhang von Fluglinienabstand und mittlerer Punktdichte pro Zelle",
           subtitle = paste0("Untersuchungsgebiet Nr.: ", i, "; Zellgrösse: ", cell_size, "m"),
           x = "Abstand zur Fluglinie [m]",
           y = "mittlere Punktdichte [Pkt/m²]", colour = "Linie") +
      theme_minimal()
  )
  
  # Liniendiagramm: Abhängigkeit Abstand - Anzahl Bodenpunkte
  print(
    ggplot(long_df, aes(x = distance, y = n_groundpoints, colour = line)) +
      stat_summary_bin(fun = mean, bins = 10, geom = "line") +
      labs(title    = "Zusammenhang von Fluglinienabstand und Anzahl Bodenpunkte pro Zelle",
           subtitle = paste0("Untersuchungsgebiet Nr.: ", i, "; Zellgrösse: ", cell_size, "m"),
           x = "Abstand zur Fluglinie [m]",
           y = "Anzahl Bodenpunkte [Pkt]", colour = "Linie") +
      theme_minimal()
  )
  
  # Anteil der Zellen mit Bodenpunkt in Abhängigkeit zum Abstand zur Fluglinie
  print(
    ggplot(long_df, aes(x = distance, y = groundhit, colour = line)) +
      stat_summary_bin(fun = mean, bins = 10, geom = "line") +
      labs(title = "Anteil der Zellen mit Bodenpunkt in Abhängigkeit zum Abstand zur Fluglinie",
           subtitle = paste0("Untersuchungsgebiet Nr.: ", i, "; Zellgrösse: ", cell_size, "m"),
           x = "Abstand zur Fluglinie [m]",
           y = "Anteil der Zellen mit Bodenpunkt", colour = "Linie") +
      theme_minimal()
  )
  
  # ------------------------------------------------------------
  # Kombinierte Darstellungen Kreuz-/Parallel-/Einzeln
  # ------------------------------------------------------------
  
  
  # ------------------------------------------------------------
  # Gegenüberstellungen Kreuz- und Parallelkombinationen
  # ------------------------------------------------------------

  # graph für Abhängigkeit von Linienkombinationen mit div parameter erstellen. es Werden nur die Zellen b/c untersucht
  graph_df <- subset(file, gh_change %in% c("b", "c"))  # Alle Zellen die sich verändert haben. b: parallel kein hit, kreuz hit; c: parallel hit, kreuz kein hit
  graph_df$distance_diff <- graph_df$cross_distance_mean - graph_df$parallel_distance_mean
  graph_df$pointdensity_change <- graph_df$cross_pointdensity - graph_df$parallel_pointdensity
  graph_df$sa_mean_change <- graph_df$cross_sa_mean - graph_df$parallel_sa_mean
  graph_df$sa_range_change <- graph_df$cross_sa_range - graph_df$parallel_sa_range
  
  bw <- 1 / cell_size^2

  # Histogramm McNemar-Felder B und C, farblich getrennt, in Abhängigkeit zum Abstand zur Fluglinie
  print(
    ggplot(graph_df, aes(x = distance_diff, fill = gh_change)) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(title = "Abstandsunterschied in den diskordanten Zellen",
           subtitle = paste0("Untersuchungsgebiet Nr.: ", i, "; Zellgrösse: ", cell_size, "m"),
           x = "Abstand kreuz − parallel [m]", y = "Anzahl Zellen", fill = NULL) +
      scale_fill_manual(
        values = c(b = "#2c7fb8", c = "#d95f0e"),
        labels = c(b = "Nur Kreuzflug trifft Boden",
                   c = "Nur Parallelflug trifft Boden"),
        name   = NULL
      ) +
      theme_minimal()
  )

  
  print(
    ggplot(graph_df, aes(x = pointdensity_change, fill = gh_change)) +
      geom_histogram(binwidth = max(bw, 1), center = 0, position = "identity", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      coord_cartesian(xlim = c(-50, 50)) +
      labs(title = "Punktdichte Unterschied in den diskordanten Zellen",
           subtitle = paste0("Untersuchungsgebiet Nr.: ", i, "; Zellgrösse: ", cell_size, "m"),
           x = "Punktdichte kreuz − parallel [Pkt/m²]", y = "Anzahl Zellen", fill = NULL) +
      scale_fill_manual(
        values = c(b = "#2c7fb8", c = "#d95f0e"),
        labels = c(b = "Nur Kreuzflug trifft Boden",
                   c = "Nur Parallelflug trifft Boden"),
        name   = NULL
      ) +
      theme_minimal()
  )
  

  print(
    ggplot(graph_df, aes(x = sa_mean_change, fill = gh_change)) +
      geom_histogram(binwidth = 2, position = "identity", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      coord_cartesian(xlim = c(-50, 50)) +
      labs(title = "Scanwinkelunterschied in den diskordanten Zellen",
           subtitle = paste0("Untersuchungsgebiet Nr.: ", i, "; Zellgrösse: ", cell_size, "m"),
           x = "Scanangle-Differenz Durchschnitte kreuz − parallel [°]", y = "Anzahl Zellen", fill = NULL) +
      scale_fill_manual(
        values = c(b = "#2c7fb8", c = "#d95f0e"),
        labels = c(b = "Nur Kreuzflug trifft Boden",
                   c = "Nur Parallelflug trifft Boden"),
        name   = NULL
      ) +
      theme_minimal()
  )

  
  print(paste0("...fertig mit ", basename(file_path), " in ", round(as.numeric(difftime(now(), start_time_loop, units = "mins")), 2), " min"))
  
}

print(paste0("...fertig in ", round(as.numeric(difftime(now(), start_time, units = "mins")), 2), " min"))
