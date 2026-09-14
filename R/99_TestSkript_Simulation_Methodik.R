library(stats)

# Objekte im Arbeitsspeicher löschen (für das mehrmalige Ausführen des Skripts nacheinander)
rm(list = ls())
gc()

# 250 Zellen
n <- 250

# Wenn True, werden treffer zufällig verteilt, wenn false, liefern parallele linien eher redundante informationen
zufällig <- TRUE

# Prüfvektoren erstellen
anzahl_bodenzellen_kreuz <- c()
anzahl_bodenzellen_parallel <- c()
parallel_vec <- c()
kreuz_vec <- c()
mc_nemars_vec <- c()


# Simulation von 12 Untersuchungsgebieten
for (gebiet in seq(1,12)){
  
  # Lückentyp pro Zelle
  lueckentyp <- sample(c("NS", "EW", "offen", "dicht"), n, replace = TRUE)
  # Ost_West Linien treffen wenn EW oder offen, Nord_Süd Linien treffen wenn NS oder offen
  ew_trifft <- lueckentyp %in% c("EW", "offen")
  ns_trifft <- lueckentyp %in% c("NS", "offen")
  
  if (zufällig){
    # Vektor für Treffer der einzelnen Linien Rauschen wird hinzugefügt
    line_1 <- sample(c(TRUE,FALSE), n, TRUE, prob=c(0.7,0.3))
    line_2 <- sample(c(TRUE,FALSE), n, TRUE, prob=c(0.7,0.3))
    line_3 <- sample(c(TRUE,FALSE), n, TRUE, prob=c(0.7,0.3))
    line_4 <- sample(c(TRUE,FALSE), n, TRUE, prob=c(0.7,0.3))
  } else {
    # Vektor für Treffer der einzelnen Linien Rauschen wird hinzugefügt
    line_1 <- ew_trifft & sample(c(TRUE,FALSE), n, TRUE, prob=c(0.9,0.1))
    line_2 <- ew_trifft & sample(c(TRUE,FALSE), n, TRUE, prob=c(0.9,0.1))
    line_3 <- ns_trifft & sample(c(TRUE,FALSE), n, TRUE, prob=c(0.9,0.1))
    line_4 <- ns_trifft & sample(c(TRUE,FALSE), n, TRUE, prob=c(0.9,0.1))
  }
  
  # Dataframe daraus bilden
  df <- data.frame(line_1, line_2, line_3, line_4)
  
  # durch jede Zeile im data frame iterieren
  for (zeile in seq_len(nrow(df))){
    
    # prüfen welche zelle von linien 1/2 getroffen wurden
    if(df[zeile, "line_1"] == TRUE | df[zeile, "line_2"] == TRUE){
      df[zeile, "parallel_1_2"] <- TRUE
    } else{
      df[zeile, "parallel_1_2"] <- FALSE
    }
    
    # prüfen welche zelle von linien 3/4 getroffen wurden
    if(df[zeile, "line_3"] == TRUE | df[zeile, "line_4"] == TRUE){
      df[zeile, "parallel_3_4"] <- TRUE
    } else{
      df[zeile, "parallel_3_4"] <- FALSE
    }
    
    # prüfen welche zelle von linien 1/3 getroffen wurden
    if(df[zeile, "line_1"] == TRUE | df[zeile, "line_3"] == TRUE){
      df[zeile, "kreuz_1_3"] <- TRUE
    } else{
      df[zeile, "kreuz_1_3"] <- FALSE
    }
    
    # prüfen welche zelle von linien 2/4 getroffen wurden
    if(df[zeile, "line_2"] == TRUE | df[zeile, "line_4"] == TRUE){
      df[zeile, "kreuz_2_4"] <- TRUE
    } else{
      df[zeile, "kreuz_2_4"] <- FALSE
    }
    
    # prüfen welche zelle von linien 1/4 getroffen wurden
    if(df[zeile, "line_1"] == TRUE | df[zeile, "line_4"] == TRUE){
      df[zeile, "kreuz_1_4"] <- TRUE
    } else{
      df[zeile, "kreuz_1_4"] <- FALSE
    }
    
    # prüfen welche zelle von linien 2/3 getroffen wurden
    if(df[zeile, "line_2"] == TRUE | df[zeile, "line_3"] == TRUE){
      df[zeile, "kreuz_2_3"] <- TRUE
    } else{
      df[zeile, "kreuz_2_3"] <- FALSE
    }
  }
  kreuz <- (sum(df[, "kreuz_1_3"]) + sum(df[, "kreuz_2_4"]))/2
  parallel <- (sum(df[, "parallel_1_2"]) + sum(df[, "parallel_3_4"]))/2

  anzahl_bodenzellen_kreuz <- append(anzahl_bodenzellen_kreuz, kreuz)
  anzahl_bodenzellen_parallel <- append(anzahl_bodenzellen_parallel, parallel)
  
  # ------------------------------------------------------------------------------
  # Versuch mit McNemars Test: 
  # Zufällige auswahl einer Parallel- und einer Kreuz-Kombination
  
  # Vektor erstellen, der n* zufällig eine Kombination nennt
  parallel_kombination <- sample(c("parallel_1_2", "parallel_3_4"), n, replace = TRUE)
  kreuz_kombination <- sample(c("kreuz_1_3", "kreuz_2_4", "kreuz_1_4", "kreuz_2_3"), n, replace = TRUE)
  
  # n durchläufe
  for (zelle in 1:n){
    
    # Prüfen welche der beiden Kombinationen im zufalls-vekotr enthalten und den Wert zum Parallel-Vektor hinzufügen
    if (parallel_kombination[zelle] == "parallel_1_2"){
      parallel_vec <- append(parallel_vec, df[zelle, "parallel_1_2"])
    } else{
      parallel_vec <- append(parallel_vec, df[zelle, "parallel_3_4"])
    }
    
    # Prüfen welche der vier Kombinationen im zufalls-vekotr enthalten und den Wert zum Kreuz-Vektor hinzufügen
    if (kreuz_kombination[zelle] == "kreuz_1_3"){
      kreuz_vec <- append(kreuz_vec, df[zelle, "kreuz_1_3"])
    } else if(kreuz_kombination[zelle] == "kreuz_2_4"){
      kreuz_vec <- append(kreuz_vec, df[zelle, "kreuz_2_4"])
    } else if(kreuz_kombination[zelle] == "kreuz_1_4"){
      kreuz_vec <- append(kreuz_vec, df[zelle, "kreuz_1_4"])
    } else if(kreuz_kombination[zelle] == "kreuz_2_3"){
      kreuz_vec <- append(kreuz_vec, df[zelle, "kreuz_2_3"])
    }
  }
  
  # nemars_Matrix erstellen
  nemars_table <- matrix(c(0, 0, 0, 0), nrow = 2,
                         dimnames = list("Parallelflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden"),
                                         "Kreuzflug" = c("Bodenpunkt nicht vorhanden", "Bodenpunkt vorhanden")))
  
  for (zelle_1 in seq_along(kreuz_vec)){
    if (parallel_vec[zelle_1]){
      if (kreuz_vec[zelle_1]){
        nemars_table[4] = nemars_table[4] + 1
      } else{
        nemars_table[2] = nemars_table[2] + 1
      }
    } else {
      if (kreuz_vec[zelle_1]){
        nemars_table[3] = nemars_table[3] + 1
      } else{
        nemars_table[1] = nemars_table[1] + 1
      }
    }
  }
    
    mc_nemar_statistic <- mcnemar.test(nemars_table, correct = TRUE)
    mc_nemars_vec <- append(mc_nemars_vec, mc_nemar_statistic$p.value)
}


t_test_statistics <- t.test(anzahl_bodenzellen_kreuz, anzahl_bodenzellen_parallel, paired = TRUE, alternative = "two.sided", conf.level = 0.99)

nemar_adjusted <- p.adjust(mc_nemars_vec, method = "bonferroni")
nemar_adjusted_diff <- nemar_adjusted - mc_nemars_vec

print(paste("zufällig:", zufällig))
print(paste("p-value paired t-test:", t_test_statistics$p.value))
print(paste("mean p-value mc nemar test:", mean(mc_nemars_vec)))
print(paste("adjusted p-values - mc-nemars p-values must be bigger than 0:", all(nemar_adjusted_diff>0)))

# Test funktioniert. Wenn so strukturierte Ergebnisse systematisch zugunsten kreuz verzerrt werden, ergibt der t.test auch bei 12 Gebieten und alpha = 0.01 ein signifikantes Ergebnis. 
# Werden die Ergebnisse 100% zufällig organisiert ist der p-Wert hoch -> t-test nicht signifikant
# "adjusted p-values - mc-nemars p-values are bigger than 0: " ist bei zufälligen Daten verbugt
