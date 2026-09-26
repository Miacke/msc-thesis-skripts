# ============================================================
# 01_clip_reclassify_normalize.R
# ============================================================
# Beschreibung:
#   Nimmt die rohen Untersuchungsgebiete gem. halbautomatischer Auswertung, sucht die entsprechenden Fluglinien und führt folgendes aus:
#   Untersuchungsgebiete puffern, clip, klassifizierung löschen, boden und noise neu klassifizieren und noise herausfiltern, normalisieren, laz-file abspeichern, chm erstellen, chm abspeichern
#
# Input:
#   - Untersuchungsgebiete
#   - Ordner Pfad mit BEV-Lieferung
#
# Output:
#   - .laz und .lax files pro Untersuchungsgebiet + 30m puffer, neu klassifiziert und normalisiert
#   - .tif pro Untersuchungsgebiet mit CHM
#   nach diesem Skript müssen die Untersuchungsgebiete manuell reduziert werden
#
# Autor:       Mirco Ackermann
# Datum:       24.09.2026
# Projekt:     UNIGIS MasterThesis

#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

library(lidR)
library(terra)
library(dplyr)
library(sf)
library(future)
library(lubridate)

start_time <- now()
print(paste0("Start: ", start_time))

# Parallelisierung und andere Optionen
plan(multisession, workers = 3)  # max. 3 workers
options(lidR.progress = FALSE)

# Pfad zur halbautomatisch generierten Survey Area setzen (ohne inverse Buffer und nicht auf Waldmaske zugeschnitten)
survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\02_Survey_areas\SURVEY_AREA_MSCTHESIS.gdb)"
survey_area_fcname <- r"(survey_area_raw)"
survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# Pfad zu den vom BEV gelieferten LAZ-Files
las_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data)"

# Pfad zum Ort wo die gepufferten und geclippten LAZ-Files geschrieben werden sollen
write_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_01)"

# alle Flugliniennamen der .laz files der BEV-Lieferung in vektor speichern
laz_files <- list.files(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data)", pattern = "\\.laz$", recursive = TRUE, full.names = TRUE)
flightline_vec <- c()  # alle Flugliniennamen (str_bez) der BEV-Lieferung

for (laz_file in laz_files){
  filename_parts <- strsplit(basename(laz_file), "_")[[1]]
  pos <- which(filename_parts == "POST")
  linie <- paste(filename_parts[(pos - 2):(pos - 1)], collapse = "_")
  flightline_vec <- append(flightline_vec, linie)
}

names(laz_files) <- flightline_vec  # benannter Vektor erstellen -> str_bez - "laz_file_path"

# ------------------------------------------------------------------------------------------------------------
# laz-files einlesen, untersuchungsgebiet puffern, clippen, normalisieren, chm-rechnen

for (i in survey_area$SA_Nr){  # jedes Untersuchungsgebiet
  
  if (i != 6){  # Wenn Programm abbricht kann ggf. an bestimmter Stelle wieder eingesetzt werden.
    print(paste("Gebiet Nr", i, "übersprugen"))
    next
    }
  
  start_time_loop <- now()
  print(paste("Start Untersuchungsgebiet Nummer", i))
  sa <- dplyr::filter(survey_area, SA_Nr == i)
  sa_flightline_paths <- c()  # Vektor wo die Pfade aller Fluglinien pro Untersuchungsgebiet gespeichert sind
  
  for (str_bez in strsplit(sa$str_bez_list, ",")[[1]]){  # Jede beteiligte Fluglinie jedes Untersuchungsgebietes
    
    # Kontrolle ob die Fluglinienbezeichnung der fGDB-Feature Class eindeutig ist
    if (sum(flightline_vec == str_bez) > 1){
      stop(paste("Linie", str_bez, "kommt mehr als 1 mal im flightline_vec vor"))
    }
    
    str_bez_pfad <- laz_files[[str_bez]]  # Pfad aus named-vector auslesen...
    sa_flightline_paths <- append(sa_flightline_paths, str_bez_pfad)  # ... und in Vektor speichern
    
  }
  
  # laz-files einlesen
  laz <- readLAScatalog(sa_flightline_paths)
  
  # survey area puffern für bessere Resultate des CHM
  cat("start Puffer")
  sa_buffer <- st_buffer(sa, dist = 30)
  cat("...fertig\n")
  
  # las-ctg clippen
  cat("Start Clippen")
  start_time_clip <- now()
  laz_clipped <- clip_roi(laz, sa_buffer)
  cat(paste0("Clip fertig in ", round(as.numeric(difftime(now(), start_time_clip, units = "mins")), 2), " min\n"))
  
  # Neu klassifizieren (da einige Teile der Punktwolke nicht klassifiziert sind, ist die anshcliessende Normalisierung unbrauchbar)
  laz_clipped@data$Classification <- 0L
  
  # Boden und Noise neu Klassifizieren
  # Bodenklassifizierung: 
  #   - res = Zellengrösse um Seed-Punkte zu finden. 20 empfohlen für Wald
  #   - ptd() kann mit outliern umgehen, weshalb Noise-Klassifizierung erst nach der Bodenklassifikation ausgeführt wird
  #   - angle/distance/spacing wurden beim Standard (30,2,0.25) belassen
  cat("Start Bodenklassifizieren")
  laz_clipped <- classify_ground(laz_clipped, ptd(res = 20))
  cat("...fertig\n")
  
  # Noise klassifizieren und herausfiltern
  cat("Start Noise klassifizieren und filtern")
  laz_clipped <- classify_noise(laz_clipped, sor())
  laz_clipped <- filter_poi(laz_clipped, Classification != 18L & Classification != 7L)
  cat("...fertig\n")
  
  # Punktwolke normalisieren
  cat("Start Punktwolke normalisieren ... ")
  start_time_norm <- now()
  nlaz_clipped <- normalize_height(laz_clipped, tin(extrapolate = knnidw(1,1,50)))  # Punktwolke normalisieren mit schnellerem tin()-algorithmus und günstiger extrapolation (nur 1 Nachbar)
  cat(paste0("Normalisierung fertig in ", round(as.numeric(difftime(now(), start_time_norm, units = "mins")), 2), " min\n"))
  
  # normalisiertes und reklassifiziertes laz-file abspeichern
  cat("Start Schreibvorgang laz-clip")
  save_lazfile_path <- file.path(write_path, paste0("buffered_clip_",i,".laz"))
  writeLAS(nlaz_clipped, save_lazfile_path, index = TRUE)
  cat("...fertig\n")

  # CHM-Raster erstellen (einfacher point-to-raster algorithmus)
  cat("CHM-Raster rechnen")
  chm <- rasterize_canopy(nlaz_clipped, res = 1, algorithm = p2r())
  cat("...fertig\n")
  
  # tif-file abspeichern
  cat("Start Schreibvorgang CHM.tif")
  save_tiffile_path <- gsub("\\.laz$", ".tif",save_lazfile_path)
  writeRaster(chm, save_tiffile_path, overwrite = TRUE)
  cat(paste0("... fertig mit Untersuchungsgebiet ", i, " in ", round(as.numeric(difftime(now(), start_time_loop, units = "mins")), 2), " min\n"))
}

print(paste0("Gesamte Rechenzeit: ", round(as.numeric(difftime(now(), start_time, units = "mins")), 2), " min"))

