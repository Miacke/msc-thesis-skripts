# ============================================================
# 05_gebietsmetriken.R
# ============================================================
# Beschreibung:
#   Laedt pro Untersuchungsgebiet den passenden Ausschnitt des amtlichen
#   ALS-Gelaendemodells (BEV, 1 m) und berechnet daraus Gebietsmetriken:
#   Deckungsgrad, Bestandeshoehe, Hangneigung, Exposition, Gelaenderauheit.
#
# Input:
#   - Untersuchungsgebiete (fGDB)
#   - CHM-Raster pro Gebiet aus Skript 01
#   - ALS DTM 1 m des BEV (wird automatisch bezogen)
#
# Output:
#   - gebiete_df: eine Zeile pro Untersuchungsgebiet
#   - DTM-Ausschnitt pro Gebiet als .tif
#
# Autor:       Mirco Ackermann
# Datum:       02.10.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieses Skripts wurde generative KI (Claude, Anthropic) unterstuetzend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klaerung von Funktionialitaet und
#     allfaelliger Besonderheiten einzelner Funktionen
#   - Darueber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstuetzend eingesetzt
#   - Von der KI generierte Vorschlaege, welche mindestens in Ansaetzen in das Skript einflossen,
#     sind allesamt vom Autor geprueft, vollstaendig verstanden und in dessen Verantwortung
# ============================================================

library(sf)
library(terra)
library(dplyr)

# ============================================================
# Pfade und Einstellungen
# ============================================================

survey_area_fgdb_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\02_Survey_areas\SURVEY_AREA_MSCTHESIS.gdb)"
survey_area_fcname    <- "survey_area_final"

chm_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_01)"
dtm_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\04_DTM)"
out_path <- r"(A:\11_MasterThesis\01_DefStruktur\07_Auswertungen\05_gebietsmetriken)"

dir.create(dtm_path, recursive = TRUE, showWarnings = FALSE)
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

# Stichtag der BEV-Kacheln (jaehrlich aktualisiert, im BEV-Datenkatalog nachsehen)
bev_stichtag <- "20250915"

# Schwelle fuer den Deckungsgrad (Hoehe in m ueber Grund)
hoehe_schwelle <- 2

survey_area <- st_read(survey_area_fgdb_path, layer = survey_area_fcname)

# ============================================================
# Funktion 1: DTM-Ausschnitt fuer ein Gebiet beziehen
# ============================================================

hole_dtm <- function(sa, stichtag){
  
  # Die BEV-Kacheln liegen in EPSG:3035 und sind 50 km gross.
  # Der Kachelname enthaelt die Koordinaten der suedwestlichen Ecke.
  sa_3035 <- st_transform(sa, 3035)
  bb      <- st_bbox(sa_3035)
  
  kachel_x <- seq(floor(bb["xmin"] / 50000), floor(bb["xmax"] / 50000)) * 50000
  kachel_y <- seq(floor(bb["ymin"] / 50000), floor(bb["ymax"] / 50000)) * 50000
  
  ausschnitte <- list()
  
  for (x in kachel_x){
    for (y in kachel_y){
      
      name <- paste0("ALS_DTM_CRS3035RES50000mN", formatC(y, width = 7, flag = "0"),
                     "E", formatC(x, width = 7, flag = "0"), ".tif")
      url  <- paste0("/vsicurl/https://data.bev.gv.at/download/ALS/DTM/", stichtag, "/", name)
      
      # Nur den benoetigten Ausschnitt lesen, nicht die ganze Kachel herunterladen
      kachel <- try(rast(url), silent = TRUE)
      if (inherits(kachel, "try-error")) next
      
      ausschnitte[[name]] <- crop(kachel, ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))
    }
  }
  
  if (length(ausschnitte) == 0) stop("Keine BEV-Kachel fuer dieses Gebiet erreichbar")
  
  dtm <- if (length(ausschnitte) == 1) ausschnitte[[1]] else do.call(merge, unname(ausschnitte))
  
  # In das CRS der Untersuchungsgebiete bringen
  project(dtm, crs(vect(sa)))
}

# ============================================================
# Funktion 2: Metriken fuer ein Gebiet berechnen
# ============================================================

berechne_metriken <- function(sa, chm, dtm, nummer, schwelle){
  
  flaeche <- vect(sa)
  
  chm <- mask(crop(chm, flaeche), flaeche)
  dtm <- mask(crop(dtm, flaeche), flaeche)
  
  neigung <- terrain(dtm, v = "slope",  unit = "degrees")
  expo    <- terrain(dtm, v = "aspect", unit = "degrees")
  rauheit <- terrain(dtm, v = "TRI")
  
  h <- values(chm,     na.rm = TRUE)
  s <- values(neigung, na.rm = TRUE)
  a <- values(expo,    na.rm = TRUE)
  
  # Die Exposition ist eine Richtungsgroesse: Mittelwert ueber Sinus und Kosinus.
  # In flachem Gelaende ist sie beliebig, deshalb dort ausgeschlossen.
  geneigt <- s >= 2
  bogen   <- a[geneigt] * pi / 180
  sin_m   <- mean(sin(bogen))
  cos_m   <- mean(cos(bogen))
  
  data.frame(
    SA_Nr          = nummer,
    flaeche_ha     = as.numeric(st_area(sa)) / 10000,
    
    deckungsgrad   = mean(h > schwelle),            # Anteil Flaeche mit Vegetation ueber der Schwelle
    hoehe_mittel   = mean(h),
    hoehe_p99      = as.numeric(quantile(h, 0.99)),
    hoehe_sd       = sd(h),
    
    hang_mittel    = mean(s),
    hang_min       = min(s),
    hang_max       = max(s),
    hang_sd        = sd(s),
    rauheit_tri    = mean(values(rauheit, na.rm = TRUE)),
    
    expo_mittel    = (atan2(sin_m, cos_m) * 180 / pi) %% 360,
    expo_konz      = sqrt(sin_m^2 + cos_m^2),       # 0 = alle Richtungen, 1 = einheitlicher Hang
    anteil_flach   = mean(!geneigt)
  )
}

# ============================================================
# Verarbeitung aller Untersuchungsgebiete
# ============================================================

gebiete_df <- data.frame()

for (i in survey_area$SA_Nr){
  
  if (i != 2){
    next
  }
  
  print(paste("Start Gebiet", i))
  
  sa <- dplyr::filter(survey_area, SA_Nr == i)
  
  # DTM beziehen und als Ausschnitt ablegen
  dtm_datei <- file.path(dtm_path, paste0("dtm_", i, ".tif"))
  
  if (file.exists(dtm_datei)) {
    dtm <- rast(dtm_datei)
  } else {
    dtm <- hole_dtm(sa, bev_stichtag)
    writeRaster(dtm, dtm_datei, overwrite = TRUE)
  }
  
  # CHM aus Skript 01
  chm <- rast(file.path(chm_path, paste0("buffered_clip_", i, ".tif")))
  
  gebiete_df <- rbind(gebiete_df, berechne_metriken(sa, chm, dtm, i, hoehe_schwelle))
  
  print(paste("... fertig mit Gebiet", i))
}

write.csv(gebiete_df, file.path(out_path, "gebietsmetriken.csv"), row.names = FALSE)
print(gebiete_df)