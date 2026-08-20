# ============================================================
# 00_LAZ_Checks.R
# ============================================================
# Beschreibung:
#   Diverse Checks der laz-files, soll nicht komplett laufen, sondern nur teilweise
#
# Input:
#   - div
#
# Output:
#   - div
#
# Autor:       Mirco Ackermann
# Datum:       20.08.2026
# Projekt:     UNIGIS MasterThesis
#
# Nutzung generativer KI bei der Erstellung dieses Skripts:
#   - Bei der Entwicklung dieser Skripts wurde generative KI (Claude, Anthropic, Opus 4.8) unterstützend eingesetzt
#   - Der Einsatz betraf die Fehlersuche (Debugging) sowie die Klärung von Funktionialität und allfälliger Besonderheiten einzelner Funktionen
#   - Darüber hinaus wurde die KI als Dialogpartner beim Brainstorming unterstützend eingesetzt
#   - Von der KI generierte Vorschläge, welche mindestens in Ansätzen in das Skript einflossen, sind allesamt vom Autor geprüft, vollständig verstanden und in dessen Verantwortung 
# ============================================================

# ------------------------------------------------------------
# Klassifizierungscheck
# ------------------------------------------------------------

library(lidR)
library(sf)
library(lubridate)

folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_ClipROI_Normalized)"
laz_files <- list.files(folder, pattern = r"(\.laz$)", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
# failed_areas <- c()
counter <- 0

for (file in laz_files){
  counter <- counter + 1
  
  print(paste0(counter, "/", length(laz_files)))
  area <- basename(dirname(dirname(file)))
  tmp_laz <- readLAS(file)
  tmp_laz_filtered <- filter_poi(tmp_laz, Classification %in% c(6,8,10,11,12,13,14,15,16,17,19))
  if (npoints(tmp_laz_filtered) > 0){
    # failed_areas <- union(failed_areas, area)
    print(paste0("File: ", file, " Gebiet: ", area, " mit Klassifikation: ", unique(tmp_laz_filtered$Classification)))
  }
}


# ------------------------------------------------------------
# Geschwindigkeitscheck tile_size lasindex
# Befund 
#   -.lax file macht sinn, Rund 12* schneller bei default .lax als ohne .lax
#   - tile_size vergrössern macht sinn: 250/500 2* schneller als default wert
# ------------------------------------------------------------

# Teste Geschwindigkeit
start_time <- now()
laz_ctg <- readALSLAScatalog(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data\Ennstal\PC_POSTClass\A2025460_14025_265_POST_ETRS89.laz)")

# kleine bbox zum testen erstellen in mitte der bbox_laz
bbox <- st_bbox(laz_ctg)
x_mid <- ((bbox[["xmax"]] - bbox[["xmin"]])/2) + bbox[["xmin"]]
y_mid <- ((bbox[["ymax"]] - bbox[["ymin"]])/2) + bbox[["ymin"]]
bbox_mid <- c(xmin = x_mid - 50, ymin = y_mid - 50, xmax = x_mid + 50, ymax = y_mid + 50)
bbox_polygon <- st_as_sfc(st_bbox(bbox_mid))

laz_ctg_clip <- clip_roi(laz_ctg, bbox_polygon) # clippen
laz_ctg_clip$Classification <- 0L  # klassifizierung löschen
laz_ctg_clip <- classify_ground(laz_ctg_clip, ptd(res = 20))  # boden klassifizieren
laz_ctg_clip <- classify_noise(laz_ctg_clip, sor())  # noise klassifizieren
laz_ctg_clip <- filter_poi(laz_ctg_clip, Classification != 18L & Classification != 7L)  # noise herausfiltern
laz_ctg_clip <- normalize_height(laz_ctg_clip, knnidw())
end_time <- now()
print(paste0("Benötigte Zeit: ", round(as.numeric(difftime(end_time, start_time, units = "secs")), 2), " sek"))

# ------------------------------------------------------------
# seed resolution bei ptd testen
# Befund 
#   - Veränderung der resolution bewirkt fast nichts. res=10 hatte weniger als 0.05% mehr Bodenpunkte als res=20
#   - 
# ------------------------------------------------------------

# gebirge und flach einlesen
laz1 <- readLAS(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_ClipROI_Normalized\19\Richtung_1\08010_649.laz)")
laz2 <- readLAS(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data_ClipROI_Normalized\4\Richtung_1\2004_199.laz)")

# klassifizierung löschen
laz1$Classification <- 0L
laz2$Classification <- 0L

# boden pro gebiet mit res=20 und res=10 klassifizieren
las1a <- classify_ground(laz1 , ptd(res = 20, distance = 1))
las1b <- classify_ground(laz1 , ptd(res = 10, distance = 1))
las2a <- classify_ground(laz2 , ptd(res = 20, distance = 1))
las2b <- classify_ground(laz2 , ptd(res = 10, distance = 1))

# bodenpunkte vergleichen
print(paste0("Gebirge/res=20: ",table(las1a$Classification)))
print(paste0("Gebirge/res=10: ",table(las1b$Classification)))
print(paste0("flach/res=20: ",table(las2a$Classification)))
print(paste0("flach/res=10: ",table(las2b$Classification)))


