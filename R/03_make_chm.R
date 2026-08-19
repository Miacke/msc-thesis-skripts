# ============================================================
# 02_make_chm.R
# ============================================================
# Beschreibung:
#   Nimmt LAS-Files pro ROi, normalisiert die Punktwolke, erstellt ein CHM und speichert ab
#
# Input:
#   - Grundordner mit allen las-files auf das roi zugeschnitten
#   - Grundordner wo alle tifs abgespeichert werden sollen
#
# Output:
#   - CHM-Rasterfile pro ROI
#
# Autor:       Mirco Ackermann
# Datum:       17.08.2026
# Projekt:     UNIGIS MasterThesis
# ============================================================

library(lidR)
library(terra)

# Pfade setzen
las_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\04_CHM\LAS-FILES)"
las_files <- list.files(las_folder, full.names = TRUE)

for (file in las_files) {  # zu debugging-zwecken wird kein catalog verwendet sondern durch jedes file iteriert
  
  # Las-file einlesen
  las <- readLAS(file)
  las <- filter_duplicates(las)  # Mehrere Files weisen dublizierte Groundpoints auf, was die Normalisierung der Punktwolke möglicherweise stört. Diese werden herausgefiltert.
  print(paste0("Start bei file ", file))
  
  # Punktwolke normalisieren
  nlas <- normalize_height(las, tin(extrapolate = knnidw(1,1,50)))  # Punktwolke normalisieren mit schnellerem tin()-algorithmus und günstiger extrapolation (nur 1 Nachbar)
  print("Punktwolke normalisiert")
  
  # CHM-Raster erstellen (einfacher point-to-raster algorithmus)
  chm <- rasterize_canopy(nlas, res = 1, algorithm = p2r())
  print("CHM erstellt")
  
  # tif-file abspeichern
  tif_file <- gsub("LAS-FILES", "TIF-FILES", gsub(".las", ".tif",file))
  writeRaster(chm, tif_file)
  print(paste0("Raster geschrieben ",tif_file))
}


