# ============================================================
# 02_handle_overlaps_clip.R
# ============================================================
# Beschreibung:
#   Baut auf 01_Check_LAZ_Files.R auf
#   Prüft was es mit der Warnung "Some tiles seem to overlap each other" der las_check-Funktion auf sich hat
#
# Vorgehen:
#   - 

# Input:
#   - Grundordner mit allen geclippten laz-files
#
# Output:
#   - Log-file
#
# Autor:       Mirco Ackermann
# Datum:       03.08.2026
# Projekt:     UNIGIS MasterThesis
# ============================================================

library(lidR)

# Pfad zum ordner setzen und File-Liste erstellen
las_clipped_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data\LAS_clipped_surveyarea)"
las_clipped_files <- list.files(las_clipped_folder, pattern = ".las$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
las_clipped_catalog <- readLAScatalog(las_clipped_files)

