# ============================================================
# 01_Check_LAZ_Files.R
# ============================================================
# Beschreibung:
#   Checkt LAZ-Files mittels lidR-Funktion "las_check"
#
# Vorgehen:
#   - Iteriert durch die Ordner und sucht/checkt alle laz-files

# Input:
#   - Grundordner mit allen laz-files
#
# Output:
#   - Log-file
#
# Autor:       Mirco Ackermann
# Datum:       31.07.2026
# Projekt:     UNIGIS MasterThesis
# ============================================================

library(lidR)
library(sf)

# Pfad zum ordner setzen und File-Liste erstellen
laz_folder <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\03_ALS-Data\BEV_Data)"
laz_files <- list.files(laz_folder, pattern = ".laz$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

# Files in LAS-Catalog laden
laz_catalog <- readLAScatalog(laz_files)

# Survey Areas aus fGDB einlesen
survey_areas_fGDB_path <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\02_Survey_areas\Datenbestellung_AT.gdb)"
survey_areas_layer <- "survey_area_EPSG_25833_5_bearbeitet"
survey_areas <- st_read(survey_areas_fGDB_path, layer = survey_areas_layer)

# Las_catalog auf survey-areas clippen auf Platte, nicht auf RAM
opt_output_files(laz_catalog) <- r"(A:\11_MasterThesis\01_DefStruktur\01_Skripts\01_MSC_Thesis_Skripts\R\temp_data\roi_{ID})"  # speichert alle outputs von catalog-operationen in files anstatt im memory (was standard wäre)
laz_clipped <- clip_roi(laz_catalog, survey_areas)

# deep-Check durchführen und in Konsolenausgabe in log-file speichern
options(lidR.progress = FALSE)  # Fortschrittsbalken deaktivieren, da sonst log-file, welches die terminal-ausgaben beinhaltet,  unschön wird
con <- file("las_check_log.txt")

sink(con)
sink(con, type="message")  # las_check "deep" schreibt über anderen Kanal als las_check, deshalb mit sink beide abfangen.

las_check(laz_clipped)  # Header Check
las_check(laz_clipped, deep = TRUE)  # deep-check auf Datenebene

sink(type="message")
sink()
close(con)


