# ============================================================
# 05_calculate_metrics.R
# ============================================================
# Beschreibung:
#   Füllt Untersuchungsgebiete mit Rasterzellen
#
# Input:
#   - Untersuchungsgebiete
#
# Output:
#   - Rasterzellen innerhalb Untersuchungsgebieten
#
# Autor:       Mirco Ackermann
# Datum:       18.08.2026
# Projekt:     UNIGIS MasterThesis
# ============================================================

library(sf)
library(spatialEco)
library(lidR)

# Untersuchungsgebiete aus fGDB einlesen

sa_f_gdb <- r"(A:\11_MasterThesis\01_DefStruktur\06_GIS\MasterThesis_Datenanalyse\MasterThesis_Datenanalyse.gdb)"
sa_fc <- r"(survey_area_WALD_MANUELL)"
sa <- st_read(sa_f_gdb, layer = sa_fc)  # survey area

# ALS-Daten einlesen

las_testfile <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\04_CHM\LAS-FILES\roi_17.las)"
las_test <- readLAS(las_testfile)
nlas_test <- normalize_height(las_test, tin())


# -------------------------------------------------------------
# Testdurchlauf mit einer Fläche
# Fläche filtern

sa_test <- st_read(sa_f_gdb, query = "SELECT * FROM survey_area_WALD_MANUELL WHERE Nummer_Untersuchungsgebiet = 17")
# -------------------------------------------------------------

# =============================================================
# Gitterzellen erstellen
# =============================================================

# Gitter bilden (ergibt sfc-objekt -> reine Geometrie, ohne Attributentabelle)
grid_15_sfc <- st_make_grid(sa_test, cellsize = 15)  
grid_5_sfc <- st_make_grid(sa_test, cellsize = 5)
grid_15_sfc_selected <- spatial.select(sa_test, grid_15_sfc, predicate = "contains")  # Nur Zellen weiterverarbeiten, welche sich komplett innerhalb des Polygons befinden
grid_5_sfc_selected <- spatial.select(sa_test, grid_5_sfc, predicate = "contains")

# Umwandlung der Gitterzellen in sf-Objekte und Ergänzung der Attributentabelle

grid_5 <- st_sf(geometry = grid_5_sfc_selected)
grid_15 <- st_sf(geometry = grid_15_sfc_selected)

# Untersuchungsgebiet zuweisen
grid_5$Nummer_Untersuchungsgebiet <- sa_test$Nummer_Untersuchungsgebiet
grid_15$Nummer_Untersuchungsgebiet <- sa_test$Nummer_Untersuchungsgebiet

# Eindeutiger Zellenidentifikator
grid_5$ID <- paste(sa_test$Nummer_Untersuchungsgebiet, seq_len(nrow(grid_5)), sep="_")
grid_15$ID <- paste(sa_test$Nummer_Untersuchungsgebiet, seq_len(nrow(grid_15)), sep="_")

# =============================================================
# Metriken berechnen pro Zelle
# =============================================================

# -------------------------------------------------------------
# hmax: 99%-Quantil 

pm_5_hmax <- polygon_metrics(nlas_test, ~quantile(Z, probs = 0.99),geometry = grid_5)
grid_5$hmax <- pm_5_hmax$V1

# -------------------------------------------------------------
# Bodenpenetrationsrate
# Punkte filtern

nlas_test_gp <- filter_ground(nlas_test)
nlas_test_lr <- filter_last(nlas_test)

# Punkte zählen

pm_5_gp <- polygon_metrics(nlas_test_gp, ~length(Z), geometry = grid_5)
grid_5$ground_points <- pm_5_gp$V1
lr_5_gp <- polygon_metrics(nlas_test_lr, ~length(Z), geometry = grid_5)
grid_5$last_returns <- lr_5_gp$V1

# Bodenpenetrationsrate berechnen

grid_5$penetration_rate <- grid_5$ground_points/grid_5$last_returns

# -------------------------------------------------------------
# Kronenpenetrationsrate

pm_5_up <- polygon_metrics(nlas_test_lr, ~sum(Classification != 2L & Z < 0.3 * quantile(Z,0.99)), geometry = grid_5)  # Filter: Nicht Ground-points und Z tiefer als 0.3*hmax. Verwendung des schon gefilterten nlas_test_lr
grid_5$understory_points <- pm_5_up$V1
grid_5$crown_penetration_rate <- grid_5$understory_points/grid_5$last_returns
grid_5
