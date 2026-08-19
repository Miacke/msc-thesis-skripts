library(lidR)
library(terra)

# LAS catalog erstellen

ctg <- readLAScatalog(r"(A:\11_MasterThesis\01_DefStruktur\02_Data\04_CHM\LAS-FILES)")
opt_independent_files(ctg) <- TRUE  # Gebiete sind disjunkt, es wird kein Puffer benötigt, weshalb catalog die Files einzeln behandelt (wie eine for-schleife)

# height normalization

opt_output_files(ctg) <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\04_CHM\LAS-FILES\{*}_n)"
nctg <- normalize_height(ctg, knnidw())

# canopy height model

opt_output_files(nctg) <- r"(A:\11_MasterThesis\01_DefStruktur\02_Data\04_CHM\TIF-FILES\{*}_chm)"
chm <- rasterize_canopy(nctg, res = 1, algorithm = p2r())

