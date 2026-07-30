#-----------------------------------------------------
# Single time data preparation
#-----------------------------------------------------
# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# DHM25 - reproject from LV03 (no CRS tag) to LV95
#-----------------------------------------------------

dir.create(dirname(dhm25_path), recursive = TRUE, showWarnings = FALSE)

cmd <- sprintf(
  'gdalwarp -s_srs EPSG:21781 -t_srs EPSG:2056 -r bilinear -co COMPRESS=LZW -co TILED=YES -overwrite "%s" "%s"',
  dhm25_raw_path,
  dhm25_path
)
system(cmd)
