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

#-----------------------------------------------------
# BFF - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Biodiversitätsförderflächen (BFF) as its own
# geopackage, all sharing the same two layers - union them into a single
# Switzerland-wide geopackage.

bff_files <- list.files(bff_raw_path, pattern = "\\.gpkg$", full.names = TRUE)
bff_layers <- st_layers(bff_files[1])$name

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(bff_path)) unlink(bff_path)

for (layer in bff_layers) {

  message("Merging layer ", layer)

  merged <- bff_files %>%
    lapply(st_read, layer = layer, quiet = TRUE) %>%
    bind_rows()

  st_write(
    merged,
    dsn = bff_path,
    layer = layer,
    append = FALSE
  )
}
