#-----------------------------------------------------
# Single time data preparation
#-----------------------------------------------------
# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")
library(sf)
library(dplyr)

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

#*******************************************************
# BLW DATA 2025 - PROXY SWISS2
#*******************************************************

#-----------------------------------------------------
# BFF - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Biodiversitätsförderflächen (BFF) as its own
# geopackage, all sharing the same two layers - union them into a single
# Switzerland-wide geopackage.

bff_2025_files <- list.files(bff_2025_raw_path, pattern = "\\.gpkg$", full.names = TRUE)
bff_2025_layers <- st_layers(bff_2025_files[1])$name

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(bff_2025_path)) unlink(bff_2025_path)

for (layer in bff_2025_layers) {

  message("Merging layer ", layer)

  merged <- bff_2025_files %>%
    lapply(st_read, layer = layer, quiet = TRUE) %>%
    bind_rows()

  st_write(
    merged,
    dsn = bff_2025_path,
    layer = layer,
    append = FALSE
  )
}

#-----------------------------------------------------
# BWE - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Bewirtschaftungseinheiten (BWE) as its own
# geopackage - union the "bewirtschaftungseinheit" layer into a single
# Switzerland-wide geopackage.

bwe_2025_files <- list.files(bwe_2025_raw_path, pattern = "\\.gpkg$", full.names = TRUE)

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(bwe_2025_path)) unlink(bwe_2025_path)

message("Merging layer bewirtschaftungseinheit")

merged <- bwe_2025_files %>%
  lapply(st_read, layer = "bewirtschaftungseinheit", quiet = TRUE) %>%
  bind_rows()

st_write(
  merged,
  dsn = bwe_2025_path,
  layer = "bewirtschaftungseinheit",
  append = FALSE
)


#-----------------------------------------------------
# NF - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Nutzungsflaechen (NF) as its own
# geopackage - union the "nutzungsflaechen" layer into a single
# Switzerland-wide geopackage.

nf_2025_files <- list.files(nf_2025_raw_path, pattern = "nutzungsflaechen.*\\.gpkg$", full.names = TRUE)

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(nf_2025_path)) unlink(nf_2025_path)

message("Merging layer nutzungsflaechen")

merged <- nf_2025_files %>%
  lapply(st_read, layer = "nutzungsflaechen", quiet = TRUE) %>%
  bind_rows()

st_write(
  merged,
  dsn = nf_2025_path,
  layer = "nutzungsflaechen",
  append = FALSE
)

#*******************************************************
# BLW DATA 2021 - PROXY SWISS1
#*******************************************************

