# Load libraries
library(sf)
library(terra)

# List study area data from geopackage
#--------------------------------------

# Get all geopackages paths
sa_vec_path <- list.files(
  study_area_data_path, 
  pattern = "\\.gpkg$", 
  full.names = T)

# Specify the geopackages paths and list the available layers
sa_vec <- lapply(sa_vec_path, function(gpkg) {
  list(
    path   = gpkg,
    layers = st_layers(gpkg)$name
  )
})

# Name the list elements as the id of the study area
names(sa_vec) <- gsub(".gpkg","",basename(sa_vec_path))


# List study area data from raster - SWISS2
#--------------------------------------

# Get all raster paths
sa_tif_S2_path <- list.files(
  study_area_data_path,
  pattern = "\\_S2_wgs84.tif$",
  full.names = T)

# Load the rasters
sa_tif_S2 <- lapply(sa_tif_S2_path, function(tif) {
  rast(tif)
})

# Name the list elements as the id of the study area
names(sa_tif_S2) <- gsub("_vhm_S2_wgs84.tif","",basename(sa_tif_S2_path))
