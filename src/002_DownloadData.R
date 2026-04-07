#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Loading libraries
library(sf)
library(rstac)

# Sourcing initialization code (paths and such) 
source("001_Initialization.R")

#-----------------------------------------------------
# STAC related functions
#-----------------------------------------------------
# Following and adapting tutorial https://stacspec.org/en/tutorials/1-download-data-using-r/

# Get stac collections - Check out in browser, easier to read
# https://data.geo.admin.ch/api/stac/v0.9/collections

# Get stac source
stac_source <- rstac::stac(
  "http://data.geo.admin.ch/api/stac/v0.9/"
)
stac_source

# Function to get STAC data from swisstopo
getSwisstopoSTAC <- function(collectionName, timespan){
  
  # Prepare and get stac info concerning collectionName layer, precising wanted timespan
  stac_query <- rstac::stac_search(
    q = stac_source,
    collections = collectionName,
    datetime = timespan
  )
  stac_query
  
  executed_stac_query <- rstac::get_request(stac_query)
  executed_stac_query
  
  # Downloading corresponding assets
  for (item in executed_stac_query$features) {
    print(item$id)
    rstac::assets_download(
      item = item,
      assets_names = NULL,
      destdir = NULL, 
      overwrite = TRUE
    )
  }
}

#-----------------------------------------------------
# DOWNLOAD Vegetation Height Model
#-----------------------------------------------------

# Download the data
getSwisstopoSTAC("ch.bafu.landesforstinventar-vegetationshoehenmodell_lidar","2025-01-01/2025-12-31")

# Move it from the current folder (where it is downlaoded by default, even when setting destdir) to the data folder
dir.create(paste0(orig_data_path,"VHM/"), recursive = TRUE, showWarnings = FALSE)
file.copy(
  from = paste0(getwd(),"/ch.bafu.landesforstinventar-vegetationshoehenmodell_lidar/landesforstinventar-vegetationshoehenmodell_lidar_2025/landesforstinventar-vegetationshoehenmodell_lidar_2025_2056.tif"),
  to   = paste0(orig_data_path,"VHM/","landesforstinventar-vegetationshoehenmodell_lidar_2025_2056.tif"),
  overwrite = TRUE
)

# Delete download folder
unlink(paste0(getwd(),"/ch.bafu.landesforstinventar-vegetationshoehenmodell_lidar/"), recursive = TRUE, force = TRUE)

# Data saved  in 001_Initialization.R under:
# VHM_path <- paste0(orig_data_path,"VHM/landesforstinventar-vegetationshoehenmodell_lidar_2025_2056.tif")

#-----------------------------------------------------
# DOWNLOAD Landwirtschaftliche Nutzungsflächen Schweiz (Bundesamt für Landwirtschaft BLW, Kantone)
#-----------------------------------------------------

# Not available on STAC, go to https://www.geodienste.ch/services/lwb_nutzungsflaechen and do a manual download. 
# Save the result under D:/BLW/ORIG_DATA/BLW/nutzungsflaechen.gpkg

# First test, download for canton TG only

# Data saved  in 001_Initialization.R under:
# NF_path <- D:/BLW/ORIG_DATA/BLW/nutzungsflaechen.gpkg

#-----------------------------------------------------
# DOWNLOAD Lebensraumkarte der Schweiz v1.2: Einzelbäume, -gebüsche, -sträuchern und -hecken ausserhalb des Waldes v1.0
#-----------------------------------------------------

# Not available on STAC, nor on map.geo.admin

#-----------------------------------------------------
# DOWNLOAD TLM_EINZELBAUM_GEBUESCH
#-----------------------------------------------------

# Download the data
getSwisstopoSTAC("ch.swisstopo.swisstlm3d","2025-01-01/2025-12-31")

# Move it from the current folder (where it is downlaoded by default, even when setting destdir) to the data folder
dir.create(paste0(orig_data_path,"TLM/"), recursive = TRUE, showWarnings = FALSE)
unzip(
  zipfile = paste0(getwd(),"/ch.swisstopo.swisstlm3d/swisstlm3d_2025-03/swisstlm3d_2025-03_2056_5728.gpkg.zip"),
  exdir   = paste0(orig_data_path,"TLM/")
)

# Delete download folder
unlink(paste0(getwd(),"/ch.swisstopo.swisstlm3d/"), recursive = TRUE, force = TRUE)

# Data saved  in 001_Initialization.R under:
# TLM_EB_path <- paste0(orig_data_path,"TLM/SWISSTLM3D_2025.gpkg")