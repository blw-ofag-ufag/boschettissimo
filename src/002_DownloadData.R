#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Loading libraries
library(sf)
library(rstac)
library(dplyr)

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

#*****************************************************
#* STAC download provides the VHM bits that were updated, ie one year only covers part of Switzerland
#* To get a full coverage of Switzerland use the dataset provided by Envidat (manual download): https://www.envidat.ch/#/metadata/vegetation-height-model-lidar-nfi
#* Since this is produced by WSL, local file is used in this project (but corresponds to one in envidat)
#*****************************************************

#-----------------------------------------------------
# DOWNLOAD Landwirtschaftliche Nutzungsflächen Schweiz (Bundesamt für Landwirtschaft BLW, Kantone)
#-----------------------------------------------------

#*****************************************************
#* Not available on STAC, go to https://www.geodienste.ch/services/lwb_nutzungsflaechen and do a manual download. 
#* Since this is produced by BWL, intern files will be used (but correspond to the ones that can be downloaded from geodienste)
#*****************************************************

#-----------------------------------------------------
# DOWNLOAD Lebensraumkarte der Schweiz v1.2: Einzelbäume, -gebüsche, -sträuchern und -hecken ausserhalb des Waldes v1.0
#-----------------------------------------------------

#*****************************************************
#* Not available on STAC
#* To get a full coverage of Switzerland use the dataset provided by Envidat (manual download): https://www.envidat.ch/#/metadata/the-habitat-map-of-switzerland-v1_2-2025
#* Since this is produced by WSL, local file is used in this project (but corresponds to one in envidat)
#*****************************************************

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

#-----------------------------------------------------
# COMPUTE TLM_EINZELBAUM_GEBUESCH vollständig
#-----------------------------------------------------

#*****************************************************
#* Not available publicly. This dataset is a the raw product of TLM_EINZELBAUM_GEBUESCH, before filtering steps that prduce the final publicly open dataset.
#*****************************************************

# Fetch the study areas
study_areas <- read.csv2(file = paste0(git_root_path,"data/study_areas.csv"), sep=",")

# Get the needed LKS tiles on which the study areas are located
tlm_EBv_layers <- list(
  st_read(paste0(TLM_EBv_input_path,study_areas$lk25_path[1]), layer = "LK1054_LokalMax3D_LIDAR_5m") %>% st_zm(drop = TRUE, what = "ZM") %>% select(HEIGHT,Shape),
  st_read(paste0(TLM_EBv_input_path,study_areas$lk25_path[2]), layer = "LokalMax3D_LIDAR_LK1204_5m") %>% st_zm(drop = TRUE, what = "ZM") %>% select(HEIGHT,Shape),
  st_read(paste0(TLM_EBv_input_path,study_areas$lk25_path[3]), layer = "LK1052_LokalMax3D_LIDAR_5m") %>% st_zm(drop = TRUE, what = "ZM") %>% select(HEIGHT,Shape),
  st_read(paste0(TLM_EBv_input_path,study_areas$lk25_path[4]), layer = "LokalMax3D_LIDAR_LK1203_5m") %>% st_zm(drop = TRUE, what = "ZM") %>% select(HEIGHT,Shape),
  st_read(paste0(TLM_EBv_input_path,study_areas$lk25_path[5]), layer = "LK1051_LokalMax3D_LIDAR_5m") %>% st_zm(drop = TRUE, what = "ZM") %>% select(HEIGHT,Shape),
  st_read(paste0(TLM_EBv_input_path,study_areas$lk25_path[6]), layer = "LK1151_LokalMax3D_LIDAR_5m") %>% st_zm(drop = TRUE, what = "ZM") %>% select(HEIGHT,Shape)
)
tlm_EBv <- do.call(rbind, tlm_EBv_layers)

# Export the combined layers
st_write(tlm_EBv,TLM_EBv_path)

#-----------------------------------------------------
# ALS data
#-----------------------------------------------------
#*****************************************************
#* TODO
#*****************************************************

