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
getSwisstopoSTAC <- function(collectionName, timespan, out_path){
  
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
      destdir = out_path, 
      overwrite = TRUE
    )
  }
}

#-----------------------------------------------------
# DOWNLOAD VEGETATION HEIGHT MODEL
#-----------------------------------------------------

getSwisstopoSTAC("ch.bafu.landesforstinventar-vegetationshoehenmodell_lidar","2025-01-01/2025-12-31", orig_data_path)
