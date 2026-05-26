#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(lidR)
library(terra)
library(sf)
library(data.table)
library(dplyr)
library(purrr)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# Get study areas ALS file
ALS_sas <- read.csv2(file = "data/ALS_study_areas.csv", sep=",")

# Get the study areas
sas <- read.csv2(file = "data/study_areas.csv", sep=",")

# Small function to clean the segmented polygons
rast_to_polys <- function(r){
  r %>%
    as.polygons(dissolve = TRUE) %>%
    st_as_sf() %>%
    mutate(area_m2 = as.numeric(st_area(.))) %>%   # Calculate area of each polygon
    filter(area_m2 > 1) %>%    # Keep polygons above minimum area
    select(-area_m2)
}

#-----------------------------------------------------
# Tree segementation
#-----------------------------------------------------

# Loop through the study areas
for(sa in sas$id){
  
  # Filter the LAS files associated to the study area
  ALS_sa <- ALS_sas[which(ALS_sas$sa_id == sa),]
  
  # Load the VHM associated to the study area
  vhm_sa <- rast(paste0(study_area_data_path,sa,"_vhm_S2.tif"))
  
  # Have VHM in memory (for crown delimitation and also segmentation of big raster (engelswilen))
  vhm_sa <- toMemory(vhm_sa)
  
  # Perform the VHM smoothings
  #-----------------------------------------------------
  # Compute the different smoothing
  vhm_sa_smooth3_mean <- focal(vhm_sa, w = matrix(1,3,3), fun = mean)
  vhm_sa_smooth5_mean <- focal(vhm_sa, w = matrix(1,5,5), fun = mean)
  vhm_sa_smooth3_med <- focal(vhm_sa, w = matrix(1,3,3), fun = median)
  vhm_sa_smooth5_med <- focal(vhm_sa, w = matrix(1,5,5), fun = median)
  vhm_sa_smooth3_max <- focal(vhm_sa, w = matrix(1,3,3), fun = max)
  vhm_sa_smooth5_max <- focal(vhm_sa, w = matrix(1,5,5), fun = max)
  
  # Perform the segmentation
  #-----------------------------------------------------
  # Compute the different watershed segmentations
  crowns_ws_mw3_mean <- lidR::watershed(vhm_sa_smooth3_mean)() %>% rast_to_polys() 
  crowns_ws_mw5_mean <- lidR::watershed(vhm_sa_smooth5_mean)() %>% rast_to_polys()
  crowns_ws_mw3_med <- lidR::watershed(vhm_sa_smooth3_med)() %>% rast_to_polys()
  crowns_ws_mw5_med <- lidR::watershed(vhm_sa_smooth5_med)()%>% rast_to_polys()
  crowns_ws_mw3_max <- lidR::watershed(vhm_sa_smooth3_max)() %>% rast_to_polys()
  crowns_ws_mw5_max <- lidR::watershed(vhm_sa_smooth5_max)()%>% rast_to_polys()
  
  # Load in LN Data and filter only crowns on agricultural surface 
  # TEMP - to be moved to sa geopackage
  #-----------------------------------------------------
  # Set extent
  sa_ext <- ext(
    sas$upper_left_e[which(sas$id == sa)],
    sas$lower_right_e[which(sas$id == sa)],
    sas$lower_right_n[which(sas$id == sa)],
    sas$upper_left_n[which(sas$id == sa)]
  )
  
  # Convert to correct format
  sa_wkt <- st_as_text(st_geometry(st_as_sf(as.polygons(sa_ext))))
  
  # Read BLW geoms
  ln_sa <- st_read(LN_2025_path, layer="Landwirtschaftliche_Nutzungsflaechen_Schweiz_2025", wkt_filter = sa_wkt) 
  
  # TEMP - Just union all the geoms, later keep only relevant ones
  ln_filter <- st_union(ln_sa)
  
  # Filter the crowns to keep only what is on agricultural land
  #-----------------------------------------------------
  crowns_ws_mw3_mean <- crowns_ws_mw3_mean[st_within(crowns_ws_mw3_mean, ln_filter, sparse = FALSE), ]
  crowns_ws_mw5_mean <- crowns_ws_mw5_mean[st_within(crowns_ws_mw5_mean, ln_filter, sparse = FALSE), ]
  crowns_ws_mw3_med <- crowns_ws_mw3_med[st_within(crowns_ws_mw3_med, ln_filter, sparse = FALSE), ]
  crowns_ws_mw5_med <- crowns_ws_mw5_med[st_within(crowns_ws_mw5_med, ln_filter, sparse = FALSE), ]
  crowns_ws_mw3_max <- crowns_ws_mw3_max[st_within(crowns_ws_mw3_max, ln_filter, sparse = FALSE), ]
  crowns_ws_mw5_max <- crowns_ws_mw5_max[st_within(crowns_ws_mw5_max, ln_filter, sparse = FALSE), ]

  # Identify the tree tops
  #-----------------------------------------------------
  # Following Silva 2016 methodology
  ttops <- locate_trees(vhm_sa_smooth3_mean,lmf(ws = 7, hmin = 1, shape = "circular")) %>% st_zm(drop = TRUE, what = "ZM") 

  # Export results
  #-----------------------------------------------------
  write_sf(crowns_ws_mw3_mean, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns_ws_mw3_mean", append = FALSE)
  write_sf(crowns_ws_mw5_mean, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns_ws_mw5_mean", append = FALSE)
  write_sf(crowns_ws_mw3_med, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns_ws_mw3_med", append = FALSE)
  write_sf(crowns_ws_mw5_med, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns_ws_mw5_med", append = FALSE)
  write_sf(crowns_ws_mw3_max, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns_ws_mw3_max", append = FALSE)
  write_sf(crowns_ws_mw5_max, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns_ws_mw5_max", append = FALSE)
  write_sf(ttops, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="ttops", append = FALSE)
  write_sf(ln_filter, paste0(study_area_data_path,sa,".gpkg"), layer="nutzungsflaechen", append = FALSE)
  
  # TEMP - First kind of analysis of different segmentation methods
  #-----------------------------------------------------
  
  # Combine all crowns in one list
  crowns <- list(
    mw3_mean = crowns_ws_mw3_mean,
    mw5_mean = crowns_ws_mw5_mean,
    mw3_med  = crowns_ws_mw3_med,
    mw5_med  = crowns_ws_mw5_med,
    mw3_max  = crowns_ws_mw3_max,
    mw5_max  = crowns_ws_mw5_max
  )
  
  # Add id and method that produced feature
  crowns <- lapply(names(crowns), function(nm){
    x <- st_as_sf(crowns[[nm]])
    x$crown_id <- seq_len(nrow(x))
    x$method <- nm
    x
  })
  
  names(crowns) <- c(
    "mw3_mean",
    "mw5_mean",
    "mw3_med",
    "mw5_med",
    "mw3_max",
    "mw5_max"
  )
  
  # Create a template to rasterize the segmented polygons
  template <- rast(vhm_sa)
  
  # Rasterize the different segmentation outputs
  rasters <- lapply(crowns, function(x){
    v <- vect(x)
    rasterize(
      v,
      template,
      field = "crown_id",
      background = NA
    )
  })
  
  # Number of times a pixel was considered to be a tree crown
  # ---------------
  
  # Does the raster have a value?
  binary_rasters <- lapply(rasters, function(r){
    classify(r, cbind(NA, NA, 0), others = 1)
  })
  
  # Sum over all rasters
  # 0 = no method detected crown
  # 6 = all methods detected a crown
  is_crown <- sum(rast(binary_rasters), na.rm = TRUE)
  
  # Export results
  #-----------------------------------------------------
  writeRaster(is_crown, filename=paste0(study_area_data_path,sa,"_is_crown.tif"), overwrite=TRUE)
  
}



