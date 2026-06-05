#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(terra)
library(sf)
library(dplyr)
library(future.apply)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# Load the grid for processing 
ALLEMA_Q <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_Q_ref.gpkg")
out_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/PROCESSED_DATA/ALLEMA/"
  
#-----------------------------------------------------
# Segmentation functions
#-----------------------------------------------------

prep_vhm <- function(vhm_path, e_buf){
  
  # Create temp file
  tmpfile <- tempfile("vhm_crop_")
  
  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(e_buf), ymax(e_buf), xmax(e_buf), ymin(e_buf),
    vhm_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt")) %>% 
    toMemory()
  
  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))
  
  return(vhm_cell)
  
}

seg_cell <- function(vhm_cell, e_buf, LN_2025_path, outname, ALLEMA_filter){
  
  
  # Set the filter (area to consider) step
  #-----------------------------------------------------
  
  # # Load in LN Data and filter only crowns on agricultural surface 
  # # TEMP - to be moved to sa geopackage
  # #-----------------------------------------------------
  # 
  # # Convert extent to correct format
  # sa_wkt <- st_as_text(st_geometry(st_as_sf(as.polygons(e_buf))))
  # 
  # # Read BLW geoms
  # ln_sa <- st_read(LN_2025_path, layer="Landwirtschaftliche_Nutzungsflaechen_Schweiz_2025", wkt_filter = sa_wkt) 
  # 
  # # Keep only LN Flächen with code < 900
  # ln_sa <- ln_sa[which(ln_sa$lnf_code < 900),]
  # 
  # # TEMP - Just union all the geoms, later keep only relevant ones
  # ln_filter <- st_union(ln_sa) %>%
  #   st_transform(crs(vhm_cell))
    
  # Segmentation step
  #-----------------------------------------------------
  
  # Check which VHM we are processing
  res_cm <- res(vhm_cell)[1] * 100
  
  if(res_cm == 50){ # VHM with 50 cm resolution
    
    #-----------------------------------------------------
    # TODO - Make a function! Super ugly like that...
    # Perform the VHM smoothings
    # Compute the different watershed segmentations
    # Filter the crowns to keep only what is on agricultural land
    # Export results
    # Remove unused objects
    #-----------------------------------------------------

    vhm_sa_smooth3_mean <- focal(vhm_cell, w = matrix(1,3,3), fun = mean)
    crowns_ws_mw3_mean <- lidR::watershed(vhm_sa_smooth3_mean)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_mw3_mean <- crowns_ws_mw3_mean[!st_intersects(crowns_ws_mw3_mean, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_mw3_mean, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_mw3_mean"), delete_dsn = TRUE)
    rm(vhm_sa_smooth3_mean); rm(crowns_ws_mw3_mean)
    
    vhm_sa_smooth5_mean <- focal(vhm_cell, w = matrix(1,5,5), fun = mean)
    crowns_ws_mw5_mean <- lidR::watershed(vhm_sa_smooth5_mean)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_mw5_mean <- crowns_ws_mw5_mean[!st_intersects(crowns_ws_mw5_mean, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_mw5_mean, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_mw5_mean"), append = TRUE)
    rm(vhm_sa_smooth5_mean); rm(crowns_ws_mw5_mean)
    
    vhm_sa_smooth3_max <- focal(vhm_cell, w = matrix(1,3,3), fun = max)
    crowns_ws_mw3_max <- lidR::watershed(vhm_sa_smooth3_max)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_mw3_max <- crowns_ws_mw3_max[!st_intersects(crowns_ws_mw3_max, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_mw3_max, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_mw3_max"), append = TRUE)
    rm(vhm_sa_smooth3_max); rm(crowns_ws_mw3_max)
    
    vhm_sa_smooth5_max <- focal(vhm_cell, w = matrix(1,5,5), fun = max)
    crowns_ws_mw5_max <- lidR::watershed(vhm_sa_smooth5_max)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_mw5_max <- crowns_ws_mw5_max[!st_intersects(crowns_ws_mw5_max, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_mw5_max, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_mw5_max"), append = TRUE)
    rm(vhm_sa_smooth5_max); rm(crowns_ws_mw5_max)
    
    g03 <- focalMat(vhm_cell, d = 0.3, type = "Gauss")
    vhm_sa_gauss03 <- focal(vhm_cell, w = g03, fun = sum)
    crowns_gauss03 <- lidR::watershed(vhm_sa_gauss03)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss03 <- crowns_gauss03[!st_intersects(crowns_gauss03, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss03, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss03"), append = TRUE)
    rm(vhm_sa_gauss03); rm(crowns_gauss03); rm(g03)
    
    g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
    vhm_sa_gauss05 <- focal(vhm_cell, w = g05, fun = sum)
    crowns_gauss05 <- lidR::watershed(vhm_sa_gauss05)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss05 <- crowns_gauss05[!st_intersects(crowns_gauss05, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss05, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss05"), append = TRUE)
    rm(vhm_sa_gauss05); rm(crowns_gauss05); rm(g05)
    
    g07 <- focalMat(vhm_cell, d = 0.7, type = "Gauss")
    vhm_sa_gauss07 <- focal(vhm_cell, w = g07, fun = sum)
    crowns_gauss07 <- lidR::watershed(vhm_sa_gauss07)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss07 <- crowns_gauss07[!st_intersects(crowns_gauss07, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss07, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss07"), append = TRUE)
    rm(vhm_sa_gauss07); rm(crowns_gauss07); rm(g07)
    
    g1 <- focalMat(vhm_cell, d = 1, type = "Gauss")
    vhm_sa_gauss1 <- focal(vhm_cell, w = g1, fun = sum)
    crowns_gauss1 <- lidR::watershed(vhm_sa_gauss1)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss1 <- crowns_gauss1[!st_intersects(crowns_gauss1, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss1, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss1"), append = TRUE)
    rm(vhm_sa_gauss1); rm(crowns_gauss1); rm(g1)
    
    g2 <- focalMat(vhm_cell, d = 2, type = "Gauss")
    vhm_sa_gauss2 <- focal(vhm_cell, w = g2, fun = sum)
    crowns_gauss2 <- lidR::watershed(vhm_sa_gauss2)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss2 <- crowns_gauss2[!st_intersects(crowns_gauss2, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss2, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss2"), append = TRUE)
    rm(vhm_sa_gauss2); rm(crowns_gauss2); rm(g2)
    
    g3 <- focalMat(vhm_cell, d = 3, type = "Gauss")
    vhm_sa_gauss3 <- focal(vhm_cell, w = g3, fun = sum)
    crowns_gauss3 <- lidR::watershed(vhm_sa_gauss3)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss3 <- crowns_gauss3[!st_intersects(crowns_gauss3, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss3, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss3"), append = TRUE)
    rm(vhm_sa_gauss3); rm(crowns_gauss3); rm(g3)
    
  } else { # VHM with 1 m resolution
    

    crowns_ws_raw <- lidR::watershed(vhm_cell)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_raw <- crowns_ws_raw[!st_intersects(crowns_ws_raw, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_raw, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_raw"), append = TRUE)
    rm(crowns_ws_raw)
    
    vhm_sa_smooth3_mean <- focal(vhm_cell, w = matrix(1,3,3), fun = mean)
    crowns_ws_mw3_mean <- lidR::watershed(vhm_sa_smooth3_mean)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_mw3_mean <- crowns_ws_mw3_mean[!st_intersects(crowns_ws_mw3_mean, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_mw3_mean, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_mw3_mean"), append = TRUE)
    rm(vhm_sa_smooth3_mean); rm(crowns_ws_mw3_mean)
    
    vhm_sa_smooth3_max <- focal(vhm_cell, w = matrix(1,3,3), fun = max)
    crowns_ws_mw3_max <- lidR::watershed(vhm_sa_smooth3_max)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_ws_mw3_max <- crowns_ws_mw3_max[!st_intersects(crowns_ws_mw3_max, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_ws_mw3_max, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ws_mw3_max"), append = TRUE)
    rm(vhm_sa_smooth3_max); rm(crowns_ws_mw3_max)
    
    g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
    vhm_sa_gauss05 <- focal(vhm_cell, w = g05, fun = sum)
    crowns_gauss05 <- lidR::watershed(vhm_sa_gauss05)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss05 <- crowns_gauss05[!st_intersects(crowns_gauss05, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss05, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss05"), append = TRUE)
    rm(vhm_sa_gauss05); rm(crowns_gauss05); rm(g05)
    
    g07 <- focalMat(vhm_cell, d = 0.7, type = "Gauss")
    vhm_sa_gauss07 <- focal(vhm_cell, w = g07, fun = sum)
    crowns_gauss07 <- lidR::watershed(vhm_sa_gauss07)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss07 <- crowns_gauss07[!st_intersects(crowns_gauss07, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss07, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss07"), append = TRUE)
    rm(vhm_sa_gauss07); rm(crowns_gauss07); rm(g07)
    
    g1 <- focalMat(vhm_cell, d = 1, type = "Gauss")
    vhm_sa_gauss1 <- focal(vhm_cell, w = g1, fun = sum)
    crowns_gauss1 <- lidR::watershed(vhm_sa_gauss1)() %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_cell))
    crowns_gauss1 <- crowns_gauss1[!st_intersects(crowns_gauss1, ALLEMA_filter, sparse = FALSE), ]
    write_sf(crowns_gauss1, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_gauss1"), append = TRUE)
    rm(vhm_sa_gauss1); rm(crowns_gauss1); rm(g1)
    
  }
  
  
  # # ENSEMBLE SEGMENTATION 
  # TODO - Probably put in other script once all other methods are better analyzed
  # #-----------------------------------------------------
  # 
  # # Small helper function
  # get_points <- function(sf_poly, method_name) {
  #   pts <- st_centroid(sf_poly)
  #   pts$method <- method_name
  #   return(pts)
  # }
  # 
  # # Get the cendroids of the selected methods
  # pts_list <- list(
  #   get_points(crowns_ws_mw3_mean, "mw3_mean"),
  #   get_points(crowns_ws_mw5_mean, "mw5_mean"),
  #   get_points(crowns_ws_mw3_max,  "mw3_max"),
  #   get_points(crowns_ws_mw5_max,  "mw5_max"),
  #   get_points(crowns_gauss03,     "g03"),
  #   get_points(crowns_gauss05,     "g05"),
  #   get_points(crowns_gauss07,     "g07"),
  #   get_points(crowns_gauss1,      "g1"),
  #   get_points(crowns_gauss2,      "g2"),
  #   get_points(crowns_gauss3,      "g3")
  # )
  # all_pts <- do.call(rbind, pts_list)
  # 
  # # Create a vote raster
  # template <- vhm_cell
  # vote_raster <- rast(template)
  # values(vote_raster) <- 0
  # 
  # # Sum up the number of trees that were detected for each cell across the different methods
  # vote_raster <- rasterize(vect(all_pts), vote_raster, field = 1, fun = "sum")
  # 
  # # Smooth the vote map to merge nearby detections
  # kernel <- focalMat(vhm_cell, d = 1, type = "Gauss") 
  # vote_smooth <- focal(vote_raster, w = kernel, fun = sum)
  # 
  # # Extract the final trees using lidr
  # final_trees <- locate_trees(vote_smooth, lmf(ws = 3))
  # 
  # # Get the number of times that a final tree was detected across methods
  # coords <- crds(vect(final_trees))
  # final_trees$votes <- extract(vote_raster, coords)[,1]
  # 
  # # # Apply filter to keep only trees that were detected at least 2 times
  # # final_trees <- final_trees[final_trees$votes >= 2, ]
  # 
  # # Export the ensemble trees
  # write_sf(final_trees, dsn=outname, layer=paste0("vhm",res_cm,"cm_crowns_ensemble"), append = TRUE)
  
  return(NULL)
}


#-----------------------------------------------------
# Cell processing function
#-----------------------------------------------------
process_cell <- function(i) {
  
  library(terra)
  library(sf)
  library(lidR)
  
  # Get extent of cell
  e <- ext(ALLEMA_Q[i, ])
  
  # Buffered extent
  e_buf <- ext(
    xmin(e) - 5, xmax(e) + 5,
    ymin(e) - 5, ymax(e) + 5
  )
  
  # Write to temp directory to avoid crash during parallelization
  local_out <- file.path(tempdir(),
                         paste0("ALLEMA_", ALLEMA_Q[i, ]$ID_Quadrat, ".gpkg"))
  
  #-----------------------------------------------------
  # VHM 0.5 m
  #-----------------------------------------------------
  
  # Crop the VHM to needed extent, and load it to memory so that can be used by lidR
  vhm_cell <- prep_vhm(VHM_S1_path, e_buf) 
  
  # Filter on which surfaces segmentation should happen
  #-----------------------------------------------------
  
  # Get the forest mask used in ALLEMA to be able to compare the same single trees (and actually also Feldgehoelze etc)
  ALLEMA_filter <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/Wald_Gehoelz_ErhZyk2.gdb", 
                           query = paste0("SELECT * FROM Wald_Gehoelz_ErhZyk2_3D WHERE FK_Quadrat = ", ALLEMA_Q[i, ]$ID_Quadrat, " AND Gehoelztyp IN (50,58)"))
  
  ALLEMA_filter <- st_union(ALLEMA_filter) %>%
    st_transform(crs(vhm_cell)) %>%
    st_buffer(5)
  
  # Perform the segmentation
  seg_cell(vhm_cell, e_buf, LN_2025_path, local_out, ALLEMA_filter)
  
  #-----------------------------------------------------
  # VHM 1 m
  #-----------------------------------------------------
  
  # Crop the VHM to needed extent, and load it to memory so that can be used by lidR
  vhm_cell <- prep_vhm(VHM_S1_1m_path, e_buf)
  
  # Perform the segmentation
  seg_cell(vhm_cell, e_buf, LN_2025_path, local_out, ALLEMA_filter)

  
  # Move the files to the final location
  final_out <- file.path(out_data_path, paste0("ALLEMA_", ALLEMA_Q[i, ]$ID_Quadrat, ".gpkg"))
  file.copy(local_out, final_out, overwrite = TRUE)
  
  
  return(NULL)
}

#-----------------------------------------------------
# Process over cells
#-----------------------------------------------------

# Set processing parameters - Limit number of threads to avoid explosion with parallel processing
Sys.setenv(GDAL_NUM_THREADS = "2")
Sys.setenv(OMP_NUM_THREADS = "2")

# Set up parallel processing
n_workers <- 4 # (detectCores() --> 20)
plan(multisession, workers = n_workers)

# Process cell by cell in parallel 
future_lapply(
  seq_len(nrow(ALLEMA_Q)),
  process_cell,
  future.seed = TRUE,
  future.packages = c("terra", "lidR", "sf"),
  future.globals = list(
    ALLEMA_Q = ALLEMA_Q,
    VHM_S1_path = VHM_S1_path,
    VHM_S1_1m_path = VHM_S1_1m_path,
    out_data_path = out_data_path,
    LN_2025_path = LN_2025_path,
    prep_vhm = prep_vhm,
    seg_cell = seg_cell
  )
)
