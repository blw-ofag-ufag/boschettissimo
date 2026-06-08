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

ws_segmentation <- function(vhm_in, smooth_f, mws, res_vhm, pos_filter, neg_filter, outname, new_gpkg){
  
  # Run the watershed algorithm
  ws_rast <- lidR::watershed(vhm_in)()
  
  # In case worker switched to raster output and not terra
  if (inherits(ws_rast, "Raster")) {
    ws_rast <- terra::rast(ws_rast)
  }
  
  # Get the crowns of the segmented trees
  crowns_ws <- ws_rast %>% as.polygons(dissolve = TRUE) %>% st_as_sf() %>% st_transform(crs(vhm_in))
  
  # If positive filter keep only crowns on pos_filter area
  if(!is.null(pos_filter) && length(pos_filter) > 0){
    crowns_ws <- crowns_ws[st_intersects(crowns_ws, pos_filter, sparse = FALSE), ]
  } 
  
  # If negative filter keep only crowns outside of neg_filter
  if(!is.null(neg_filter) && length(neg_filter) > 0){
    crowns_ws <- crowns_ws[!st_intersects(crowns_ws, neg_filter, sparse = FALSE), ]
  }
  
  # Export obtained layer
  write_sf(crowns_ws, dsn=outname, layer=paste0("vhm",res_vhm,"cm_crowns_",mws,"_", smooth_f), delete_dsn = !new_gpkg, append = new_gpkg)
  
  return(NULL)
  
}

seg_cell <- function(vhm_cell, e_buf, LN_2025_path, outname, ALLEMA_filter){
  
  # Segmentation step
  #-----------------------------------------------------
  
  # Check which VHM we are processing
  res_cm <- res(vhm_cell)[1] * 100
  
  if(res_cm == 50){ # VHM with 50 cm resolution
    
    vhm_sa_smooth3_mean <- focal(vhm_cell, w = matrix(1,3,3), fun = mean)
    ws_segmentation(vhm_sa_smooth3_mean, "mean", "3x3", res_cm, NULL, ALLEMA_filter, outname, FALSE)
    rm(vhm_sa_smooth3_mean)
    
    vhm_sa_smooth5_mean <- focal(vhm_cell, w = matrix(1,5,5), fun = mean)
    ws_segmentation(vhm_sa_smooth5_mean, "mean", "5x5", res_cm, NULL, ALLEMA_filter, outname, TRUE)
    rm(vhm_sa_smooth5_mean)
    
    vhm_sa_smooth3_max <- focal(vhm_cell, w = matrix(1,3,3), fun = max)
    ws_segmentation(vhm_sa_smooth3_max, "max", "3x3", res_cm, NULL, ALLEMA_filter, outname, TRUE)
    rm(vhm_sa_smooth3_max)
    
    vhm_sa_smooth5_max <- focal(vhm_cell, w = matrix(1,5,5), fun = max)
    ws_segmentation(vhm_sa_smooth5_max, "max", "5x5", res_cm, NULL, ALLEMA_filter, outname, TRUE)
    rm(vhm_sa_smooth5_max)
    
    g03 <- focalMat(vhm_cell, d = 0.3, type = "Gauss")
    vhm_sa_gauss03 <- focal(vhm_cell, w = g03, fun = sum)
    ws_segmentation(vhm_sa_gauss03, "gauss", "03", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss03); rm(g03)
    
    g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
    vhm_sa_gauss05 <- focal(vhm_cell, w = g05, fun = sum)
    ws_segmentation(vhm_sa_gauss05, "gauss", "05", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss05); rm(g05)
    
    g07 <- focalMat(vhm_cell, d = 0.7, type = "Gauss")
    vhm_sa_gauss07 <- focal(vhm_cell, w = g07, fun = sum)
    ws_segmentation(vhm_sa_gauss07, "gauss", "07", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss07); rm(g07)
    
    g1 <- focalMat(vhm_cell, d = 1, type = "Gauss")
    vhm_sa_gauss1 <- focal(vhm_cell, w = g1, fun = sum)
    ws_segmentation(vhm_sa_gauss1, "gauss", "1", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss1); rm(g1)
    
    g2 <- focalMat(vhm_cell, d = 2, type = "Gauss")
    vhm_sa_gauss2 <- focal(vhm_cell, w = g2, fun = sum)
    ws_segmentation(vhm_sa_gauss2, "gauss", "2", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss2); rm(g2)
    
    g3 <- focalMat(vhm_cell, d = 3, type = "Gauss")
    vhm_sa_gauss3 <- focal(vhm_cell, w = g3, fun = sum)
    ws_segmentation(vhm_sa_gauss3, "gauss", "3", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss3); rm(g3)
    
  } else { # VHM with 1 m resolution
    
    ws_segmentation(vhm_cell, "raw", "", res_cm, NULL, ALLEMA_filter, outname, TRUE)
    
    vhm_sa_smooth3_mean <- focal(vhm_cell, w = matrix(1,3,3), fun = mean)
    ws_segmentation(vhm_sa_smooth3_mean, "mean", "3x3", res_cm, NULL, ALLEMA_filter, outname, TRUE)
    rm(vhm_sa_smooth3_mean)
    
    vhm_sa_smooth3_max <- focal(vhm_cell, w = matrix(1,3,3), fun = max)
    ws_segmentation(vhm_sa_smooth3_max, "max", "3x3", res_cm, NULL, ALLEMA_filter, outname, TRUE)
    rm(vhm_sa_smooth3_max)
    
    g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
    vhm_sa_gauss05 <- focal(vhm_cell, w = g05, fun = sum)
    ws_segmentation(vhm_sa_gauss05, "gauss", "05", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss05); rm(g05)
    
    g07 <- focalMat(vhm_cell, d = 0.7, type = "Gauss")
    vhm_sa_gauss07 <- focal(vhm_cell, w = g07, fun = sum)
    ws_segmentation(vhm_sa_gauss07, "gauss", "07", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss07); rm(g07)
    
    g1 <- focalMat(vhm_cell, d = 1, type = "Gauss")
    vhm_sa_gauss1 <- focal(vhm_cell, w = g1, fun = sum)
    ws_segmentation(vhm_sa_gauss1, "gauss", "1", res_cm, NULL, ALLEMA_filter, outname, TRUE) 
    rm(vhm_sa_gauss1); rm(g1)
    
  }
  
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
    st_transform(crs(vhm_cell)) 
  
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
    seg_cell = seg_cell,
    ws_segmentation = ws_segmentation
  )
)
