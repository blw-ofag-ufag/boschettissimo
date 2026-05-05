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
CH_1000 <- rast(CH_1000_path) %>%
  as.polygons(values=TRUE, dissolve=FALSE) %>%
  st_as_sf()

#-----------------------------------------------------
# TEMP! - Filter on canton TG
#-----------------------------------------------------

# Load the canton
TG <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb", 
              query="select * from TLM_KANTONSGEBIET t where t.NAME = 'Thurgau'") %>%
  st_zm(drop = TRUE, what = "ZM") 

# Set the crs (same, but had the Z mention for TG)
st_crs(TG) <- st_crs(CH_1000)
  
# Keep only intersecting polygons
CH_1000 <- CH_1000[st_intersects(CH_1000, TG, sparse = FALSE), ]

#-----------------------------------------------------
# Segmentation functions (TODO: put it in separate code file)
#-----------------------------------------------------

segment_small_trees <- function(vhm){
  
  # Specify the segmentation algorithm
  seg_algo <- lidR::watershed(vhm, th_tree = 1.5, ext = 1, tol = 1)
  
  # Segment
  crowns <- seg_algo()
  
  # Transform rastered crowns to polygons
  crowns_polys <- as.polygons(crowns, dissolve = TRUE)
  
  return(crowns_polys)
  
}

segment_large_trees <- function(vhm){
  
  # Specify the segmentation algorithm
  seg_algo <- lidR::watershed(vhm, th_tree = 2, ext = 2, tol = 1.5)
  
  # Segment
  crowns <- seg_algo()
  
  # Transform rastered crowns to polygons
  crowns_polys <- as.polygons(crowns, dissolve = TRUE)
  
  return(crowns_polys)
  
}

#-----------------------------------------------------
# Cell processing function
#-----------------------------------------------------
process_cell <- function(i) {
  
  library(terra)
  library(lidR)
  
  # Get extent of cell
  e <- ext(CH_1000[i, ])
  
  # Buffered extent
  e_buf <- ext(
    xmin(e) - 25, xmax(e) + 25,
    ymin(e) - 25, ymax(e) + 25
  )
  
  # Create temp file
  tmpfile <- tempfile("vhm_crop_")
  
  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(e_buf), ymax(e_buf), xmax(e_buf), ymin(e_buf),
    VHM_S2_local_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt"))

  # Load the raster into memory (needed for lidr processing steps)
  vhm_cell <- toMemory(vhm_cell)
  
  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))
  
  # Smooth
  vhm_cell_smooth <- focal(vhm_cell, w = matrix(1,3,3), fun = mean)
  
  # Segment
  tree_seg_s <- segment_small_trees(vhm_cell)
  tree_seg_l <- segment_large_trees(vhm_cell_smooth)
  
  # Output filename 
  fname <- paste0(
    treeseg_data_local_path,
    "CH1000_", xmin(e), "_", xmax(e), "_",
    ymin(e), "_", ymax(e), ".gpkg"
  )
  
  # Write results
  writeVector(tree_seg_s,
              filename = fname,
              layer = "crowns_small",
              overwrite = TRUE)
  
  writeVector(tree_seg_l,
              filename = fname,
              layer = "crowns_large",
              insert = TRUE)
  
  return(NULL)
}

#-----------------------------------------------------
# Process over cells
#-----------------------------------------------------

# Set processing parameters - Limit number of threads to avoid explosion with parallel processing
Sys.setenv(GDAL_NUM_THREADS = "2")
Sys.setenv(OMP_NUM_THREADS = "2")

# Set up parallel processing
n_workers <- 16 # (detectCores() --> 20)
plan(multisession, workers = n_workers)

# Process cell by cell in parallel 
future_lapply(
  seq_len(nrow(CH_1000)),
  process_cell,
  future.packages = c("terra", "lidR"),
  future.globals = list(
    CH_1000 = CH_1000,
    VHM_S2_local_path = VHM_S2_local_path,
    treeseg_data_local_path = treeseg_data_local_path,
    segment_small_trees = segment_small_trees,
    segment_large_trees = segment_large_trees
  )
)
