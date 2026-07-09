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
CH_1000 <- st_read(ALS_ndms) %>%
  filter(source == "SWISS")

# Limit analysis on LN areas
LN_mask <- st_read(LN_mask_path)

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
# Cell processing function
#-----------------------------------------------------
process_cell <- function(i) {
  
  library(terra)
  library(lidR)
  library(ForestTools)
  
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
    VHM_S1_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt"))

  # Load the raster into memory (needed for lidr processing steps)
  vhm_cell <- toMemory(vhm_cell)
  
  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))
  
  # Smooth vhm
  g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
  vhm_cell <- focal(vhm_cell, w = g05, fun = sum)
  
  # Limit analysis on LN areas
  LN_sub <- st_filter(LN_mask, CH_1000[i, ], .predicate = st_intersects) %>%
    st_union()
  
  # Have a buffered version to limit processing
  LN_sub_buff <- LN_sub %>%
    st_buffer(50)
  
  # Crop and mask to buffered area only
  LN_sub_buff_vect <- vect(LN_sub_buff)
  vhm_cell <-  vhm_cell %>%
    crop(LN_sub_buff_vect) %>%
    mask(LN_sub_buff_vect)
  
  # Define the function that should be used to find tree tops 
  find_ttops <- function(h) {
    3.5 + 0.7*h
  }
  
  # Identify the tree tops
  ttops <- locate_trees(vhm_cell, lmf(find_ttops, shape="circular")) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  # Get the watershed crowns
  crowns2 <- mcws(
    treetops = ttops,
    CHM = vhm_cell,
    minHeight = 1,
    format = "polygons"
  )
  
  # Get the centroid of the crown 
  centroids <- st_centroid(crowns2)
  
  # Keep only crowns that have their centroid within the LN parcels
  crowns2 <- crowns2[lengths(st_intersects(centroids, LN_sub)) > 0, ]
  
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
