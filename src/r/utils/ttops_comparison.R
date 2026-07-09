#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(lidR)
library(sf)
library(dplyr)
library(terra)
library(mapview)
library(ForestTools)

#-----------------------------------------------------
# Functions
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

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# Allema quadrants
ALLEMA_Q <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_Q_ref.gpkg")
perim_ALLEMA <- ALLEMA_Q[24,] 

# Import the LAS for the test area
las <- readLAS("//speedy11-12-fs/data_17/_LIDAR/ALS_CH/nDSM/nDSM_2689000_1214000_SWISS_20180526_LV95LN02.laz", filter = "-drop_z_below 0" )

# Filter to keep only the vegetation points
las_EB_BG <- filter_poi(las, Classification == 3)

# Set the projection system to the las file
st_crs(las_EB_BG) <- st_crs(perim_ALLEMA)

# Get extent of cell
e <- ext(perim_ALLEMA)

# Buffered extent
e_buf <- ext(
  xmin(e), xmax(e),
  ymin(e), ymax(e)
)

# Crop the VHM to needed extent
vhm_perim <- prep_vhm(VHM_S1_path, e_buf) 

# Import the test areas
perim <- st_read("D:/temp/EB_vs_BGruppe.gpkg") 


# Function to get tree tops
# ------------------------------------------
get_ttops <- function(las, perim, f, vhm){
  
  # Filter to keep only points within test area
  las <- clip_roi(las, perim)
  
  # Identify the tree tops
  ttops <- locate_trees(las, lmf(f, shape="circular")) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  # Limit raster extent
  vhm <- crop(vhm, perim)
  
  # Get the watershed crowns
  g08 <- focalMat(vhm, d = 0.55, type = "Gauss")
  vhm <- focal(vhm, w = g08, fun = sum)
  crowns <- mcws(
    treetops = ttops,
    CHM = vhm,
    minHeight = 0.5,
    format = "polygons"
  )
  
  mapview(ttops)+mapview(vhm)+mapview(crowns) # + mapview(perim, alpha = 0.4)
}

get_ttops_vhm <- function(perim, f, vhm){
  # Limit raster extent
  vhm <- crop(vhm, perim)
  
  # Get the watershed crowns
  g08 <- focalMat(vhm, d = 0.5, type = "Gauss")
  vhm <- focal(vhm, w = g08, fun = sum)
  
  # Identify the tree tops
  ttops <- locate_trees(vhm, lmf(f, shape="circular")) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  crowns <- mcws(
    treetops = ttops,
    CHM = vhm,
    minHeight = 0.5,
    format = "polygons"
  )
  
  mapview(ttops)+mapview(vhm)+mapview(crowns) # + mapview(perim, alpha = 0.4)
}



# AREA 1
# ------------------------------------------
f1 <- function(h) {
  ifelse(h < 4, 4,
         ifelse(h < 7.5, 7,
                ifelse(h < 10, 8.5,
                       11)))
}

get_ttops(las_EB_BG, perim[1,], f2, vhm_perim)
get_ttops_vhm(perim[1,], f2, vhm_perim)


# Test linear function
# ------------------------------------------
f1 <- function(h) {
  ifelse(h < 4, 3, 3.5 + 0.7*h)
}
f1 <- function(h) {
  3.5 + 0.7*h
}

get_ttops(las_EB_BG, perim[1,], f1, vhm_perim)
get_ttops_vhm(perim[1,], f1, vhm_perim)

# AREA 2
# ------------------------------------------
f2 <- function(h) {
  ifelse(h < 7.5, 7,
         ifelse(h < 10, 8.5,
                11))
}

get_ttops(las_EB_BG, perim[2,], f2, vhm_perim)
get_ttops_vhm(perim[2,], f2, vhm_perim)
# Quasi parfait


# AREA 3
# ------------------------------------------
get_ttops(las_EB_BG, perim[3,], f2, vhm_perim)

# AREA 4
# ------------------------------------------
get_ttops(las_EB_BG, perim[4,], f2, vhm_perim)


get_crowns <- function(las, perim, f, vhm){
  
  # Filter to keep only points within test area
  las <- clip_roi(las, perim)
  
  # Identify the tree tops
  ttops <- locate_trees(las, lmf(f, shape="circular")) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  # Limit raster extent
  vhm <- crop(vhm, perim)
  
  # Get the watershed crowns
  g08 <- focalMat(vhm, d = 0.55, type = "Gauss")
  vhm <- focal(vhm, w = g08, fun = sum)
  crowns <- mcws(
    treetops = ttops,
    CHM = vhm,
    minHeight = 0.5,
    format = "polygons"
  )
  
  return(crowns)
}

crowns_perim <- get_crowns(las_EB_BG, perim_ALLEMA, f1, vhm_perim)
st_write(crowns_perim, "D:/temp/crowns_perim_linf2.gpkg", append = F)
