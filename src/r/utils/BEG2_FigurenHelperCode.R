
# Libraries
library(lidR)
library(terra)
library(dplyr)
library(sf)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# Load the grid for processing 
CH_1000 <- rast(CH_1000_path) %>%
  as.polygons(values=TRUE, dissolve=FALSE) %>%
  st_as_sf()

# Define Schafboden ALLEMA Quadrat center
cxy <- st_point(c(2689375.53, 1214453.54)) %>%
  st_sfc(crs = 2056) %>%
  st_sf()

# Keep only intersecting polygons
CH_1000 <- CH_1000[lengths(st_intersects(CH_1000, cxy)) > 0, ]

# Get the VHM for the study area
vhm_cell_prep <- function(arg_e_buf, arg_VHM_S2_path) {
  
  # Create temp file
  tmpfile <- tempfile("vhm_crop_")
  
  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(arg_e_buf), ymax(arg_e_buf), xmax(arg_e_buf), ymin(arg_e_buf),
    arg_VHM_S2_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt"))
  
  # Load the raster into memory (needed for lidr processing steps)
  vhm_cell <- toMemory(vhm_cell)
  
  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))
  
  return(vhm_cell)
  
}
vhm_sa <- vhm_cell_prep(ext(CH_1000),VHM_S2_path)


# Path to las files
las_schafboden <- c("nDSM_2688000_1215000_SWISS2_20240709_LV95LN02.copc.laz",
                    "nDSM_2689000_1215000_SWISS2_20240322_LV95LN02.copc.laz",
                    "nDSM_2690000_1215000_SWISS2_20240322_LV95LN02.copc.laz",
                    "nDSM_2688000_1214000_SWISS2_20240325_LV95LN02.copc.laz",
                    "nDSM_2689000_1214000_SWISS2_20240325_LV95LN02.copc.laz",
                    "nDSM_2690000_1214000_SWISS2_20240325_LV95LN02.copc.laz",
                    "nDSM_2688000_1213000_SWISS2_20240325_LV95LN02.copc.laz",
                    "nDSM_2689000_1213000_SWISS2_20240325_LV95LN02.copc.laz",
                    "nDSM_2690000_1213000_SWISS2_20240325_LV95LN02.copc.laz")

las_rischberg <- c(
  
)
las_dir <- paste0("//speedy11-12-fs/data_17/_LIDAR/ALS_CH/nDSM/",las_schafboden)

# Load the las catalogue
ctg <- readLAScatalog(las_dir)

# Clip to the study area
las <- clip_rectangle(ctg,ext(CH_1000)[1],ext(CH_1000)[3],ext(CH_1000)[2],ext(CH_1000)[4])

# Filter to keep only the vegetation points
las_veg <- filter_poi(las, Classification == 3)

#-----------------------------------------------------
# Silva 2016 - Paper parameters
# not bad, some oversegmentation of big trees, some small trees not segmented
#-----------------------------------------------------

# 1. Smooth CHM with fixed SWS = 3x3
chm_s <- focal(vhm_sa, w = matrix(1,3,3), fun = mean)

# 2. Check COV
global(vhm_sa > 1.37, fun = "mean", na.rm = TRUE) * 100

# 3. Detect trees with TWS = 5 if COV > 70%, TWS = 7 otherwise
ttops <- locate_trees(
  chm_s,
  lmf(ws = 7, hmin = 1.37, shape = "circular")
)

# 4. Segment trees (crown delineation)
seg_algo <- lidR::silva2016(chm_s, ttops, max_cr_factor = 0.6)
crowns <- seg_algo()
crowns_polys <- as.polygons(crowns, dissolve = TRUE)%>%
  st_as_sf()

st_write(crowns_polys, "D:/temp/crowns_silva_test_ws7.gpkg")

#-----------------------------------------------------
# Silva 2016 - Larger search radius
#-----------------------------------------------------

# 3. Detect trees with TWS = 5 if COV > 70%, TWS = 7 otherwise
ttops <- locate_trees(
  chm_s,
  lmf(ws = 15, hmin = 1.37, shape = "circular")
)

# 4. Segment trees (crown delineation)
seg_algo <- lidR::silva2016(chm_s, ttops, max_cr_factor = 0.6)
crowns <- seg_algo()
crowns_polys <- as.polygons(crowns, dissolve = TRUE)%>%
  st_as_sf()

st_write(crowns_polys, "D:/temp/crowns_silva_test_ws15.gpkg")

#-----------------------------------------------------
# Silva 2016 - Our parameters
#-----------------------------------------------------

# Define the function that should be used to find tree tops 
find_ttops <- function(h) {
  3.5 + 0.7*h
}

# Identify the tree tops
ttops <- lidR::locate_trees(vhm_sa, lmf(find_ttops, shape="circular", hmin = 1.5)) %>%
  st_zm(drop = TRUE, what = "ZM")

# 4. Segment trees (crown delineation)
seg_algo <- lidR::silva2016(chm_s, ttops, max_cr_factor = 0.6)
crowns <- seg_algo()
crowns_polys <- as.polygons(crowns, dissolve = TRUE)%>%
  st_as_sf()

st_write(crowns_polys, "D:/temp/crowns_silva_test_linreg.gpkg")

#-----------------------------------------------------
# Watershed
#-----------------------------------------------------


vhm_sa_smooth3_mean <- focal(vhm_sa, w = matrix(1,3,3), fun = mean)
vhm_sa_smooth5_mean <- focal(vhm_sa, w = matrix(1,5,5), fun = mean)
vhm_sa_smooth3_max <- focal(vhm_sa, w = matrix(1,3,3), fun = max)
vhm_sa_smooth5_max <- focal(vhm_sa, w = matrix(1,5,5), fun = max)


crowns_ws_mw3_mean <- lidR::watershed(vhm_sa_smooth3_mean)() %>%
  as.polygons()%>%
  st_as_sf() %>%
  st_write("D:/temp/crowns_watershed_mean3.gpkg")

crowns_ws_mw5_mean <- lidR::watershed(vhm_sa_smooth5_mean)() %>%
  as.polygons()%>%
  st_as_sf() %>%
  st_write("D:/temp/crowns_watershed_mean5.gpkg")

crowns_ws_mw3_max <- lidR::watershed(vhm_sa_smooth3_max)() %>%
  as.polygons()%>%
  st_as_sf() %>%
  st_write("D:/temp/crowns_watershed_max3.gpkg")

crowns_ws_mw5_max <- lidR::watershed(vhm_sa_smooth5_max)()%>%
  as.polygons()%>%
  st_as_sf() %>%
  st_write("D:/temp/crowns_watershed_max5.gpkg")
