
# Libraries
library(lidR)
library(terra)
library(dplyr)
library(sf)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# Get study areas ALS file
ALS_sas <- read.csv2(file = "data/ALS_study_areas.csv", sep=",")

# Get the study areas
sas <- read.csv2(file = "data/study_areas.csv", sep=",")

# Test for one study area
sa <- "allema_rischberg"

# Filter the LAS files associated to the study area
ALS_sa <- ALS_sas[which(ALS_sas$sa_id == sa),]

# Load the VHM associated to the study area
vhm_sa <- rast(paste0(study_area_data_path,sa,"_vhm_S2.tif"))

# Load vhm to memory
vhm_sa <- toMemory(vhm_sa)

# Path to las files
las_dir <- paste0(study_area_data_path,sa,"_las/")

# Load the las catalogue
ctg <- readLAScatalog(las_dir)

# Clip to the study area
las <- clip_rectangle(ctg, 
                      sas$upper_left_e[which(sas$id == sa)]-100,
                      sas$lower_right_n[which(sas$id == sa)]-100,
                      sas$lower_right_e[which(sas$id == sa)]+100,
                      sas$upper_left_n[which(sas$id == sa)]+100)

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
seg <- segment_trees(
  las_veg,
  silva2016(chm_s, ttops, max_cr_factor = 0.6)
)

plot(seg, bg = "white", size = 4, color = "treeID")

#-----------------------------------------------------
# Silva 2016 - Adapted parameters
# pretty good, some additional small trees could be detected
#-----------------------------------------------------

# 1. Smooth CHM with fixed SWS = 3x3
chm_s <- focal(vhm_sa, w = matrix(1,3,3), fun = mean)

# 2. Check COV
global(vhm_sa > 1.37, fun = "mean", na.rm = TRUE) * 100

# 2. Detect trees with TWS = 5 if COV > 70%, TWS = 7 otherwise
ttops <- locate_trees(
  chm_s,
  lmf(ws = 9, hmin = 1.37, shape = "circular")
)

# 3. Segment trees (crown delineation)
seg <- segment_trees(
  las_veg,
  silva2016(chm_s, ttops, max_cr_factor = 0.9)
)

plot(seg, bg = "white", size = 4, color = "treeID")

#-----------------------------------------------------
# Watershed
# oversegmentation visible for quite some cases where single tree seems quite clear
#-----------------------------------------------------
seg <- segment_trees(
  las_veg,
  lidR::watershed(vhm_sa, th_tree = 1.37)
)

plot(seg, bg = "white", size = 4, color = "treeID")

#-----------------------------------------------------
# Watershed - smoothed VHM
# oversegmentation less visible, but some of the smaller trees are lost
#-----------------------------------------------------
seg <- segment_trees(
  las_veg,
  lidR::watershed(chm_s, th_tree = 1.37)
)

plot(seg, bg = "white", size = 4, color = "treeID")

#-----------------------------------------------------
# Watershed - setting ext & tol
# ext higher --> less oversegmentation
# tol higher --> less oversegmentation
# tolerance seems a bit harder to set, keep it at 1 (default)
# rickenbach: ext = 2 best compromise between under and over segmentation
# allema_rischberg: hard to say what is best between 2 and 3...
#-----------------------------------------------------
seg <- segment_trees(
  las_veg,
  lidR::watershed(vhm_sa, th_tree = 2, ext = 2) 
)

plot(seg, bg = "white", size = 4, color = "treeID")

#-----------------------------------------------------
# Treetops guided watershed (ForestTools), Habitat Map methodology
# allema_rischberg: very high number of oversegmented trees
#-----------------------------------------------------
library(ForestTools)
lin <- function(x){x * 0.05 + 0.6}
ttops <- vwf(chm_s, winFun = lin, minHeight = 2)
crowns_poly <- mcws(treetops = ttops, CHM = chm_s, format = "polygons", minHeight = 1.5)

mapview(chm_s)+mapview(crowns_poly)

#-----------------------------------------------------
# 2-approach test
# 1 segment small new trees on raw VHM
# 2 segment old large trees on smoothed VHM
#-----------------------------------------------------

library(dplyr)
library(leaflet)
library(leafgl)
library(sf)
library(units)

#-----------------------------------------------------
# Segmentation for small trees
#-----------------------------------------------------

seg_algo <- lidR::watershed(vhm_sa, th_tree = 1.37, ext = 1, tol = 1)
crowns_small <- seg_algo()
crowns_small_polys <- as.polygons(crowns_small, dissolve = TRUE)

layer_crowns_small <- crowns_small_polys %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

#-----------------------------------------------------
# Segmentation for large trees
#-----------------------------------------------------

# Smooth CHM with fixed SWS = 3x3
chm_s <- focal(vhm_sa, w = matrix(1,3,3), fun = mean)

#-----------------------------------------------------
# Smoothed VHM, diff tol and ext tests
#-----------------------------------------------------

# Segment the larger trees, testing different parameters
seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1)
crowns_big_1 <- seg_algo()
crowns_big_1_polys <- as.polygons(crowns_big_1, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1)
crowns_big_2 <- seg_algo()
crowns_big_2_polys <- as.polygons(crowns_big_2, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 2)
crowns_big_3 <- seg_algo()
crowns_big_3_polys <- as.polygons(crowns_big_3, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 2)
crowns_big_4 <- seg_algo()
crowns_big_4_polys <- as.polygons(crowns_big_4, dissolve = TRUE)

# Smoothed VHM
# Comment : tol = 2 undersegmentation is too strong. 
# Usually ext = 2 worse than ext = 1

#-----------------------------------------------------
# Smoothed VHM, diff tol and ext tests
#-----------------------------------------------------

# Segment the larger trees, testing different parameters
seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1.25)
crowns_big_1 <- seg_algo()
crowns_big_1_polys <- as.polygons(crowns_big_1, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1.25)
crowns_big_2 <- seg_algo()
crowns_big_2_polys <- as.polygons(crowns_big_2, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1.1)
crowns_big_3 <- seg_algo()
crowns_big_3_polys <- as.polygons(crowns_big_3, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1.1)
crowns_big_4 <- seg_algo()
crowns_big_4_polys <- as.polygons(crowns_big_4, dissolve = TRUE)

# Smoothed VHM
# Comment : tried tol = 1.1, 1.25, 1.5 --> 1.25 seemed like an ok value. But still quite a lot of undersegmentation
# Usually ext = 2 worse than ext = 1

#-----------------------------------------------------
# Smoothed VHM, diff tol and ext tests
#-----------------------------------------------------

# Segment the larger trees, testing different parameters
seg_algo <- lidR::watershed(vhm_sa, th_tree = 2, ext = 1, tol = 1.25)
crowns_big_1 <- seg_algo()
crowns_big_1_polys <- as.polygons(crowns_big_1, dissolve = TRUE)

seg_algo <- lidR::watershed(vhm_sa, th_tree = 2, ext = 2, tol = 1.25)
crowns_big_2 <- seg_algo()
crowns_big_2_polys <- as.polygons(crowns_big_2, dissolve = TRUE)

seg_algo <- lidR::watershed(vhm_sa, th_tree = 2, ext = 1, tol = 1.5)
crowns_big_3 <- seg_algo()
crowns_big_3_polys <- as.polygons(crowns_big_3, dissolve = TRUE)

seg_algo <- lidR::watershed(vhm_sa, th_tree = 2, ext = 2, tol = 1.5)
crowns_big_4 <- seg_algo()
crowns_big_4_polys <- as.polygons(crowns_big_4, dissolve = TRUE)

# Unsmoothed VHM
# Comment : even with ext=2 and tol=1.5 gives a bit ugly results with a mix of over segmentation in some regions and undersegmentation in others

#-----------------------------------------------------
# Smoothed VHM, diff tol and ext tests
#-----------------------------------------------------

# Segment the larger trees, testing different parameters
seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1.3)
crowns_big_1 <- seg_algo()
crowns_big_1_polys <- as.polygons(crowns_big_1, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1.3)
crowns_big_2 <- seg_algo()
crowns_big_2_polys <- as.polygons(crowns_big_2, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1.4)
crowns_big_3 <- seg_algo()
crowns_big_3_polys <- as.polygons(crowns_big_3, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1.5)
crowns_big_4 <- seg_algo()
crowns_big_4_polys <- as.polygons(crowns_big_4, dissolve = TRUE)

# Smoothed VHM
# Comment : Real hard to say... Quite a lot of undersegmentation, with ext = 2, tol = 1.3

#-----------------------------------------------------
# Smoothed VHM, diff tol and ext tests
#-----------------------------------------------------

# Segment the larger trees, testing different parameters
seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1.7)
crowns_big_1 <- seg_algo()
crowns_big_1_polys <- as.polygons(crowns_big_1, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1.6)
crowns_big_2 <- seg_algo()
crowns_big_2_polys <- as.polygons(crowns_big_2, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 2, tol = 1.5)
crowns_big_3 <- seg_algo()
crowns_big_3_polys <- as.polygons(crowns_big_3, dissolve = TRUE)

seg_algo <- lidR::watershed(chm_s, th_tree = 2, ext = 1, tol = 1.9)
crowns_big_4 <- seg_algo()
crowns_big_4_polys <- as.polygons(crowns_big_4, dissolve = TRUE)

# Smoothed VHM
# Comment : Real hard to say... Quite a lot of undersegmentation, with ext = 2, tol = 1.3

#-----------------------------------------------------
# Silva 2016
#-----------------------------------------------------

ttops1 <- locate_trees(
  chm_s,
  lmf(ws = 9, hmin = 1.37, shape = "circular")
)
seg_algo <- lidR::silva2016(chm_s, ttops1)
crowns_big_1 <- seg_algo()
crowns_big_1_polys <- as.polygons(crowns_big_1, dissolve = TRUE)

ttops2 <- locate_trees(
  chm_s,
  lmf(ws = 11, hmin = 1.37, shape = "circular")
)
seg_algo <- lidR::silva2016(chm_s, ttops2)
crowns_big_2 <- seg_algo()
crowns_big_2_polys <- as.polygons(crowns_big_2, dissolve = TRUE)

ttops3 <- locate_trees(
  chm_s,
  lmf(ws = 13, hmin = 1.37, shape = "circular")
)
seg_algo <- lidR::silva2016(chm_s, ttops3)
crowns_big_3 <- seg_algo()
crowns_big_3_polys <- as.polygons(crowns_big_3, dissolve = TRUE)

f4 <- function(x) {
  pmax(3, 0.5 * x^0.8)
}
ttops4 <- locate_trees(
  chm_s,
  lmf(ws = f4, hmin = 1.37, shape = "circular")
)
seg_algo <- lidR::silva2016(chm_s, ttops4)
crowns_big_4 <- seg_algo()
crowns_big_4_polys <- as.polygons(crowns_big_4, dissolve = TRUE)

# Silva
# Comment : Not ideal. Using fixed window size either over or undesegmentation but in a weird way 
# (around a tree creates another tree, and not splitting so weird) and using function based on height seems too complicated, 
# since width not easily relatable to height

#-----------------------------------------------------
# Visual look at the different outputs
#-----------------------------------------------------

# Transform to match leaflet objects
layer_crowns_big_1 <- crowns_big_1_polys %>% 
  st_as_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

layer_crowns_big_2 <- crowns_big_2_polys %>% 
  st_as_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

layer_crowns_big_3 <- crowns_big_3_polys %>% 
  st_as_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

layer_crowns_big_4 <- crowns_big_4_polys %>% 
  st_as_sf() %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Create map
m <- leaflet() %>%
  addGlPolygons(
    data = layer_crowns_big_1,
    fillColor = "cyan",
    group = "layer_crowns_big_1",
    weight = 2,
    stroke = T
  ) %>%
  addGlPolygons(
    data = layer_crowns_big_2,
    fillColor = "cyan",
    group = "layer_crowns_big_2",
    weight = 2,
    stroke = T
  ) %>%
  addGlPolygons(
    data = layer_crowns_big_3,
    fillColor = "cyan",
    group = "layer_crowns_big_3",
    weight = 2,
    stroke = T
  ) %>%
  addGlPolygons(
    data = layer_crowns_big_4,
    fillColor = "cyan",
    group = "layer_crowns_big_4",
    weight = 2,
    stroke = T
  ) %>%
  addWMSTiles(
    "https://wmts10.geo.admin.ch/1.0.0/ch.swisstopo.swissimage-product/default/current/3857/{z}/{x}/{y}.jpeg",
    layers = "swissimage-product",
    group = "Aerial imagery - swisstopo",
    layerId = "swissimage-product",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "swisstopo"
  )  %>%
  addWMSTiles(
    "https://wmts10.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
    layers = "pixelkarte-farbe",
    group = "Location map color - swisstopo",
    layerId = "pixelkarte-farbe",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "swisstopo"
  ) %>%
  addLayersControl(
    baseGroups = c("Aerial imagery - swisstopo", "Location map color - swisstopo"),
    overlayGroups = c("layer_crowns_big_1","layer_crowns_big_2","layer_crowns_big_3","layer_crowns_big_4"),
    options = layersControlOptions(collapsed = TRUE)
  ) 
m

#-----------------------------------------------------
# Different approach, test stem segmentation
#-----------------------------------------------------

# Not realistic, too few points for the stems using airborne lidar, terrestrial lidar would be needed for that.

#-----------------------------------------------------
# Different approach, VHM > 1m, segment using  thiessen polygons
#-----------------------------------------------------

# 1. Smooth CHM with fixed SWS = 3x3
chm_s <- focal(vhm_sa, w = matrix(1,5,5), fun = median)

# 2. Detect trees with TWS = 5 if COV > 70%, TWS = 7 otherwise
ttops <- locate_trees(
  chm_s,
  lmf(ws = 7, hmin = 1, shape = "circular")
)

# Function to ensure polygons are valid, single-part, and distinct
clean_polygons <- function(polys){
  polys %>%
    st_make_valid() %>%                   # Fix invalid geometries
    st_collection_extract("POLYGON") %>%  # Keep only POLYGON geometries
    st_cast("POLYGON")                    # Convert multipolygons to single-part polygons
  # distinct(geometry, .keep_all = TRUE) # Optional: remove exact duplicates
}

thiessen_poly <- ttops %>%
  st_geometry() %>%                # Extract tree point geometries
  do.call(c, .) %>%               # Combine into a single geometry list
  st_voronoi() %>%                # Compute Voronoi polygons
  clean_polygons() %>%            
  st_set_crs(2056) %>%            # Assign CRS
  st_crop(ext(vhm_sa)) %>%           # Crop to VHM extent
  clean_polygons() %>% 
  st_sf(geometry = .)


# Create binary mask: 1 if height >= threshold, 0 otherwise
vhm_mask <- (vhm_sa >= 1)*1

# Convert raster mask to EBImage object for morphological operations
library(EBImage) 
vhm_img <- Image(as.matrix(vhm_mask, wide = TRUE))

# Fill small holes in binary raster
vhm_filled <- vhm_img %>%
  `!`() %>%                    # Invert image for hole detection
  bwlabel() %>%                 # Label connected components
  {
    lbl <- .
    tab <- table(lbl)
    small_holes <- as.numeric(names(tab[tab <= 4])) # Identify small holes
    lbl[lbl %in% small_holes] <- 0                                # Remove small holes
    lbl
  } %>%
  `>`(0) %>% `!`()             # Convert back to binary mask

# Convert filled binary mask back to SpatRaster
vhm_mask_filled <- rast(matrix(as.numeric(vhm_filled), nrow = nrow(vhm_mask)),
                        crs = crs(vhm_mask)) %>%
  setNames("vhm_high")
ext(vhm_mask_filled) <- ext(vhm_sa)

# Convert filled raster to polygons
library(tidyverse)
vhm_mask_poly <- as.polygons(vhm_mask_filled, dissolve = TRUE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  filter(vhm_high == 1) %>%           # Keep only high vegetation
  clean_polygons() %>%
  mutate(area_m2 = as.numeric(st_area(.))) %>%
  filter(area_m2 > 1) %>%
  select(-area_m2, -vhm_high)

# ezb_nf <- st_read(paste0(study_area_data_path,"allema_rischberg.gpkg"), layer="tlm_ebv")

# Count number of TLM points per polygon
wtr_poly_class <- vhm_mask_poly %>%
  mutate(n_points = lengths(st_intersects(., ezb_nf)),
         ID = row_number())  # Unique ID

multitrees <- wtr_poly_class %>% filter( n_points > 1) %>%
  distinct(geometry, .keep_all = TRUE)

# Split polygons with multiple trees using Thiessen polygons
multitrees_thiessen <- multitrees %>%
  st_intersection(., thiessen_poly) %>%
  clean_polygons() %>%
  mutate(n_points = lengths(st_intersects(., ttops)),
         tree_class = case_when(
           n_points == 1 ~ "tree",
           n_points == 0 ~ "small_no_ezb",
           TRUE ~ "other"
         ))

# Separate split polygons
trees_thiessen <- multitrees_thiessen %>% filter(tree_class == "tree")
small_thiessen <- multitrees_thiessen %>% filter(tree_class == "small_no_ezb")


ttops <- ttops %>%
  st_zm(drop = TRUE, what = "ZM") 
mapview(ttops) + mapview(chm_s)


#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------
# Functions
#-----------------------------------------------------

rast_to_polys <- function(r){
  r %>%
    as.polygons(dissolve = TRUE) %>%
    st_as_sf() %>%
    mutate(area_m2 = as.numeric(st_area(.))) %>%   # Calculate area of each polygon
    filter(area_m2 > 1) %>%    # Keep polygons above minimum area
    select(-area_m2)
}

# For more post processing
# TODO: Copy steps from \\speedy16-36\data_15\_PROJEKTE\2018_Lebensraumkarte_BAFU\Data\Siedlung\Results_2025\R_script_BKZH

#-----------------------------------------------------
# Smoothing the VHMs
#-----------------------------------------------------

vhm_sa_smooth3_mean <- focal(vhm_sa, w = matrix(1,3,3), fun = mean)
vhm_sa_smooth5_mean <- focal(vhm_sa, w = matrix(1,5,5), fun = mean)
vhm_sa_smooth3_med <- focal(vhm_sa, w = matrix(1,3,3), fun = median)
vhm_sa_smooth5_med <- focal(vhm_sa, w = matrix(1,5,5), fun = median)
vhm_sa_smooth3_max <- focal(vhm_sa, w = matrix(1,3,3), fun = max)
vhm_sa_smooth5_max <- focal(vhm_sa, w = matrix(1,5,5), fun = max)

#-----------------------------------------------------
# Watershed Approach
#-----------------------------------------------------

crowns_ws_mw3_mean <- lidR::watershed(vhm_sa_smooth3_mean)() %>%
  rast_to_polys() 
# 6697 features

crowns_ws_mw5_mean <- lidR::watershed(vhm_sa_smooth5_mean)() %>%
  rast_to_polys() 
# 4622 features

crowns_ws_mw3_med <- lidR::watershed(vhm_sa_smooth3_med)() %>%
  rast_to_polys() 
# 6826 features

crowns_ws_mw5_med <- lidR::watershed(vhm_sa_smooth5_med)()%>%
  rast_to_polys() 
# 4583 features

crowns_ws_mw3_max <- lidR::watershed(vhm_sa_smooth3_max)() %>%
  rast_to_polys() 
# 5074 features

crowns_ws_mw5_max <- lidR::watershed(vhm_sa_smooth5_max)()%>%
  rast_to_polys() 
# 3399 features


#-----------------------------------------------------
# Load in LN Data and filter only crowns on agricultural surface
#-----------------------------------------------------
# Set extent
sa_ext <- ext(
  sas$upper_left_e[which(sas$id == sa)],
  sas$lower_right_e[which(sas$id == sa)],
  sas$lower_right_n[which(sas$id == sa)],
  sas$upper_left_n[which(sas$id == sa)]
)

# Convert to correct format
sa_wkt <- st_as_text(
  st_geometry(
    st_as_sf(as.polygons(sa_ext))
  )
)

# Read BLW geoms
ln_sa <- st_read(LN_2025_path, layer="Landwirtschaftliche_Nutzungsflaechen_Schweiz_2025", wkt_filter = sa_wkt) 

# TEMP - Just union all the geoms
ln_filter <- st_union(ln_sa)

# Filter the crowns to keep only what is on agricultural land
# -------------

crowns_ws_mw3_mean <- crowns_ws_mw3_mean[st_within(crowns_ws_mw3_mean, ln_filter, sparse = FALSE), ]
# 1301 features

crowns_ws_mw5_mean <- crowns_ws_mw5_mean[st_within(crowns_ws_mw5_mean, ln_filter, sparse = FALSE), ]
# 1101 features

crowns_ws_mw3_med <- crowns_ws_mw3_med[st_within(crowns_ws_mw3_med, ln_filter, sparse = FALSE), ]
# 1382 features

crowns_ws_mw5_med <- crowns_ws_mw5_med[st_within(crowns_ws_mw5_med, ln_filter, sparse = FALSE), ]
# 1153 features

crowns_ws_mw3_max <- crowns_ws_mw3_max[st_within(crowns_ws_mw3_max, ln_filter, sparse = FALSE), ]
# 1366 features

crowns_ws_mw5_max <- crowns_ws_mw5_max[st_within(crowns_ws_mw5_max, ln_filter, sparse = FALSE), ]
# 1067 features

#* The numbers here are already more comparable as what we got before.
#* This seems to indicate that the segmentation method are most of all different 
#* in the forest and a bit less on agricultural openland.

library(terra)
library(sf)
library(data.table)
library(dplyr)
library(purrr)

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

#* plet(is_crown)
#* Is quite interesting. This contains everything that was segmented in at least one method as
#* being part of a tree crown. --> Kind of an ensemble tree crown detection. 
#* Problem is that individual tree segmentation (crown boundaries) is not visible here.
#* But could be used to make sure that all trees (even small ones only detected by max filter)
#* are in the end output.

# # Boundary uncertainty
# # ---------------
# 
# # Get the boundaries of the segmented trees (consider the different trees, and neighbors in all directions)
# crown_boundaries <- lapply(rasters, function(r){
#   boundaries(r, classes = TRUE, directions=8, falseval=NA, ignoreNA =FALSE)
# })
# 
# # Sum over all rasters
# # strong concentrated lines → stable crown boundaries
# # fuzzy thick areas → disagreement
# boundary_uncertainty <- sum(rast(crown_boundaries), na.rm = TRUE)
# 
# #* plet(crown_boundaries$mw3_mean)
# #* Actually cannot be used, because of the resolution the boundaries of the tree crowns
# #* are not correctly represented.
# #* Would maybe be useful to try with a raster template with a resolution smaller than 
# #* what was used for segmentation.

# Pairwise comparison
# ---------------


# Key concept
# Raster 1
  # | pixel | crown label |
  # | ----- | ----------- |
  # | p1    | 17          |
  # | p2    | 17          |
  # | p3    | 17          |
  # | p4    | 18          |

# Raser 2
  # | pixel | crown label |
  # | ----- | ----------- |
  # | p1    | 842         |
  # | p2    | 842         |
  # | p3    | 842         |
  # | p4    | 901         |

# vals <- data.table(id1 = vals1,id2 = vals2)
  # | r1 | r2  |
  # | -- | --- |
  # | 17 | 842 |
  # | 17 | 842 |
  # | 17 | 842 |
  # | 18 | 901 |

# dt[, .N, by = .(id1, id2)]  
  # | id1 | id2 | n_pixels |
  # | --- | --- | -------- |
  # | 17  | 842 | 3        |
  # | 18  | 901 | 1        |
  
  
# Function to compare the overlap
compute_overlap <- function(r1, r2){
  
  vals1 <- values(r1, mat = FALSE)
  vals2 <- values(r2, mat = FALSE)
  dt <- data.table(
    id1 = vals1,
    id2 = vals2
  )
  dt[is.na(id1), id1 := 0]
  dt[is.na(id2), id2 := 0]
  dt[
    ,
    .(n_pixels = .N),
    by = .(id1, id2)
  ]
}

# Function to get the area (proxy with number of cells) per segmented tree 
get_areas <- function(r){
  
  vals <- values(r, mat = FALSE)
  
  vals[is.na(vals)] <- 0
  
  dt <- data.table(id = vals)
  
  dt[
    ,
    .(area = .N),
    by = id
  ]
}

# Function to compare the segmentations
compare_segmentations <- function(name1, name2){
  
  r1 <- rasters[[name1]]
  r2 <- rasters[[name2]]
  
  ov <- compute_overlap(r1, r2)
  
  a1 <- get_areas(r1)
  a2 <- get_areas(r2)
  
  ov <- merge(
    ov,
    a1,
    by.x = "id1",
    by.y = "id",
    all.x = TRUE
  )
  
  setnames(ov, "area", "area1")
  
  ov <- merge(
    ov,
    a2,
    by.x = "id2",
    by.y = "id",
    all.x = TRUE
  )
  
  setnames(ov, "area", "area2")
  
  ov[, iou := n_pixels / (area1 + area2 - n_pixels)]
  
  ov[, method1 := name1]
  ov[, method2 := name2]
  
  ov
}

pairs <- combn(names(rasters), 2, simplify = FALSE)

all_overlaps <- rbindlist(
  lapply(pairs, function(p){
    
    compare_segmentations(p[1], p[2])
    
  })
)

#* Je dois reflechir a ce que j'ai la et ce que je veux / je dois faire avec cette table all_overlaps

# CHENIL A REVOIR COPIE COLLE DE CHATGPT
# Best match
best_matches <- all_overlaps[
  id1 != 0 & id2 != 0,
  .SD[which.max(iou)],
  by = .(method1, id1, method2)
]

# Stability
stability <- best_matches[
  ,
  .(
    mean_iou = mean(iou),
    sd_iou = sd(iou),
    n_matches = .N
  ),
  by = .(method1, id1)
]

stability <- all_overlaps[
  ,
  .(best_iou = max(iou)),
  by = .(method1, id1, method2)
]

stability_summary <- stability[
  ,
  .(
    mean_iou = mean(best_iou),
    sd_iou = sd(best_iou),
    n_matches = .N
  ),
  by = .(method1, id1)
]

ref_crowns <- crowns$mw5_mean

ref_crowns <- left_join(
  ref_crowns,
  stability,
  by = c("crown_id" = "id1")
)

mapview(ref_crowns, z="mean_iou")

stable <- stability[
  mean_iou >= 0.7
]

unstable <- stability[
  mean_iou < 0.4
]

unstable_polys <- ref_crowns[
  ref_crowns$crown_id %in% unstable$id1,
]

#-----------------------------------------------------
# Compare segmentations - using a reference
#-----------------------------------------------------

library(dplyr)
library(purrr)

# Combine all crowns in one list
crowns <- list(
  mw3_mean = crowns_ws_mw3_mean,
  mw5_mean = crowns_ws_mw5_mean,
  mw3_med  = crowns_ws_mw3_med,
  mw5_med  = crowns_ws_mw5_med,
  mw3_max  = crowns_ws_mw3_max,
  mw5_max  = crowns_ws_mw5_max
)

# Convert to sf and add IDs
crowns <- imap(crowns, function(x, name) {
  x <- st_as_sf(x)
  x$tree_id <- 1:nrow(x)
  x$method  <- name
  x
})

# Choose a reference layer to match the ids across methods
ref_method <- "mw5_mean" 
crown_ref <- crowns[[ref_method]]   

# Function to match the ids of the different methods to the ref
match_to_ref <- function(target, ref) {
  
  inter <- st_intersection(
    target %>% mutate(target_id = tree_id),
    ref    %>% mutate(ref_id = tree_id)
  )
  
  if (nrow(inter) == 0) return(NULL)
  
  inter$area <- st_area(inter)
  
  matches <- inter %>%
    group_by(target_id) %>%
    slice_max(area, n = 1) %>%
    ungroup() %>%
    st_drop_geometry() %>%   
    select(target_id, ref_id)
  
  target <- target %>%
    left_join(matches, by = c("tree_id" = "target_id"))
  
  return(target)
}

# Apply function across the different crowns
aligned <- imap(crowns, function(x, name) {
  if (name == ref_method) {
    x$ref_id <- x$tree_id
    return(x)
  }
  match_to_ref(x, crown_ref)
})

# Create template to rasterize the crowns
template <- rast(vhm_sa_smooth3_mean)

# Rasterize the crowns, using the ref id as field to rasterize
rasters <- map(aligned, function(x) {
  if (is.null(x)) return(NULL)
  
  r <- rasterize(
    vect(x),
    template,
    field = "ref_id",
    background = NA
  )
  
  return(r)
})

# Remove NULLs if any
rasters <- compact(rasters)

# Stack rasters
r_stack <- rast(rasters)

# Functions to compare the segementations
#----

# Agreement function (consensus)
agreement_fun <- function(vals) {
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) return(NA)
  
  tab <- table(vals)
  max(tab) / length(vals)
}

agreement <- app(r_stack, agreement_fun)

# # Entropy function (fuzziness of the result)
# entropy_fun <- function(vals) {
#   vals <- vals[!is.na(vals)]
#   
#   if (length(vals) == 0) return(NA)
#   
#   p <- table(vals) / length(vals)
#   -sum(p * log(p))
# }
# 
# entropy <- app(r_stack, entropy_fun)
# 
# # Consensus map
# consensus_fun <- function(vals) {
#   vals <- vals[!is.na(vals)]
#   
#   if (length(vals) == 0) return(NA)
#   
#   tab <- table(vals)
#   as.numeric(names(which.max(tab)))
# }
# 
# consensus <- app(r_stack, consensus_fun)

#* Quite cool the agreement map. Problem with choosing a reference is that if a method
#* different from the reference segmented a supplementary tree it will not show up here,
#* as the reference is a bit considered like the truth...

#-----------------------------------------------------
# Compare segmentations - using all trees
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

# Put all trees into one dataset
all_trees <- imap(crowns, function(x, name) {
  x <- st_as_sf(x)
  x$tree_id <- 1:nrow(x)
  x$method  <- name
  x$uid <- paste0(name, "_", x$tree_id)
  x
}) %>% bind_rows()

# Keep only the segmented trees that are in the agricultural area
all_trees <- all_trees[st_intersects(all_trees, ln_filter, sparse = FALSE), ]


#-----------------------------------------------------
# Identify the tree tops
#-----------------------------------------------------

# Following Silva 2016 methodology
ttops <- locate_trees(
            vhm_sa_smooth3_mean,
            lmf(ws = 7, hmin = 1, shape = "circular")
          ) %>%
          st_zm(drop = TRUE, what = "ZM") 

#* Tried tests with all the smoothed VHM variants. 
#* The difference between mean and median was not so significant, mean seemed to perform better
#* Using the max filter however, especially with the mw=5 allows to detect more smaller trees. 
#* But it also creates supplementary tree tops where it shouldn't.
#* Maybe the treetops with mwx filter mw=5 could be used to complete the list of identified trees
#* by focusing on the newly planted and only considering those that are not already intersecting one 
#* of the identified crowns.
 
ttops2 <- locate_trees(
  vhm_sa_smooth3_med,
  lmf(ws = 7, hmin = 1, shape = "circular")
) %>%
  st_zm(drop = TRUE, what = "ZM") 

