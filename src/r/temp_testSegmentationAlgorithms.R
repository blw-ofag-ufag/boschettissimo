
# Libraries
library(lidR)
library(terra)

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
