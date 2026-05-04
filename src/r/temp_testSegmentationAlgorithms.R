
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
vhm_sa <- rast(paste0(study_area_data_path,sa,"_vhm.tif"))

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


