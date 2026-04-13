#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(lidR)
library(terra)

# Sourcing initialization code (paths and such) 
source("001_Initialization.R")

# Get study areas ALS file
ALS_sas <- read.csv2(file = paste0(git_root_path,"prj-data/ALS_study_areas.csv"), sep=",")

# Get the study areas
sas <- read.csv2(file = paste0(git_root_path,"prj-data/study_areas.csv"), sep=",")

#-----------------------------------------------------
# Tree segementation
#-----------------------------------------------------

#*****************************************************
#* lidR provides 4 segmentation algorithms
#* watershed --> often splits one tree into multiparts
#* li 2012 --> segmentation on point cloud. Tests inconclusive, paper mentions optimized for coniferous trees.
#* dalponte 2016 --> region growing algorithm, paper mentions good for trees with dbh > 80cm, less for smaller trees.
#* silva 2016 --> uses tree tops and vhm. From tests was the most conclusive one.
#*****************************************************

# Function that defines the moving window size based on height
# TODO : first implementation based on test for sa rickenbach, 1st ALS file
f <- function(h) {
  ifelse(h > 4.5, 9, 3)
}

# Loop through the study areas
for(sa in sas$id){
  
  # Initialize an empty tree top df
  ttops_sa <- data.frame()
  
  # Filter the LAS files associated to the study area
  ALS_sa <- ALS_sas[which(ALS_sas$sa_id == sa),]
  
  # Load the VHM associated to the study area
  vhm_sa <- rast(paste0(study_area_data_path,sa,"_vhm.tif"))
  
  # Get two smoothed VHMS, one for the big trees identification, one for the crown delimitation
  chm_7 <- focal(vhm_sa, w = matrix(1,7,7), fun = mean) # Big tree identification
  chm_3 <- focal(vhm_sa, w = matrix(1,3,3), fun = mean) # Crown delimitation
  
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
  
  # Identify the big trees
  ttops_big <- locate_trees(chm_7, lmf(ws= f, hmin = 1.37, shape="circular"))
  ttops_big$treeSize <- "big"
  
  # Identify the small new trees
  # TODO see if this should be kept or not, output not super ideal, test with several study areas
  ttops_small <- locate_trees(vhm_sa, lmf(ws= f, hmin = 1.37, shape="circular"))
  
  # Find nearest big trees point for each small tree point and compute the distance between them
  nearest_big <- st_nearest_feature(ttops_small, ttops_big)
  dist_nearest <- st_distance(
    ttops_small,
    ttops_big[nearest_big, ],
    by_element = TRUE
  )
  
  # Filter to keep only small trees that are at least 10m away from an already identified big tree
  ttops_small_filtered <- ttops_small[dist_nearest > units::set_units(7, "m"), ]
  ttops_small_filtered$treeSize <- "small"
  
  # Bind them together and to the study area treetop df
  ttops_sa <- rbind(ttops_sa,ttops_big, ttops_small_filtered)
  
  # Recreate the tree id column
  ttops_sa$treeID <- seq_len(nrow(ttops_sa))
  
  # Segment the trees from the identified tree tops
  eb_sa <- segment_trees(las_veg, silva2016(chm_3, ttops_sa, max_cr_factor=1))
  # plot(eb_sa, bg = "white", size = 4, color = "treeID")

  # Keep only single trees that are on wanted LWN areas
  # TODO
  
  # Export the tree tops 
  st_write(ttops_sa, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="treetops", append = F)
  
  # and the crowns to compare to other products
  # TODO check if geom="concave", concaveman = c(5,0.5) or geom="convex" or any other combination would be best
  crowns <- crown_metrics(eb_sa, func = NULL, treeID = "IDws", geom="convex")
  st_write(crowns, dsn=paste0(study_area_data_path,sa,".gpkg"), layer="crowns", append = F)
  
}



