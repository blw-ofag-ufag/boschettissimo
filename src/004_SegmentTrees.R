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

# Loop through the study areas
for(sa in sas$id){
  
  # Filter the LAS files associated to the study area
  ALS_sa <- ALS_sas[which(ALS_sas$sa_id == sa),]
  
  # Load the VHM associated to the study area
  vhm_sa <- rast(paste0(study_area_data_path,sa,"_vhm_S2.tif"))
  
  # Have VHM in memory (for crown delimitation and also segmentation of big raster (engelswilen))
  vhm_sa <- toMemory(vhm_sa)
  
  # #***************************************************************************
  # # COMMENTED OUT, SEGMENTED LAS NOT NEEDED AT THE MOMENT, CHECK IN FUTURE
  # # Path to las files
  # las_dir <- paste0(study_area_data_path,sa,"_las/")
  # 
  # # Load the las catalogue
  # ctg <- readLAScatalog(las_dir)
  # 
  # # Clip to the study area
  # las <- clip_rectangle(ctg, 
  #                sas$upper_left_e[which(sas$id == sa)]-100,
  #                sas$lower_right_n[which(sas$id == sa)]-100,
  #                sas$lower_right_e[which(sas$id == sa)]+100,
  #                sas$upper_left_n[which(sas$id == sa)]+100)
  # 
  # # Filter to keep only the vegetation points
  # las_veg <- filter_poi(las, Classification == 3); rm(las)
  # 
  # # Segment the trees using the watershed algorithm
  # las_eb <- segment_trees(
  #   las_veg,
  #   lidR::watershed(vhm_sa, th_tree = 2, ext = 2)
  # ); rm(las_veg)
  # # plot(las_eb, bg = "white", size = 4, color = "treeID")
  # 
  # # Export the segmented trees
  # writeLAS(las_eb, paste0(study_area_data_path,sa,"_segmented_trees.laz")); rm(las_eb)
  # #***************************************************************************
  
  # Keep only single trees that are on wanted LWN areas
  # TODO
  
  # Get the 2D crown delimitation of the single trees using the same algorithm
  seg_algo <- lidR::watershed(vhm_sa, th_tree = 2, ext = 2)
  crowns <- seg_algo()
  crown_polys <- as.polygons(crowns, dissolve = TRUE); rm(vhm_sa); rm(crowns);

  # Export the crowns to compare to other products
  writeVector(crown_polys, filename=paste0(study_area_data_path,sa,".gpkg"), layer="crowns", insert = TRUE, overwrite = TRUE)
  
}



