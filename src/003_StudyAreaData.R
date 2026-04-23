#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Sourcing initialization code (paths and such) 
source("001_Initialization.R")

#-----------------------------------------------------
# Prepare study area data
#-----------------------------------------------------

# Get study areas extent specification file
study_areas <- read.csv2(file = paste0(git_root_path,"prj-data/study_areas.csv"), sep=",")

# Loop through the study areas
for(i in 1:nrow(study_areas)){
  
  # Define the study area bbox coordinates
  coords <- paste(
    study_areas$upper_left_e[i],
    study_areas$upper_left_n[i],
    study_areas$lower_right_e[i],
    study_areas$lower_right_n[i],
    sep = ","
  )
  
  # Crop original data to extent 
  system2(osgeo4w_path, args = c(
    "003b_CropStudyAreaData.bat", 
    paste0(study_area_data_path, "", study_areas$id[i],".gpkg"),
    NF_path,
    TLM_EB_path,
    TLM_EBv_path,
    VHM_path,
    paste0('"', coords, '"'),
    HM_EBuG_path
    ))
}


#-----------------------------------------------------
# Fetch ALS data corresponding to study areas
#-----------------------------------------------------

# Get study areas ALS file
ALS_sas <- read.csv2(file = paste0(git_root_path,"prj-data/ALS_study_areas.csv"), sep=",")

for(sa in study_areas$id){
  
  # Create directory if doesn't exist
  las_dir <- paste0(study_area_data_path,sa,"_las/")
  if (!dir.exists(las_dir)) {
    dir.create(las_dir)
  }
  
  # Filter the LAS files associated to the study area
  ALS_sa <- ALS_sas[which(ALS_sas$sa_id == sa),]
  
  # Loop over all the LAS files
  for(i in 1:nrow(ALS_sa)){
    
    # Copy LAS file to directory (needed to create a catalog to avoid edge effects)
    file.copy(from = paste0(ALS_path,ALS_sa$file_name[i]),
              to   = las_dir)
  }
  
}