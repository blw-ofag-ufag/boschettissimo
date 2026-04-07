#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Sourcing initialization code (paths and such) 
source("001_Initialization.R")

#-----------------------------------------------------
# Prepare study area data
#-----------------------------------------------------

# Get study areas extent specification file
study_areas <- read.csv2(file = paste0(prj_data_path,"study_areas.csv"))

# Loop through the study areas
for(i in 1:nrow(study_areas)){
  
  # Crop original data to extent 
  system2("C:/Program Files/QGIS 3.40.2/OSGeo4W.bat", args = c(
    "003b_CropStudyAreaData.bat", 
    paste0(study_area_data_path, "", study_areas$area_id[i],".gpkg"),
    NF_path,
    TLM_EB_path,
    VHM_path,
    study_areas$upper_left_e[i],
    study_areas$upper_left_n[i],
    study_areas$lower_right_e[i],
    study_areas$lower_right_n[i]
    ))
}
