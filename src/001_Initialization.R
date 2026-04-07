#-----------------------------------------------------
# PATHS 
#-----------------------------------------------------

# General paths
orig_data_path <- "D:/BLW/ORIG_DATA/"
prj_data_path <- gsub("src","prj-data/",getwd())
study_area_data_path <- "D:/BLW/STUDY_AREA_DATA/"
osgeo4w_path <- "C:/Program Files/QGIS 3.40.2/OSGeo4W.bat"
  
# Data specific paths
VHM_path <- paste0(orig_data_path,"VHM/landesforstinventar-vegetationshoehenmodell_lidar_2025_2056.tif")
NF_path <- "D:/BLW/ORIG_DATA/BLW/nutzungsflaechen.gpkg"
TLM_EB_path <- paste0(orig_data_path,"TLM/SWISSTLM3D_2025.gpkg")
