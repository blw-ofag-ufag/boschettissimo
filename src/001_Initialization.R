#-----------------------------------------------------
# PATHS 
#-----------------------------------------------------

# General paths
orig_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/"
study_area_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/STUDY_AREA_DATA/"

# Git relative paths
git_root_path <- paste0(rprojroot::find_root(rprojroot::is_git_root),"/")
  
# Data specific paths
VHM_path <- "//speedy12-37/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_LiDAR_NFI/2025/landesforstinventar-vegetationshoehenmodell_lidar_2025_2056.tif"
TLM_EB_path <- paste0(orig_data_path,"TLM/SWISSTLM3D_2025.gpkg")
TLM_EBv_input_path <- "//speedy12-37/data_25/Vegetationmaxima/07_WSL/"
TLM_EBv_path <- paste0(orig_data_path,"TLM/TLM_EBv.gpkg")
NF_path <- paste0(orig_data_path,"BLW/nutzungsflaechen.gpkg") # will be changed once data received from BLW

# Processing specific paths (personal, to be changed)
osgeo4w_path <- "C:/Program Files/QGIS 3.40.2/OSGeo4W.bat"
