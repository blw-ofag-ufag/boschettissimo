#-----------------------------------------------------
# PATHS 
#-----------------------------------------------------

# General paths
orig_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/"
study_area_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/STUDY_AREA_DATA/"

# Data specific paths
VHM_S1_path <- "//speedy12-37/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_LiDAR_NFI/2024/landesforstinventar-vegetationshoehenmodell_lidar_2024_2056.tif"
VHM_S1_1m_path <- "//speedy11-12-fs/data_17/_LIDAR/ALS_CH/__VHM_ALS_CH/20250217_VHM_ALS_CH_SWISS_1m_2056.tif"
VHM_S2_path <- "//speedy11-12-fs/data_17/_LIDAR/ALS_CH/__VHM_ALS_CH/_Kantone/SWISS2_forBoschettissimo/2024_05_VHM_ALS_CH_SWISS2_0.5m_2056.tif"
TLM_EB_path <- paste0(orig_data_path,"TLM/SWISSTLM3D_2025.gpkg")
TLM_EBv_input_path <- "//speedy12-37/data_25/Vegetationmaxima/07_WSL/"
TLM_EBv_path <- paste0(orig_data_path,"TLM/TLM_EBv.gpkg")
NF_path <- paste0(orig_data_path,"BLW/nutzungsflaechen.gpkg") # will be changed once data received from BLW
LN_2025_path <- paste0(orig_data_path,"BLW/LWB_Nutzungsflaechen_Derivat_BGDI_2025.gdb")
HM_EBuG_path <- "//speedy16-36/data_15/_PROJEKTE/2018_Lebensraumkarte_BAFU/Data_Sharing/HabitatMap_v1_2_202512/N2025_einzelbaum_gebuesche_2025_v1_0_20260106.gdb"
ALS_path <- "//speedy11-12-fs/data_17/_LIDAR/ALS_CH/nDSM/"
CH_1000_path <- "//speedy16-36/data_15/_PROJEKTE/_SNAPRASTER/ExampleRaster/ExampleRaster_CHbin_1000m_LV95.tif"
ALS_ndms <- "//speedy11-12-fs/data_17/_LIDAR/ALS_CH/ALS_CH_db/mv_als_ch_ndsm_20260511.gpkg"
forest_mask_path <- "//speedy11-12-fs/data_17/_GEOBASISDATEN/DATA_2025/ID164.23_Waldmaske/forest_mask_switzerland_shrub_20250902_v1/NFI_forest_mask_20250217.gpkg"
settlement_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/SIEDLUNG/siedlung_2025_2056.gpkg"

# Local paths (for faster processing)
orig_data_local_path <- "D:/BOSCHETTISSIMO/ORIG_DATA/"
VHM_S2_tif_local_path <- "D:/BOSCHETTISSIMO/ORIG_DATA/VHM/"
VHM_S2_local_path <- "D:/BOSCHETTISSIMO/ORIG_DATA/VHM/VHM_SWISS2.vrt"
treeseg_data_local_path <- "D:/BOSCHETTISSIMO/PROCESSED_DATA/TREE_SEG/"

# (TEMP only for TG)
# Path to combined unfiltered EB data from TLM (produced using 004d_Combine_TLMEB.bat)
combined_tlm_eb_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/TLM/combined_tlm_eb_TG.gpkg"
seg_tlm_EB_log_file <- "docs/logging/seg_tlm_EB_TG.txt"

# Processing specific paths (personal, to be changed)
osgeo4w_path <- "C:/Program Files/QGIS 3.40.2/OSGeo4W.bat"

