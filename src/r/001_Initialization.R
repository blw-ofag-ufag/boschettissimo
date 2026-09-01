#-----------------------------------------------------
# PATHS 
#-----------------------------------------------------

# General paths
orig_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/"

# Data specific paths
VHM_S1_path <- "//speedy12-37/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_LiDAR_NFI/2024/landesforstinventar-vegetationshoehenmodell_lidar_2024_2056.tif"
VHM_S1_1m_path <- "//speedy11-12-fs/data_17/_LIDAR/ALS_CH/__VHM_ALS_CH/20250217_VHM_ALS_CH_SWISS_1m_2056.tif"
VHM_S2_path <- "//speedy11-12-fs/data_17/_LIDAR/ALS_CH/__VHM_ALS_CH/_Kantone/SWISS2_forBoschettissimo/2024_05_VHM_ALS_CH_SWISS2_0.5m_2056.tif"
LN_2025_path <- paste0(orig_data_path,"BLW/LWB_Nutzungsflaechen_Derivat_BGDI_2025.gdb")
CH_1000_path <- "//speedy16-36/data_15/_PROJEKTE/_SNAPRASTER/ExampleRaster/ExampleRaster_CHbin_1000m_LV95.tif"
swisstlm3d_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/TLM/SWISSTLM3D_2025.gpkg"
settlement_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/SIEDLUNG/siedlung_2025_2056.gpkg"
swissimage_path <- paste0(orig_data_path,"SWISSIMAGE/")
dhm25_raw_path <- "//katze/geolib/dhm25/DHM25_MM_ASCII_GRID/ASCII_GRID_1part/dhm25_grid_raster.asc"
dhm25_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/DEM/dhm25_grid_raster_2056.tif"
bff_raw_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/BLW/LW Kulturflächen_Originaldaten_2025/2025/Biodiversitätsförderflächen"
bff_path <- file.path(bff_raw_path, "BFF_2025.gpkg")
trees_SWISS1_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/PROCESSED_DATA/TREE_SEG/TREE_SEG_merged_13KT_SWISS1.gpkg"
trees_SWISS2_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/PROCESSED_DATA/TREE_SEG/TREE_SEG_merged_13KT_SWISS2.gpkg"
VHM_ADS_2024_path <- "//speedy11-12-fs/data_17/_GEOBASISDATEN/DATA_2024/ID164.19_Vegetatonshoehenmodell/rasterdaten/landesforstinventar_vegetationshoehenmodell_stereo_2023_2056.tif"

# Local paths (for faster processing)
treeseg_data_local_path <- "D:/BOSCHETTISSIMO/PROCESSED_DATA/TREE_SEG/"
