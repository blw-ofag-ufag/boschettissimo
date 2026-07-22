#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(terra)
library(sf)
library(dplyr)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# LN
#-----------------------------------------------------

# Limit analysis on designated areas
LN <- st_read(LN_2025_path, query = "SELECT * FROM Landwirtschaftliche_Nutzungsflaechen_Schweiz_2025")

# Limit to classes desired by the BLW
LN_interest <- LN %>%
  # Filter out all non BLW parcels
  filter(lnf_code < 900) %>%  
  # Filter out lnf_codes mentioned in https://wslch365.sharepoint.com/:x:/r/sites/prj_Boschettissimo/Freigegebene%20Dokumente/General/Copie%20et%20extrait%20de%20Matrix_V_2026_03.xlsx?d=w08605ef0fa574e15b06cd912146582b3&csf=1&web=1&e=aiv6Oh tab SpezialFaelle
  filter(!(lnf_code %in% c(713,714,801,802,803,804,807,808,810,811,812,813,814,847,848,849)))

# Export as geopackage (faster for spatial queries later on)
st_write(LN_interest,paste0(orig_data_path,"BLW/LWB_mask.gpkg"))

