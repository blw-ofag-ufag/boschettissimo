#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)
library(purrr)

#-----------------------------------------------------
# Prepping the ALS dataset
#-----------------------------------------------------

# Import the ALS data
ALS <- st_read("//speedy11-12-fs/data_17/_LIDAR/ALS_CH/ALS_CH_db/mv_als_ch_ndsm_20260511.gpkg")

# Filter to keep only nation-wide ALS
ALS <- ALS %>% filter(source %in% c("SWISS","SWISS2"))

ALS <- ALS %>% 
  select(source, year, x_min, y_min) %>%
  st_drop_geometry() 

# # Check that all coordinates round up to 10
# which(ALS$x_min != round(ALS$x_min, -1)) # yes
# which(ALS$y_min != round(ALS$y_min, -1)) # yes

#-----------------------------------------------------
# Prepping the ALLEMA dataset
#* Ex FK-Quadrant 581166 : 
#*   OBJECTID 152189 has Erh_Jahr = 2024 and LubiJahr = 2017
#*   OBJECTID 152330 has Erh_Jahr = 2024 and LubiJahr = 2020
#*   --> If an object stayed the same in 2020 as in 2017 it will keep LubiJahr 2017 even though it is visible in 2020
#-----------------------------------------------------

# Import the ALLEMA data
ALLEMA_orig <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/Wald_Gehoelz_ErhZyk2.gdb", 
               layer = "Wald_Gehoelz_ErhZyk2_3D")

# Add an ID
ALLEMA_orig$OBJECT_ID <- 1:nrow(ALLEMA_orig)

# # Identify geometries that were not attributed to an ALLEMA Quadrat
# nrow(unique(ALLEMA[which(is.na(ALLEMA$FK_Quadrat)),])) # 568

# Filter out geometries that were not assigned to an ALLEMA Quadrant
ALLEMA <- ALLEMA_orig %>%  
  filter(!is.na(FK_Quadrat))      

# Apply the latest LubiJahr to the whole Quadrant
ALLEMA <- ALLEMA %>%
  group_by(FK_Quadrat) %>%
  mutate(LubiJahr_max = max(LubiJahr)) %>%
  ungroup() 

# Filter to keep only single tree polygons
ALLEMA <- ALLEMA %>% 
  filter(Gehoelztyp == 59) %>%
  select(FK_Quadrat, OBJECT_ID, LubiJahr_max) %>%
  st_drop_geometry()

# Import the perimeter of the ALLEMA Quadrants
ALLEMA_Q_orig <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/ALLEMA_Quadranten.shp")

# Add the xmin ymin info to be able to match with the ALS quadrant
ALLEMA_Q <- ALLEMA_Q_orig %>%
  mutate(bbox = map(geometry, st_bbox)) %>%
  mutate(
    x_min = round(as.integer(map_dbl(bbox, "xmin")),-1),
    y_min = round(as.integer(map_dbl(bbox, "ymin")),-1)
  ) %>%
  select(-bbox) %>%
  select(ID_Quadrat, x_min, y_min) %>%
  st_drop_geometry()

# Join the ALLEMA_Q xmin, ymin info to the main ALLEMA table
ALLEMA <- left_join(ALLEMA, ALLEMA_Q, by=join_by(FK_Quadrat == ID_Quadrat))

# # Identify ALLEMA Quadrats that are not comprised in the perimeter dataset of the ALLEMA Quadrants
# unique(ALLEMA[which(is.na(ALLEMA$x_min)),"FK_Quadrat"]) # 9

ALLEMA <- ALLEMA %>%
  filter(!is.na(x_min)) # Filter out the geometries belongig to the 9 missing ALLEMA_Q perimeters

#-----------------------------------------------------
# Matching ALS year +/- 1 year with ALLEMA LubiJahr_max
#-----------------------------------------------------

# Join the ALLEMA and ALS datasets
ALLEMA_ALS_ref <- left_join(ALLEMA, ALS, by=c("x_min", "y_min"), relationship = "many-to-many")

# Keep only the ALLEMA single trees that have a matching ALS year +/- 1 year (y_buffer) with ALLEMA LubiJahr_max
y_buffer <- 1
ALLEMA_ALS_ref <- ALLEMA_ALS_ref %>%
  filter(abs(year - LubiJahr_max) <= y_buffer)

# Get some idea about which ALLEMA Quadrants could be used as reference
length(unique(ALLEMA_ALS_ref$FK_Quadrat)) # 56 Quadrants can be used as reference
table(ALLEMA_ALS_ref$source) # Only the first VHM can be used (newest Lubi_Jahr used is 2022 in the ALLEMA dataset)
table(ALLEMA_ALS_ref$LubiJahr_max) # The years covered span from 2017 to 2022

#-----------------------------------------------------
# Export the data that will be used to test the segmentation
#-----------------------------------------------------

# The perimeters that can be considered
ALLEMA_Q_ref <- ALLEMA_Q_orig %>% 
  filter( ID_Quadrat %in% unique(ALLEMA_ALS_ref$FK_Quadrat)) %>%
  select( ID_Quadrat, geometry)
st_write(ALLEMA_Q_ref, "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_Q_ref.gpkg")

# The segmented trees that are there
ALLEMA_EB_ref <- ALLEMA_orig %>% 
  filter(OBJECT_ID %in% unique(ALLEMA_ALS_ref$OBJECT_ID)) %>%
  select(FK_Quadrat, Shape, LubiJahr)
st_write(ALLEMA_EB_ref, "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_EB_ref.gpkg")
