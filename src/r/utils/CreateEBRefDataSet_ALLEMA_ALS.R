#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)
library(purrr)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

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

# Filter to keep only classes 37 (Obstanlagen), 38 (Hochstammobst) and 59 (Einzelbaum, Baumgruppe)
ALLEMA <- ALLEMA %>% 
  filter(Gehoelztyp %in% c(37,38,59)) %>%
  select(FK_Quadrat, OBJECT_ID, Gehoelztyp, LubiJahr_max) %>%
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
# Post-filtering of reference data
#-----------------------------------------------------

# The perimeters that can be considered
ALLEMA_Q_ref <- ALLEMA_Q_orig %>% 
  filter( ID_Quadrat %in% unique(ALLEMA_ALS_ref$FK_Quadrat)) 

# Fetch the reference trees that should be considered
ALLEMA_EB <- ALLEMA_orig %>% 
  filter(OBJECT_ID %in% unique(ALLEMA_ALS_ref$OBJECT_ID)) %>%
  st_zm(drop = TRUE, what = "ZM")

# Filter based on roudness
#---------------------------

# Add roudness info
ALLEMA_EB <- ALLEMA_EB %>%
  mutate(
    area_allema = st_area(Shape),
    perimeter = st_length(st_boundary(Shape)),
    roundness =
      as.numeric(
        (4 * pi * area_allema) / (perimeter^2)
      )
  )

# Keep only ref polygons with a roundness > 0.95
# Idea is to filter out polgons that do not represent a single tree but a group of trees
ALLEMA_EB <- ALLEMA_EB %>%
  filter(roundness > 0.95)

# Filter based on all VHM = 0
#---------------------------

library(terra)

# get the vhm (use 1m to speed everything up)
VHM_ALS <- rast(VHM_S1_1m_path)

# sf -> terra vector
ALLEMA_EB_v <- vect(ALLEMA_EB)

# output column
ALLEMA_EB$has_veg <- FALSE

# optional: progress bar
pb <- txtProgressBar(min = 0, max = nrow(ALLEMA_Q_ref), style = 3)

for(i in seq_len(nrow(ALLEMA_Q_ref))) {
  
  # -------------------------------------------------
  # current quadrant
  # -------------------------------------------------
  
  quad_id <- ALLEMA_Q_ref$ID_Quadrat[i]
  
  # extent/perimeter of current quadrant
  quad <- vect(ALLEMA_Q_ref[i, ])
  
  # -------------------------------------------------
  # crop VHM to current quadrant
  # -------------------------------------------------
  
  VHM_crop <- crop(VHM_ALS, ext(quad))
  
  # optional but often safer/faster
  VHM_crop <- mask(VHM_crop, quad)
  
  # -------------------------------------------------
  # fetch matching EB polygons
  # -------------------------------------------------
  
  idx <- which(ALLEMA_EB$FK_Quadrat == quad_id)
  
  # skip if no polygons
  if(length(idx) == 0) {
    setTxtProgressBar(pb, i)
    next
  }
  
  eb_sub <- ALLEMA_EB_v[idx, ]
  
  # -------------------------------------------------
  # vegetation presence
  # -------------------------------------------------
  
  mx <- terra::extract(
    VHM_crop,
    eb_sub,
    fun = max,
    na.rm = TRUE
  )
  
  # polygons having vegetation
  ALLEMA_EB$has_veg[idx] <- mx[,2] > 0
  
  # progress
  setTxtProgressBar(pb, i)
}

close(pb)

# keep only polygons with vegetation
ALLEMA_EB_ref <- ALLEMA_EB[ALLEMA_EB$has_veg, ]%>%
  select(-has_veg)

#-----------------------------------------------------
# Export the data that will be used to test the segmentation
#-----------------------------------------------------

st_write(ALLEMA_Q_ref, "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_Q_ref.gpkg", append = FALSE)

st_write(ALLEMA_EB_ref, "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_EB_ref.gpkg", append = FALSE)
