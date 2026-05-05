#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")


#-----------------------------------------------------
# Copy Data locally for improved processing
#-----------------------------------------------------

# Set the bat file path (needed since R Project)
bat_path <- file.path(getwd(), "src", "r", "002c_CopyData.bat")

# Copy the files and create the VRT
system2(
  osgeo4w_path,
  args = c(
    bat_path,
    VHM_S2_tif_path,
    VHM_S2_tif_local_path,
    VHM_S2_local_path
  ),
  wait = TRUE
)

