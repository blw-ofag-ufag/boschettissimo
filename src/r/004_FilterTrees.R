#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)

# Sourcing initialization code (paths and such) and the shared filters
source("src/r/001_Initialization.R")
source("src/r/filter_trees.R")

#-----------------------------------------------------
# Filter the merged segmented trees and save the result
#-----------------------------------------------------

trees_SWISS1 <- st_read(trees_SWISS1_path, quiet = TRUE)
filtered_SWISS1 <- filter_segmented_trees(trees_SWISS1)
st_write(filtered_SWISS1, filtered_trees_SWISS1_path, delete_dsn = TRUE)
rm(trees_SWISS1); rm(filtered_SWISS1)

trees_SWISS2 <- st_read(trees_SWISS2_path, quiet = TRUE)
filtered_SWISS2 <- filter_segmented_trees(trees_SWISS2)
st_write(filtered_SWISS2, filtered_trees_SWISS2_path, delete_dsn = TRUE)
