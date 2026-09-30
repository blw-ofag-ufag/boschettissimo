#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)
library(dbscan)   # fast fixed-radius clustering (trunk mode) + kNN distances
library(igraph)   # connected components (crown mode)

# Sourcing initialization code (paths and such) and the orchard functions
source("src/r/001_Initialization.R")
source("src/r/identify_orchards.R")

#-----------------------------------------------------
# Config
#-----------------------------------------------------

max_dist       <- 50      # [m] max distance between neighbouring trees
min_trees      <- 5       # minimum number of trees per orchard
dist_mode      <- "trunk" # "trunk": distance between tree centres (points)
                          # "crown": distance between crown polygon edges
outline_buffer <- 5       # [m] buffer around trees/edges for the outline

#-----------------------------------------------------
# SWISS1
#-----------------------------------------------------

filtered_SWISS1 <- st_read(filtered_trees_SWISS1_path, quiet = TRUE)

res_SWISS1 <- identify_orchards(filtered_SWISS1,
                                max_dist       = max_dist,
                                min_trees      = min_trees,
                                dist_mode      = dist_mode,
                                outline_buffer = outline_buffer)

cat(sprintf("SWISS1: %d trees, %d orchards, %d trees in orchards\n",
            nrow(res_SWISS1$trees), nrow(res_SWISS1$orchards), sum(res_SWISS1$trees$is_orchard)))

# Add the orchard membership attributes to the existing filtered trees dataset
st_write(res_SWISS1$trees, filtered_trees_SWISS1_path, delete_dsn = TRUE)

# Save the identified orchards as their own (new) dataset
st_write(res_SWISS1$orchards, orchards_SWISS1_path, delete_dsn = TRUE)

rm(filtered_SWISS1); rm(res_SWISS1)

#-----------------------------------------------------
# SWISS2
#-----------------------------------------------------

filtered_SWISS2 <- st_read(filtered_trees_SWISS2_path, quiet = TRUE)

res_SWISS2 <- identify_orchards(filtered_SWISS2,
                                max_dist       = max_dist,
                                min_trees      = min_trees,
                                dist_mode      = dist_mode,
                                outline_buffer = outline_buffer)

cat(sprintf("SWISS2: %d trees, %d orchards, %d trees in orchards\n",
            nrow(res_SWISS2$trees), nrow(res_SWISS2$orchards), sum(res_SWISS2$trees$is_orchard)))

# Add the orchard membership attributes to the existing filtered trees dataset
st_write(res_SWISS2$trees, filtered_trees_SWISS2_path, delete_dsn = TRUE)

# Save the identified orchards as their own (new) dataset
st_write(res_SWISS2$orchards, orchards_SWISS2_path, delete_dsn = TRUE)

rm(filtered_SWISS2); rm(res_SWISS2)
