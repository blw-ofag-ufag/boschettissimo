#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(lidR)
library(terra)
library(sf)
library(dplyr)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# TEMP - choose one study area to perform first tests on
# TODO - apply to all study areas
sa <- "allema_rischberg"

# Get study areas ALS file
ALS_sas <- read.csv2(file = "data/ALS_study_areas.csv", sep=",")

# Get the study areas
sas <- read.csv2(file = "data/study_areas.csv", sep=",")

# Filter the LAS files associated to the study area
ALS_sa <- ALS_sas[which(ALS_sas$sa_id == sa),]

# Load the VHM associated to the study area
vhm_sa <- rast(paste0(study_area_data_path,sa,"_vhm_S2.tif"))

# TEMP - choose one of the segmentation method produced crown
# TODO - when final method is chosen choose appropriate crown
crown_layer <- "crowns_ws_mw5_max"
crowns_sa <- st_read(dsn=paste0(study_area_data_path,sa,".gpkg"), layer=crown_layer)

#-----------------------------------------------------
# Clean crown polygons 
# TEMP - WIll have to be done in precedant steps in future
#-----------------------------------------------------

# Make valid geometries
crowns_sa <- st_make_valid(crowns_sa)

# Function to split MULTIPOLYGON to POLYGON 
# REM: somehow st_cast(POLYGON) makes errors and only keeps the first in the list, not necessarily the largest
geom_list <- lapply(seq_len(nrow(crowns_sa)), function(i) {
  
  # Fetch the geometry of the crown
  geom <- st_geometry(crowns_sa[i, ])
  
  # Split multipart geometries
  parts <- st_cast(geom, "POLYGON")
  
  # Duplicate attributes for each part
  sf_part <- st_sf(
    crowns_sa[rep(i, length(parts)), ],
    geometry = parts
  )
  
  return(sf_part)
})
exploded_crown <- do.call(rbind, geom_list)

# Get the area of the exploded crown geometries
exploded_crown$area <- st_area(exploded_crown)

# # Diagnostic to compare largest area of the multipolygon to the total area
# # Max difference = 14m2 for allema_rischberg (focal_max = 2167), ok to keep largest
# fragmentation_diag <- exploded_crown %>%
#   group_by(focal_max) %>%
#   summarise(
#     n_parts = n(),
#     total_area = sum(as.numeric(area)),
#     largest_area = max(as.numeric(area)),
#     diff = total_area - largest_area
#   ) %>%
#   arrange(desc(n_parts))

# Keep only largest polygons for each crown
crowns_sa <- exploded_crown %>%
  group_by(focal_max) %>%
  slice_max(area, n = 1) %>%
  ungroup() %>%
  mutate(geom = geometry) %>%
  select(focal_max, geom)

# Add unique tree ID
crowns_sa$tree_id <- 1:nrow(crowns_sa)

#-----------------------------------------------------
# Computing the ecological parameters per tree
#-----------------------------------------------------

# Geometry metrics
#-----------------------------------------------------

# Area and diameter
crowns_sa <- crowns_sa %>%
  mutate(
    area_m2 = as.numeric(st_area(.)),
    diameter_m = 2 * sqrt(area_m2 / pi)
  )

# Centroids
centroids <- st_centroid(crowns_sa)

coords <- st_coordinates(centroids)

crowns_sa$center_x <- coords[,1]
crowns_sa$center_y <- coords[,2]

# VHM metrics
#-----------------------------------------------------

# Extract 90th percentile height
h90 <- terra::extract(
  vhm_sa,
  vect(crowns_sa),
  fun = function(x, ...) {
    quantile(x, probs = 0.90, na.rm = TRUE)
  }
)

# Add to crowns
crowns_sa$height_p90 <- h90[,2]

#-----------------------------------------------------
# Computing the ecological parameters depending on neighborhood
#-----------------------------------------------------

# Find the neighbors
neighbors <- st_is_within_distance(
  centroids,
  centroids,
  dist = 100
)

# Initialize vectors
n_neighbors <- numeric(length(neighbors))
mean_neighbor_dist <- numeric(length(neighbors))
nearest_neighbor_dist <- numeric(length(neighbors))

for(i in seq_along(neighbors)) {
  
  # Remove focal tree itself
  neigh_ids <- neighbors[[i]]
  neigh_ids <- neigh_ids[neigh_ids != i]
  
  # Number of neighbors
  n_neighbors[i] <- length(neigh_ids)
  
  # If neighbors exist
  if(length(neigh_ids) > 0) {
    
    # Distances
    dists <- st_distance(
      centroids[i,],
      centroids[neigh_ids,],
      by_element = FALSE
    )
    
    dists <- as.numeric(dists)
    
    mean_neighbor_dist[i] <- mean(dists)
    nearest_neighbor_dist[i] <- min(dists)
    
  } else {
    
    mean_neighbor_dist[i] <- NA
    nearest_neighbor_dist[i] <- NA
  }
}

# Add metrics back to crown object
crowns_sa$n_neighbors_100m <- n_neighbors
crowns_sa$mean_neighbor_dist_100m <- mean_neighbor_dist
crowns_sa$nearest_neighbor_dist_100m <- nearest_neighbor_dist


#-----------------------------------------------------
# TEMP
# Quick visualization of the outputs
#-----------------------------------------------------

st_write(crowns_sa, "D:/temp/crowns_ecolparam.gpkg")

library(mapview)

mapview(crowns_sa, zcol="n_neighbors_100m")
