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
out_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/PROCESSED_DATA/ALLEMA/"

# Get all the ALLEMA perimeters considered
allema_gpkg <- list.files(out_data_path, pattern="[ALLEMA_]*[.gpkg]", full.names = T)

# # TEMP - choose one of the segmentation method produced crown
# TODO - when final method is chosen choose appropriate crown
crown_layer <- "vhm50cm_crowns_3x3_max"

#-----------------------------------------------------
# Functions
#-----------------------------------------------------
prep_vhm <- function(vhm_path, e_buf){
  
  # Create temp file
  tmpfile <- tempfile("vhm_crop_")
  
  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(e_buf), ymax(e_buf), xmax(e_buf), ymin(e_buf),
    vhm_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt")) %>% 
    toMemory()
  
  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))
  
  return(vhm_cell)
  
}


for(j in 1:length(allema_gpkg)){
  
  # Get the crown layer
  crowns_sa <- st_read(dsn=allema_gpkg[j], layer=crown_layer)
  
  # Get extent of perimeter
  e <- ext(crowns_sa)
  
  # Buffered extent
  e_buf <- ext(
    xmin(e) - 5, xmax(e) + 5,
    ymin(e) - 5, ymax(e) + 5
  )

  # Get the vhm
  vhm_sa <- prep_vhm(VHM_S1_path, e_buf) 
  
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
    slice_max(area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(focal_max)
  
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
  
  # Compute Height / Crown diameter
  crowns_sa$height_diameter <- crowns_sa$height_p90 / crowns_sa$diameter_m
  
  # BLW metrics
  #-----------------------------------------------------
  
  # Load the LN surfaces for the perimeter
  wkt <- as.polygons(e_buf) |>
    st_as_sf() |>
    st_geometry() |>
    st_as_text()
  ln <- st_read(LN_2025_path, wkt_filter = wkt)
  
  idx <- st_intersects(centroids, ln)
  
  crowns_sa$lnf_codes <- sapply(
    idx,
    \(i) paste(sort(unique(ln$lnf_code[i])), collapse = ";")
  )
  
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
  
  neighbor_h90_mean <- numeric(length(neighbors))
  neighbor_h90_sd <- numeric(length(neighbors))
  neighbor_h90_cv <- numeric(length(neighbors))
  neighbor_h90_z <- numeric(length(neighbors))
  
  for(i in seq_along(neighbors)) {
    
    # Get the neighbors for iteration i
    neigh_ids <- neighbors[[i]]
    
    # Focal tree id
    focal_id <- neigh_ids[neigh_ids == i]
    
    # Remove focal tree itself
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
      
      # Neighbor heights
      neigh_h90 <- crowns_sa$height_p90[neigh_ids]
      
      neighbor_h90_mean[i] <- mean(neigh_h90, na.rm = TRUE)
      neighbor_h90_sd[i]   <- sd(neigh_h90, na.rm = TRUE)
      neighbor_h90_cv[i]   <- sd(neigh_h90, na.rm = TRUE)/mean(neigh_h90, na.rm = TRUE)
      neighbor_h90_z[i]   <- (crowns_sa$height_p90[focal_id] -mean(neigh_h90, na.rm = TRUE))/sd(neigh_h90, na.rm = TRUE)
      
    } else {
      
      mean_neighbor_dist[i] <- NA
      nearest_neighbor_dist[i] <- NA
      
      neighbor_h90_mean[i] <- NA
      neighbor_h90_sd[i]   <- NA
      neighbor_h90_cv[i]   <- NA
      neighbor_h90_z[i]   <- NA
    }
  }
  
  # Add metrics back to crown object
  crowns_sa$n_neighbors_100m <- n_neighbors
  crowns_sa$mean_neighbor_dist_100m <- mean_neighbor_dist
  crowns_sa$nearest_neighbor_dist_100m <- nearest_neighbor_dist
  
  crowns_sa$neighbor_h90_mean_100m <- neighbor_h90_mean
  crowns_sa$neighbor_h90_sd_100m   <- neighbor_h90_sd
  crowns_sa$neighbor_h90_cv_100m   <- neighbor_h90_cv
  crowns_sa$neighbor_h90_z_100m <- neighbor_h90_z
  
  # Exporting the results
  st_write(crowns_sa, allema_gpkg[j], layer=paste0(crown_layer,"_ecolparam"), append=F)
  
}






