#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(terra)
library(sf)
library(dplyr)
library(future.apply)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

# Load the grid for processing 
CH_1000 <- rast(CH_1000_path) %>%
  as.polygons(values=TRUE, dissolve=FALSE) %>%
  st_as_sf()

# Load the forest layer
forest_mask <- st_read(swisstlm3d_path, query="SELECT * FROM tlm_bb_bodenbedeckung t where t.OBJEKTART = 'Wald'")

# Load the Gebeaude footprint layer
settlement <- st_read(settlement_path)


# #-----------------------------------------------------
# # TEMP! - Filter on Ebertswil, Uerzlikon, Rossau
# #-----------------------------------------------------
# 
# # Load the canton
# ZU <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
#               query="select * from TLM_KANTONSGEBIET t where t.NAME = 'Zürich'") %>%
#   st_zm(drop = TRUE, what = "ZM")
# 
# # Set the crs (same, but had the Z mention for ZU)
# st_crs(ZU) <- st_crs(CH_1000)
# 
# # Keep only intersecting polygons
# CH_1000 <- CH_1000[st_intersects(CH_1000, ZU, sparse = FALSE), ]
# 
# # Keep only polygons around Ebertswil, Uerzlikon, Rossau
# CH_1000 <- CH_1000[c(1756:1763, 1786:1793, 1816:1823), ]
# 
# # Adapt output path
# treeseg_data_local_path <- "D:/BOSCHETTISSIMO/PROCESSED_DATA/TREE_SEG_Uerzlikon/"

#-----------------------------------------------------
# TEMP! - Filter on canton TG
#-----------------------------------------------------

# Load the canton
TG <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
              query="select * from TLM_KANTONSGEBIET t where t.NAME = 'Thurgau'") %>%
  st_zm(drop = TRUE, what = "ZM")

# Set the crs (same, but had the Z mention for TG)
st_crs(TG) <- st_crs(CH_1000)

# Keep only intersecting polygons
CH_1000 <- CH_1000[st_intersects(CH_1000, TG, sparse = FALSE), ]

# #-----------------------------------------------------
# # TEMP! - Filter on canton VD
# #-----------------------------------------------------
# 
# # Load the canton
# VD <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb", 
#               query="select * from TLM_KANTONSGEBIET t where t.NAME = 'Vaud'") %>%
#   st_zm(drop = TRUE, what = "ZM") 
# 
# # Set the crs (same, but had the Z mention for TG)
# st_crs(VD) <- st_crs(CH_1000)
# 
# # Keep only intersecting polygons
# CH_1000 <- CH_1000[st_intersects(CH_1000, VD, sparse = FALSE), ]


#-----------------------------------------------------
# VHM preparation - Gaussian smoothing
#-----------------------------------------------------
vhm_cell_prep <- function(arg_e_buf, arg_VHM_S2_path) {

  # Create temp file
  tmpfile <- tempfile("vhm_crop_")

  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(arg_e_buf), ymax(arg_e_buf), xmax(arg_e_buf), ymin(arg_e_buf),
    arg_VHM_S2_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt"))
  
  # Load the raster into memory (needed for lidr processing steps)
  vhm_cell <- toMemory(vhm_cell)
  
  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))
  
  # Smooth vhm
  g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
  vhm_cell <- focal(vhm_cell, w = g05, fun = sum)
  
  return(vhm_cell)

}

#-----------------------------------------------------
# Topography preparation
#-----------------------------------------------------
dem_cell_prep <- function(arg_e_buf, arg_dem_path) {

  # Create temp file
  tmpfile <- tempfile("dem_crop_")

  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(arg_e_buf), ymax(arg_e_buf), xmax(arg_e_buf), ymin(arg_e_buf),
    arg_dem_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Read cell DEM from temp file
  dem_cell <- rast(paste0(tmpfile, ".vrt"))

  # Load the raster into memory
  dem_cell <- toMemory(dem_cell)

  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))

  # Derive slope and aspect from the DEM
  topo_cell <- terrain(dem_cell, v = c("slope", "aspect"), unit = "degrees")

  # Combine altitude, slope and aspect into a single raster stack
  c(altitude = dem_cell, topo_cell)

}

#-----------------------------------------------------
# Segmentation
#-----------------------------------------------------
segment_cell <- function(arg_vhm) {
  
  # Define the function that should be used to find tree tops 
  find_ttops <- function(h) {
    3.5 + 0.7*h
  }
  
  # Identify the tree tops
  ttops <- lidR::locate_trees(arg_vhm, lmf(find_ttops, shape="circular", hmin = 1.5)) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  # If no tree tops detected skip this iteration
  if (is.null(ttops) || nrow(ttops) == 0) {
    return(NULL)
  }
  
  # Get the watershed crowns
  crowns <- ForestTools::mcws(
    treetops = ttops,
    CHM = arg_vhm,
    minHeight = 1.5,
    format = "polygons"
  )
  
  # If no detected crown skip this iteration
  if (is.null(crowns) || nrow(crowns) == 0) {
    return(NULL)
  }
  
  return(crowns)
}

#-----------------------------------------------------
# Ecological value calculation
#-----------------------------------------------------
neighborhood_val <- function(arg_neigh_dist, arg_centroids, arg_crowns){
  
  #-----------------------------------------------------
  # Computing the ecological parameters depending on neighborhood
  #-----------------------------------------------------
  
  # Find the neighbors
  neighbors <- st_is_within_distance(
    arg_centroids,
    arg_centroids,
    dist = arg_neigh_dist
  )
  
  # Initialize vectors
  n_neighbors <- numeric(length(neighbors))
  mean_neighbor_dist <- numeric(length(neighbors))
  nearest_neighbor_dist <- numeric(length(neighbors))
  
  neighbor_h90_mean <- numeric(length(neighbors))
  neighbor_h90_sd <- numeric(length(neighbors))
  neighbor_h90_z <- numeric(length(neighbors))

  prop_large_neighbors <- numeric(length(neighbors))

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
        arg_centroids[i,],
        arg_centroids[neigh_ids,],
        by_element = FALSE
      )
      
      dists <- as.numeric(dists)
      
      mean_neighbor_dist[i] <- mean(dists)
      nearest_neighbor_dist[i] <- min(dists)
      
      # Neighbor heights
      neigh_h90 <- arg_crowns$height_p90[neigh_ids]
      
      neighbor_h90_mean[i] <- mean(neigh_h90, na.rm = TRUE)
      neighbor_h90_sd[i]   <- sd(neigh_h90, na.rm = TRUE)
      neighbor_h90_z[i]   <- (arg_crowns$height_p90[focal_id] - mean(neigh_h90, na.rm = TRUE))/sd(neigh_h90, na.rm = TRUE)

      # Proportion of neighbors with a crown diameter larger than 3m
      neigh_diameter <- arg_crowns$diameter_m[neigh_ids]
      prop_large_neighbors[i] <- mean(neigh_diameter > 3, na.rm = TRUE)

    } else {

      mean_neighbor_dist[i] <- NA
      nearest_neighbor_dist[i] <- NA

      neighbor_h90_mean[i] <- NA
      neighbor_h90_sd[i]   <- NA
      neighbor_h90_z[i]   <- NA

      prop_large_neighbors[i] <- NA
    }
  }

  # Add metrics back to crown object
  neigh_df <- data.frame(
    n_neighbors = n_neighbors,
    mean_neighbor_dist = mean_neighbor_dist,
    nearest_neighbor_dist = nearest_neighbor_dist,
    neighbor_h90_mean = neighbor_h90_mean,
    neighbor_h90_sd   = neighbor_h90_sd,
    neighbor_h90_z = neighbor_h90_z,
    prop_large_neighbors = prop_large_neighbors
  )
  
  return(neigh_df)
}

coverage_val <- function(arg_radius, arg_centroids, arg_polygons, arg_vhm = NULL){

  #-----------------------------------------------------
  # Proportion of a disk around each centroid covered by arg_polygons,
  # and (optionally) the mean VHM value within that same disk
  #-----------------------------------------------------

  # Build a disk of the given radius around each centroid
  disks <- st_buffer(arg_centroids, dist = arg_radius)
  disk_area <- as.numeric(st_area(disks))

  # Cover proportion - 0 everywhere if there is nothing to intersect with
  if (nrow(arg_polygons) == 0) {
    cover <- rep(0, nrow(disks))
  } else {

    # Union once so overlapping polygons aren't double-counted
    polygons_union <- st_union(arg_polygons)

    # Intersect all disks at once and map the resulting area back by disk id
    disks_sf <- st_sf(id = seq_len(nrow(disks)), geometry = st_geometry(disks))
    inter <- st_intersection(disks_sf, polygons_union)

    covered_area <- numeric(nrow(disks))
    covered_area[inter$id] <- as.numeric(st_area(inter))

    cover <- covered_area / disk_area
  }

  out <- data.frame(cover = cover)

  # Mean VHM value within the disk, reusing the same disk geometry
  if (!is.null(arg_vhm)) {
    vhm_mean <- terra::extract(arg_vhm, vect(disks), fun = mean, na.rm = TRUE)
    out$vhm_mean <- vhm_mean[, 2]
  }

  out
}


ecological_val_tree <- function(arg_crowns, arg_vhm, arg_dem, arg_forest, arg_settlement, arg_perim, arg_ln){
  
  # Geometry metrics
  #-----------------------------------------------------
  
  # Area, diameter, perimeter and roundness
  arg_crowns <- arg_crowns %>%
    mutate(
      area_m2 = as.numeric(st_area(.)),
      diameter_m = 2 * sqrt(area_m2 / pi),
      perimeter = st_length(st_boundary(.)),
      roundness = as.numeric((4 * pi * area_m2) / (perimeter^2))
    )
  
  # Centroids
  centroids <- st_centroid(arg_crowns)
  
  coords <- st_coordinates(centroids)
  
  arg_crowns$center_x <- coords[,1]
  arg_crowns$center_y <- coords[,2]
  
  
  # VHM metrics
  #-----------------------------------------------------
  
  # Extract 90th percentile height
  h90 <- terra::extract(
    arg_vhm,
    vect(arg_crowns),
    fun = function(x, ...) {
      quantile(x, probs = 0.90, na.rm = TRUE)
    }
  )
  
  # Add to crowns
  arg_crowns$height_p90 <- h90[,2]
  
  # Compute Height / Crown diameter
  arg_crowns$height_diameter <- arg_crowns$height_p90 / arg_crowns$diameter_m

  # Topography metrics
  #-----------------------------------------------------

  # Extract altitude, slope and aspect at the crown centroid
  topo <- terra::extract(arg_dem, vect(centroids))

  arg_crowns$altitude <- topo$altitude
  arg_crowns$slope    <- topo$slope
  arg_crowns$aspect   <- topo$aspect

  # BLW metrics
  #-----------------------------------------------------

  idx <- st_intersects(centroids, arg_ln)
  
  arg_crowns$lnf_codes <- sapply(
    idx,
    \(i) paste(sort(unique(arg_ln$lnf_code[i])), collapse = ";")
  )
  
  # Neighborhood metrics
  #-----------------------------------------------------
  
  # For a radius of 100m
  df_100m <- neighborhood_val(100, centroids, arg_crowns)
  colnames(df_100m) <- paste0(colnames(df_100m), "_100m")
  arg_crowns <- cbind(arg_crowns, df_100m)
  
  # For a radius making up an area of 1 ha
  df_56m <- neighborhood_val(56, centroids, arg_crowns)
  colnames(df_56m) <- paste0(colnames(df_56m), "_56m")
  arg_crowns <- cbind(arg_crowns, df_56m)
  
  # Forest and settlement distances
  #-----------------------------------------------------
  forest <- st_filter(arg_forest, arg_perim, .predicate = st_intersects) 
  f_nearest_idx <- st_nearest_feature(centroids, forest)
  f_min_dist <- st_distance(centroids, forest[f_nearest_idx, ], by_element = TRUE)
  arg_crowns$dist_to_forest <- f_min_dist
  
  sied <- st_filter(arg_settlement, arg_perim, .predicate = st_intersects)
  s_nearest_idx <- st_nearest_feature(centroids, sied)
  s_min_dist <- st_distance(centroids, sied[s_nearest_idx, ], by_element = TRUE)
  arg_crowns$dist_to_settlement <- s_min_dist

  # Forest and settlement cover, and mean VHM within radius
  #-----------------------------------------------------
  # arg_vhm is only passed to the forest calls since centroids stay the same
  # the vhm mean would be computed duplicatly for nothing otherwise
  cov_forest_56m  <- coverage_val(56,  centroids, forest, arg_vhm)
  cov_forest_100m <- coverage_val(100, centroids, forest, arg_vhm)
  cov_sied_56m    <- coverage_val(56,  centroids, sied)
  cov_sied_100m   <- coverage_val(100, centroids, sied)

  arg_crowns$forest_cover_56m      <- cov_forest_56m$cover
  arg_crowns$forest_cover_100m     <- cov_forest_100m$cover
  arg_crowns$settlement_cover_56m  <- cov_sied_56m$cover
  arg_crowns$settlement_cover_100m <- cov_sied_100m$cover

  arg_crowns$vhm_mean_56m  <- cov_forest_56m$vhm_mean
  arg_crowns$vhm_mean_100m <- cov_forest_100m$vhm_mean

  return(arg_crowns)
}


#-----------------------------------------------------
# Cell processing function
#-----------------------------------------------------
process_cell <- function(i) {

  # Get extent of cell, and a 125m-buffered version of it
  #-------------------------
  # 125m = 100m (largest neighborhood/coverage radius) + 25m (margin for a
  # large crown centered close to the cell border) - so every metric computed
  # for a crown ultimately kept (centroid within the true cell) is based on
  # complete, non-truncated data, regardless of how close it is to the border
  e <- ext(CH_1000[i, ])
  perim_buf <- st_buffer(CH_1000[i, ], dist = 125, joinStyle = "MITRE")
  e_buf <- ext(perim_buf)

  # Load the LN surfaces for the buffered perimeter (wider than the raw
  # cell, so LN parcels just outside the true cell are still available for
  # segmentation/neighborhood context)
  wkt <- perim_buf |>
    st_geometry() |>
    st_as_text()
  LN_sub <- st_read(LN_2025_path, wkt_filter = wkt)

  # If no LN parcel skip this iteration
  if (length(LN_sub)==0) {
    message("No LN polygons in cell ", i)
    return(NULL)
  }

  # Have a buffered version to consider crowns overpassing ln parcels
  LN_sub_buff <- LN_sub %>%
    st_buffer(25) %>%
    st_union()

  # Get the VHM and topography of the buffered cell
  #-------------------------
  vhm_cell <- vhm_cell_prep(e_buf, VHM_S2_path)
  dem_cell <- dem_cell_prep(e_buf, dem_path)

  # Make a separate copy restricted to the LN parcels (+25m) to segment on,
  # so trees are not segmented in forest that isn't needed - vhm_cell itself
  # stays unmasked and buffered, so ecological_val_tree's neighborhood/coverage
  # metrics still reflect the true surrounding vegetation, not just the LN part
  LN_sub_buff_vect <- vect(LN_sub_buff)
  vhm_cell_seg <- vhm_cell %>%
    crop(LN_sub_buff_vect) %>%
    mask(LN_sub_buff_vect)

  # Perform the segmentation over the LN parcels (+25m) only
  #-------------------------
  crowns <- segment_cell(vhm_cell_seg)
  if (is.null(crowns)) {
    message("No crowns detected in cell ", i)
    return(NULL)
  }

  # Calculate the attributes pro tree, using the buffered perimeter and the
  # unmasked VHM so neighborhood/coverage metrics near the cell border and
  # near LN parcel edges aren't truncated
  #-------------------------
  crowns_out <- ecological_val_tree(crowns, vhm_cell, dem_cell, forest_mask, settlement, perim_buf, LN_sub)

  # Only now keep crowns whose centroid is both within an LN parcel and
  # within the true (unbuffered) extent of the processed cell
  centroids <- st_centroid(crowns_out)
  in_LN   <- lengths(st_intersects(centroids, LN_sub)) > 0
  in_cell <- lengths(st_intersects(centroids, CH_1000[i, ])) > 0

  crowns_in <- crowns_out[in_LN & in_cell, ]

  if (nrow(crowns_in) == 0) {
    message("No crowns within LN parcels and cell extent for cell ", i)
    return(NULL)
  }

  # Round the result to save memory space
  crowns_in <- crowns_in %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
  
  # Output filename 
  fname <- paste0(
    treeseg_data_local_path,
    "CH1000_", xmin(e), "_", xmax(e), "_",
    ymin(e), "_", ymax(e), ".gpkg"
  )
  
  # Write results
  st_write(crowns_in,
              dsn = fname,
              layer = "crowns",
              append = FALSE)

  
  return(NULL)
}

#-----------------------------------------------------
# Process over cells
#-----------------------------------------------------

# Set processing parameters - Limit number of threads to avoid explosion with parallel processing
Sys.setenv(GDAL_NUM_THREADS = "2")
Sys.setenv(OMP_NUM_THREADS = "2")

# Set up parallel processing
n_workers <- 5 # (detectCores() --> 20)
plan(multisession, workers = n_workers)

# Process cell by cell in parallel 
future_lapply(
  seq_len(nrow(CH_1000)),
  process_cell,
  future.seed = TRUE,
  future.packages = c("terra", "lidR", "sf", "dplyr", "ForestTools"),
  future.globals = list(
    CH_1000 = CH_1000,
    VHM_S2_path = VHM_S2_path,
    dem_path = dhm25_path,
    treeseg_data_local_path = treeseg_data_local_path,
    LN_2025_path = LN_2025_path,
    forest_mask = forest_mask,
    settlement = settlement,
    vhm_cell_prep = vhm_cell_prep,
    dem_cell_prep = dem_cell_prep,
    segment_cell = segment_cell,
    ecological_val_tree = ecological_val_tree,
    neighborhood_val = neighborhood_val,
    coverage_val = coverage_val
  )
)
