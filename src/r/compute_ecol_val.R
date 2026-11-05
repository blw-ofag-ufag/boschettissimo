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

habitat_val <- function(arg_radius, arg_centroids, arg_habitat, arg_n_top = 5, arg_min_area = 0){

  #-----------------------------------------------------
  # Habitat composition of a disk around each centroid, from the habitat map
  # (wall-to-wall, non-overlapping polygons, classes given by TypoCH_NUM):
  #   - n_habitats: number of distinct habitats covering more than
  #     arg_min_area m2 of the disk
  #   - habitat_topk / habitat_topk_pct: the arg_n_top habitats covering the
  #     largest area, with their share of the disk area (in %) - NA where
  #     fewer habitats are present
  #-----------------------------------------------------

  n <- nrow(arg_centroids)

  # Output skeleton - 0 habitats and NA top habitats by default
  out <- data.frame(n_habitats = integer(n))
  for (k in seq_len(arg_n_top)) {
    out[[paste0("habitat_top", k)]]         <- NA_integer_
    out[[paste0("habitat_top", k, "_pct")]] <- NA_real_
  }

  # Nothing to intersect with
  if (nrow(arg_habitat) == 0) {
    return(out)
  }

  # Build a disk of the given radius around each centroid
  disks <- st_buffer(arg_centroids, dist = arg_radius)
  disk_area <- as.numeric(st_area(disks))
  disks_sf <- st_sf(disk_id = seq_len(n), geometry = st_geometry(disks))

  # Habitat class only - attributes are constant within each geometry (avoids
  # sf's "spatially constant" warning on intersection)
  hab <- arg_habitat["TypoCH_NUM"]
  st_agr(disks_sf) <- "constant"
  st_agr(hab)      <- "constant"

  # Intersect all disks at once, then sum the area per disk and habitat class
  # (several polygons of the same class can fall within one disk)
  inter <- st_intersection(disks_sf, hab)

  hab_area <- inter %>%
    st_drop_geometry() %>%
    mutate(area = as.numeric(st_area(inter))) %>%
    group_by(disk_id, TypoCH_NUM) %>%
    summarise(area = sum(area), .groups = "drop") %>%
    filter(area > arg_min_area)

  if (nrow(hab_area) == 0) {
    return(out)
  }

  # Number of distinct habitats per disk
  n_hab <- count(hab_area, disk_id)
  out$n_habitats[n_hab$disk_id] <- n_hab$n

  # Habitats ranked by area within each disk, with their share of the disk
  top <- hab_area %>%
    mutate(pct = 100 * area / disk_area[disk_id]) %>%
    arrange(disk_id, desc(area)) %>%
    group_by(disk_id) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    filter(rank <= arg_n_top)

  for (k in seq_len(arg_n_top)) {
    top_k <- top[top$rank == k, ]
    out[[paste0("habitat_top", k)]][top_k$disk_id]         <- top_k$TypoCH_NUM
    out[[paste0("habitat_top", k, "_pct")]][top_k$disk_id] <- top_k$pct
  }

  out
}

add_bbox_cols <- function(arg_polygons){

  #-----------------------------------------------------
  # Keep only the geometry of a (Switzerland-wide) context layer, plus the
  # bounding box of each feature as columns - computed once before the cell
  # processing, so features near a cell can then be preselected with plain
  # numeric comparisons (bbox_subset) instead of a GEOS operation that would
  # convert the whole layer for every cell
  #-----------------------------------------------------

  bb <- vapply(st_geometry(arg_polygons), \(g) as.numeric(st_bbox(g)), numeric(4))

  st_sf(
    bb_xmin = bb[1, ], bb_ymin = bb[2, ],
    bb_xmax = bb[3, ], bb_ymax = bb[4, ],
    geometry = st_geometry(arg_polygons)
  )
}

bbox_subset <- function(arg_polygons, arg_bbox, arg_dist = 0){

  #-----------------------------------------------------
  # Features of a layer prepared with add_bbox_cols whose bounding box
  # intersects arg_bbox expanded by arg_dist on every side - a superset of
  # all features within arg_dist of anything inside arg_bbox
  #-----------------------------------------------------

  keep <- arg_polygons$bb_xmax >= arg_bbox[["xmin"]] - arg_dist &
          arg_polygons$bb_xmin <= arg_bbox[["xmax"]] + arg_dist &
          arg_polygons$bb_ymax >= arg_bbox[["ymin"]] - arg_dist &
          arg_polygons$bb_ymin <= arg_bbox[["ymax"]] + arg_dist

  # which() also drops features with an empty geometry (NA bounding box)
  arg_polygons[which(keep), ]
}

nearest_dist_val <- function(arg_centroids, arg_polygons, arg_radius = 1000){

  #-----------------------------------------------------
  # Distance from each centroid to the nearest feature of a layer prepared
  # with add_bbox_cols (0 if the centroid lies within one) - NA if there is
  # no feature within arg_radius
  #-----------------------------------------------------

  # Candidates: features near the box around all centroids
  cand <- bbox_subset(arg_polygons, st_bbox(arg_centroids), arg_radius)
  if (nrow(cand) == 0) {
    return(rep(NA_real_, nrow(arg_centroids)))
  }

  nearest_idx <- st_nearest_feature(arg_centroids, cand)
  d <- as.numeric(st_distance(arg_centroids, cand[nearest_idx, ], by_element = TRUE))

  # The nearest candidate is guaranteed to be the true nearest feature only
  # within arg_radius (any closer feature is then a candidate too) - beyond it
  # a closer feature could lie outside the candidate box, so set those to NA
  ifelse(d <= arg_radius, d, NA_real_)
}


ecological_val_tree <- function(arg_crowns, arg_vhm, arg_dem, arg_forest, arg_settlement, arg_bff_qual, arg_bff_vern, arg_hedges, arg_water, arg_perim, arg_ln, arg_bwe, arg_habitat){

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

  # Inbetween crown distance and number of neighboring crowns it touches/overlaps
  #-----------------------------------------------------

  # Distance of 0 <=> polygons intersect (touch or overlap), self included
  touching <- st_intersects(arg_crowns)
  n_touching_crowns <- lengths(touching) - 1L
  has_touch <- n_touching_crowns > 0

  # Set inbetween crown distance to 0 for crowns that touch other crowns
  nearest_crown_dist <- ifelse(has_touch, 0, NA_real_)

  if (nrow(arg_crowns) > 1) {
    for (i in which(!has_touch)) {
      nearest_j <- st_nearest_feature(arg_crowns[i, ], arg_crowns[-i, ])
      nearest_crown_dist[i] <- as.numeric(
        st_distance(arg_crowns[i, ], arg_crowns[-i, ][nearest_j, ])
      )
    }
  }

  arg_crowns$nearest_crown_dist <- nearest_crown_dist
  arg_crowns$n_touching_crowns  <- n_touching_crowns

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

  # LNF codes
  idx <- st_intersects(centroids, arg_ln)

  arg_crowns$lnf_codes <- sapply(
    idx,
    \(i) paste(sort(unique(arg_ln$lnf_code[i])), collapse = ";")
  )

  # Betriebsnummer
  idx_bwe <- st_intersects(centroids, arg_bwe)

  arg_crowns$bwe_codes <- sapply(
    idx_bwe,
    \(i) paste(sort(unique(arg_bwe$betriebsnummer[i])), collapse = ";")
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

  # Forest, settlement, BFF, hedge and open water distances
  #-----------------------------------------------------
  # Distance to the nearest feature of the whole layer (not only within the
  # buffered cell) - NA if there is none within 1km
  arg_crowns$dist_to_forest     <- nearest_dist_val(centroids, arg_forest)
  arg_crowns$dist_to_settlement <- nearest_dist_val(centroids, arg_settlement)
  arg_crowns$dist_to_bff_qual   <- nearest_dist_val(centroids, arg_bff_qual)
  arg_crowns$dist_to_bff_vern   <- nearest_dist_val(centroids, arg_bff_vern)
  arg_crowns$dist_to_hedges     <- nearest_dist_val(centroids, arg_hedges)
  arg_crowns$dist_to_water      <- nearest_dist_val(centroids, arg_water)

  # Features near the buffered cell, for the cover metrics below (a bounding
  # box preselection is enough, as the cover itself is an exact intersection)
  perim_bbox <- st_bbox(arg_perim)
  forest <- bbox_subset(arg_forest,     perim_bbox)
  sied   <- bbox_subset(arg_settlement, perim_bbox)
  bffq   <- bbox_subset(arg_bff_qual,   perim_bbox)
  bffv   <- bbox_subset(arg_bff_vern,   perim_bbox)

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

  # BFF cover
  #-----------------------------------------------------
  cov_bff_qual_56m  <- coverage_val(56,  centroids, bffq)
  cov_bff_qual_100m <- coverage_val(100, centroids, bffq)
  cov_bff_vern_56m  <- coverage_val(56,  centroids, bffv)
  cov_bff_vern_100m <- coverage_val(100, centroids, bffv)

  arg_crowns$bff_qual_cover_56m  <- cov_bff_qual_56m$cover
  arg_crowns$bff_qual_cover_100m <- cov_bff_qual_100m$cover
  arg_crowns$bff_vern_cover_56m  <- cov_bff_vern_56m$cover
  arg_crowns$bff_vern_cover_100m <- cov_bff_vern_100m$cover

  # Habitat composition within radius
  #-----------------------------------------------------
  # Number of distinct habitats (TypoCH_NUM) and top 5 habitats by area with
  # their share of the disk (in %) - arg_habitat is already restricted to the
  # buffered cell
  hab_56m  <- habitat_val(56,  centroids, arg_habitat)
  hab_100m <- habitat_val(100, centroids, arg_habitat)

  colnames(hab_56m)  <- paste0(colnames(hab_56m),  "_56m")
  colnames(hab_100m) <- paste0(colnames(hab_100m), "_100m")

  arg_crowns <- cbind(arg_crowns, hab_56m, hab_100m)

  return(arg_crowns)
}
