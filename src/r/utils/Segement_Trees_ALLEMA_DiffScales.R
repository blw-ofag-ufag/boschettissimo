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
ALLEMA_Q <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_Q_ref.gpkg")
out_data_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/PROCESSED_DATA/ALLEMA/"

#-----------------------------------------------------
# Segmentation functions
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

get_ttops <- function(vhm_cell, sigma){
  
  # Smooth the VHM
  g03 <- focalMat(vhm_cell, d = sigma, type = "Gauss")
  vhm_sa_gauss03 <- focal(vhm_cell, w = g03, fun = sum)
  
  # Locate the tree tops on this smoothed VHM
  ttops_temp <- locate_trees(vhm_sa_gauss03, algorithm = lmf(ws=3, hmin = 2)) %>%
    st_zm(drop = TRUE, what = "ZM")
  
  # Format object
  ttops_temp <- cbind(rep(sigma,nrow(ttops_temp)),ttops_temp)
  colnames(ttops_temp)[1] <- "sigma"
  colnames(ttops_temp)[2] <- "ID"
  
  return(ttops_temp)
}

build_tracks <- function(ttops_cand, max_xy, max_h)
{
  
  sigmas <- sort(unique(ttops_cand$sigma))
  
  # Initialize trackID column
  ttops_cand$trackID <- NA_integer_
  
  # First sigma starts all tracks (min sigma should produce the most ttops candidates)
  first_idx <- which(ttops_cand$sigma == min(sigmas))
  
  ttops_cand$trackID[first_idx] <- seq_along(first_idx)
  
  next_trackID <- max(ttops_cand$trackID, na.rm = TRUE) + 1
  
  # Loop through consecutive sigma levels
  for(k in 2:length(sigmas))
  {
    
    prev_sigma <- sigmas[k - 1]
    curr_sigma <- sigmas[k]
    
    prev <- ttops_cand %>%
      filter(sigma == prev_sigma)
    
    curr <- ttops_cand %>%
      filter(sigma == curr_sigma)
    
    if(nrow(prev) == 0 || nrow(curr) == 0)
      next
    
    # Distance matrix
    dxy <- st_distance(prev, curr)
    dxy <- units::drop_units(dxy)
    
    # Height difference matrix
    dh <- abs(
      outer(prev$height,
            curr$height,
            "-")
    )
    
    # Candidate matches
    valid <- (dxy <= max_xy) & (dh <= max_h)
    
    used_curr <- rep(FALSE, nrow(curr))
    
    # Assign track IDs from prev -> curr
    for(i in seq_len(nrow(prev)))
    {
      
      candidates <- which(valid[i, ] & !used_curr)
      
      if(length(candidates) == 0)
        next
      
      # choose closest XY match
      j <- candidates[which.min(dxy[i, candidates])]
      
      curr_row <- which(
        ttops_cand$treeID == curr$treeID[j] &
          ttops_cand$sigma  == curr_sigma
      )
      
      ttops_cand$trackID[curr_row] <- prev$trackID[i]
      
      used_curr[j] <- TRUE
    }
    
    # Unmatched trees start new tracks
    curr_rows <- which(ttops_cand$sigma == curr_sigma)
    
    unassigned <- curr_rows[
      is.na(ttops_cand$trackID[curr_rows])
    ]
    
    if(length(unassigned) > 0)
    {
      ttops_cand$trackID[unassigned] <-
        seq(next_trackID,
            length.out = length(unassigned))
      
      next_trackID <-
        next_trackID + length(unassigned)
    }
  }
  
  tracks <- ttops_cand
  
  return(tracks)
}

misc <- function(vhm_cell, ALLEMA_filter, outpackage){
  
  # GET THE TREE TOPS
  #--------------------------------------------
  
  # Initialize the tree top candidates data frame
  ttops_cand <- data.frame()
  
  # Sigma values used for smoothing
  sig <- seq(0.2, 1, 0.1)
  
  # Get the tree tops for the different smoothed VHMs
  for(i in 1:length(sig)){
    ttops <- get_ttops(vhm_cell, sig[i])
    ttops_cand <- rbind(ttops_cand,ttops)
  }
  
  # Set a treeID across all candidates
  ttops_cand$treeID <- seq_len(nrow(ttops_cand))
  
  # Get the actual tree height of the point using the original VHM
  ttops_cand$height <- terra::extract(vhm_cell, ttops_cand)[,2]
  
  # # TD - Doesn't change anything....
  # # FILTER ONLY WHAT IS ON LN FLAECHE
  # #--------------------------------------------
  # tt_ext <- ext(ttops_cand)
  # wkt <- as.polygons(tt_ext) |>
  #   st_as_sf() |>
  #   st_geometry() |>
  #   st_as_text()
  # ln <- st_read(LN_2025_path, wkt_filter = wkt)
  # 
  # ttops_cand <- ttops_cand %>%
  #   st_filter(ln, .predicate = st_intersects)
    
  # GET THE TRACKS
  #--------------------------------------------
  
  # Set the tolerence thresholds
  max_xy <- 2
  max_h <- 2
  
  # Get the tracking of the tree tops
  ttops_cand_tracked <- build_tracks(ttops_cand, max_xy, max_h)
  
  st_write(ttops_cand_tracked, outpackage, layer="ttops_cand_tracked", append = FALSE)
  
  ttops_cand_tracked$x <- st_coordinates(ttops_cand_tracked)[,1]
  ttops_cand_tracked$y <- st_coordinates(ttops_cand_tracked)[,2]
  
  tracks <- ttops_cand_tracked %>%
    st_drop_geometry() %>%
    group_by(trackID) %>%
    summarise(
      # methodology metrics
      persistence = n(),
      min_sigma = min(sigma),
      max_sigma = max(sigma),
      # position metrics
      x_mean = mean(x),
      y_mean = mean(y),
      position_sd = sqrt( mean((x - mean(x))^2 + (y - mean(y))^2) ),
      max_drift = max( sqrt( (x - mean(x))^2 + (y - mean(y))^2 ) ),
      # height metrics
      height_mean = mean(height),
      height_sd   = sd(height),
      height_range = max(height) - min(height)
    ) %>%
    st_as_sf( coords = c("x_mean", "y_mean"), crs = 2056)
  
  
  st_write(tracks, outpackage, layer = "tracks", append = FALSE)
  
  
  # # GET THE STABLE TRACKS
  # #--------------------------------------------

  stable_tracks <- tracks %>%
    filter(persistence > 1)


  st_write(stable_tracks, outpackage, layer= "stable_tracks", append = FALSE)
  
  # ISOLATE TREES THAT HAVE A SINGLE TRACK
  #--------------------------------------------
  radii <- stable_tracks$height_mean / 2
  dmat  <- st_distance(stable_tracks)
  
  stable_tracks$has_neighbor <- sapply(seq_len(nrow(stable_tracks)), function(i) {
    
    d <- as.numeric(dmat[i, ])
    d[i] <- Inf  # exclude self
    
    any(d <= pmax(radii[i], radii[-i]))
  })
  
  eb_idx <- !stable_tracks$has_neighbor 
  stable_tracks_isolated      <- stable_tracks[eb_idx, ]
  stable_tracks_with_neighbor <- stable_tracks[!eb_idx, ]
  
  st_write(stable_tracks_isolated, outpackage, layer= "stable_tracks_isolated", append = FALSE)
  st_write(stable_tracks_with_neighbor, outpackage, layer= "stable_tracks_with_neighbor", append = FALSE)
  
  # CLUSTER TRACKS WHICH HAVE NEIGHBORS
  #--------------------------------------------
  stable_tracks_with_neighbor$x <- st_coordinates(stable_tracks_with_neighbor)[,1]
  stable_tracks_with_neighbor$y <- st_coordinates(stable_tracks_with_neighbor)[,2]
  stable_tracks_with_neighbor_mat <- scale(as.matrix(stable_tracks_with_neighbor[,c("x","y")] %>% st_drop_geometry()))
  track_clusters <- hdbscan(stable_tracks_with_neighbor_mat, minPts = 2)
  
  clustered_tracks <- cbind(stable_tracks_with_neighbor, track_clusters$cluster, track_clusters$membership_prob, track_clusters$outlier_scores)
  st_write(clustered_tracks, outpackage, layer= "clustered_tracks", append = FALSE)
  
  filtered_clustered_tracks <- clustered_tracks %>%
    filter(track_clusters.cluster > 0) %>%
    filter(track_clusters.outlier_scores == 0)
  st_write(filtered_clustered_tracks, outpackage, layer= "filtered_clustered_tracks", append = FALSE)

}


#-----------------------------------------------------
# Cell processing function
#-----------------------------------------------------
process_cell <- function(i) {
  
  library(terra)
  library(sf)
  library(lidR)
  library(dbscan)
  
  # Get extent of cell
  e <- ext(ALLEMA_Q[i, ])
  
  # Buffered extent
  e_buf <- ext(
    xmin(e), xmax(e),
    ymin(e), ymax(e)
  )
  
  # # Write to temp directory to avoid crash during parallelization
  # local_out <- file.path(tempdir(),
  #                        paste0("ALLEMA_", ALLEMA_Q[i, ]$ID_Quadrat, ".gpkg"))
  
  #-----------------------------------------------------
  # VHM 0.5 m
  #-----------------------------------------------------
  
  # Crop the VHM to needed extent, and load it to memory so that can be used by lidR
  vhm_cell <- prep_vhm(VHM_S1_path, e_buf) 
  
  # Filter on which surfaces segmentation should happen
  #-----------------------------------------------------
  
  # Get the forest mask used in ALLEMA to be able to compare the same single trees (and actually also Feldgehoelze etc)
  ALLEMA_filter <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/Wald_Gehoelz_ErhZyk2.gdb", 
                           query = paste0("SELECT * FROM Wald_Gehoelz_ErhZyk2_3D WHERE FK_Quadrat = ", ALLEMA_Q[i, ]$ID_Quadrat, " AND Gehoelztyp IN (50,55,58)"))
  
  # Buffer only 50 and 57
  buf <- ALLEMA_filter |>
    dplyr::filter(Gehoelztyp %in% c(50, 57)) |>
    st_buffer(5)
  
  # Keep others unchanged
  nobuf <- ALLEMA_filter |>
    dplyr::filter(!Gehoelztyp %in% c(50, 57))
  
  # Merge everything
  ALLEMA_filter <- rbind(buf, nobuf) |>
    st_union() |>
    st_transform(crs(vhm_cell)) |>
    vect()
  
  
  # Keep only VHM values that are outside of ALLEMA filter
  vhm_cell <- mask(vhm_cell, ALLEMA_filter, inverse = T)
  
  # Run the ttops & track functions
  misc(vhm_cell, ALLEMA_filter, paste0(out_data_path,"ALLEMA_", ALLEMA_Q[i, ]$ID_Quadrat, ".gpkg"))
  
}

#-----------------------------------------------------
# Process over cells
#-----------------------------------------------------

for(j in 1:length(ALLEMA_Q)){
  process_cell(j)
}
