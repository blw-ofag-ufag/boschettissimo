#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)

# Sourcing initialization code (paths and such) 
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Logging setup
#-----------------------------------------------------

# Start logging
sink(seg_tlm_EB_log_file, split = TRUE)

cat("====================================================\n")
cat("Tree segmentation vs TLM EB comparison\n")
cat("Run date:", as.character(Sys.time()), "\n")
cat("====================================================\n\n")

#-----------------------------------------------------
# TEMP! - Code done for test area TG
#-----------------------------------------------------

# List km2 segmented tiles
tree_seg_files <- list.files(treeseg_data_local_path)

# Set seed to get reproducible results
set.seed(1)

# Number of tiles to check
n_tiles <- 20

# Get the random tile index
files_indx <- sample(seq_along(tree_seg_files), size = n_tiles, replace = FALSE)

# Do validation over 10 random tiles 
# (TODO: consider only tiles with at least 50% openland landcover (?))
# (TODO: consider only tiles fully within TG)
for(i in 1:n_tiles){
  
  # Fetch the randomly selected tile
  selected_tile <- tree_seg_files[files_indx[i]]
  
  cat("\n====================================================\n")
  cat("Processing tile:", selected_tile, "\n")
  cat("Tile", i, "of", n_tiles, "\n")
  cat("====================================================\n")
  
  # Load the segmented tree tile
  seg_EB <- st_read(paste0(treeseg_data_local_path, selected_tile), layer = "crowns_large", quiet = TRUE) 

  if(nrow(seg_EB) > 0) {
    
    # Get the extent
    seg_EB_ext <- st_bbox(seg_EB)|>
      st_as_sfc() |>
      st_as_text()
    
    # Get the TLM unfiltered EB dataset
    TLM_EB <- st_read(combined_tlm_eb_path, layer="LokalMax3D_LIDAR_5m", wkt_filter = seg_EB_ext, quiet = TRUE) %>%
      st_zm(drop = TRUE, what = "ZM") 
    
    # CONSIDER FROM THE TLM_EB POINT PERSPECTIVE
    #-------------------------------------------
    
    # For every point indicates which polygon is intersected
    pt_poly <- st_intersects(TLM_EB, seg_EB)
    
    # Specify for each point how many polygons are intersected
    TLM_EB$n_poly_hits <- lengths(pt_poly)
    
    # Get a summary
    point_summary <- TLM_EB %>%
      st_drop_geometry() %>%
      mutate(class = case_when(
        n_poly_hits == 0 ~ "no_hit",
        n_poly_hits == 1 ~ "single_hit",
        n_poly_hits > 1 ~ "multiple_hits"
      )) %>%
      count(class)
    
    # CONSIDER FROM THE SEG_EB POLYGON PERSPECTIVE
    #-------------------------------------------
    
    # For every polygon indicates which point is intersected
    poly_pt <- st_intersects(seg_EB, TLM_EB)
    
    # Specify for each polygon how many points are intersected
    seg_EB$n_point_hits <- lengths(poly_pt)
    
    # Get a summary
    polygon_summary <- seg_EB %>%
      st_drop_geometry() %>%
      mutate(class = case_when(
        n_point_hits == 0 ~ "no_point",
        n_point_hits == 1 ~ "single_point",
        n_point_hits > 1 ~ "multiple_points"
      )) %>%
      count(class)
    
    # Log results
    cat("\n--- Point summary ---\n")
    print(point_summary)
    
    cat("\n--- Polygon summary ---\n")
    print(polygon_summary)
    
    cat("\n----------------------------------------------------------------------------------\n")
  } else {
    
    cat("\n--- Either no segmented trees or no TLM EB points ---\n")
    
    cat("\n----------------------------------------------------------------------------------\n")
  }
  
  
}
  
# Stop logging
sink()

