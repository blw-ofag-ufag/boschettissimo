#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(terra)
library(sf)
library(dplyr)
library(future.apply)

# Sourcing initialization code (paths and such) and the cell preparation /
# segmentation / ecological value functions
source("src/r/001_Initialization.R")
source("src/r/prepare_cell.R")
source("src/r/segment_trees.R")
source("src/r/compute_ecol_val.R")

# Load the grid for processing 
CH_1000 <- rast(CH_1000_path) %>%
  as.polygons(values=TRUE, dissolve=FALSE) %>%
  st_as_sf()

# Load the forest layer
forest_mask <- st_read(swisstlm3d_path, query="SELECT * FROM tlm_bb_bodenbedeckung t where t.OBJEKTART = 'Wald'")

# Load the Gebeaude footprint layer
settlement <- st_read(settlement_path)

# Load the BFF layers
bff_qual <- st_read(bff_path, layer = "bff_qualitaet_2_flaechen")
bff_vern <- st_read(bff_path, layer = "bff_vernetzung_flaechen")

#-----------------------------------------------------
# TEMP! - Filter on cantons available with S2
#-----------------------------------------------------

# Load the canton
KT <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
              query="select * from TLM_KANTONSGEBIET") %>%
  st_zm(drop = TRUE, what = "ZM")

# Keep only the 13 cantons that have the newest lidar aquisition (stand 30.07.2026)
KT <- KT[which(KT$NAME %in% c("Genève", "Thurgau", "Schwyz", "Zürich", "Fribourg", "Glarus", "Appenzell Ausserrhoden",
  "Vaud", "Zug", "St. Gallen", "Schaffhausen", "Neuchâtel", "Appenzell Innerrhoden" 
)),]

# Set the crs (same, but had the Z mention for TG)
st_crs(KT) <- st_crs(CH_1000)

# Keep only intersecting polygons
CH_1000 <- CH_1000[lengths(st_intersects(CH_1000, KT)) > 0, ]


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

# #-----------------------------------------------------
# # TEMP! - Filter on canton TG
# #-----------------------------------------------------

# # Load the canton
# TG <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
#               query="select * from TLM_KANTONSGEBIET t where t.NAME = 'Thurgau'") %>%
#   st_zm(drop = TRUE, what = "ZM")

# # Set the crs (same, but had the Z mention for TG)
# st_crs(TG) <- st_crs(CH_1000)

# # Keep only intersecting polygons
# CH_1000 <- CH_1000[st_intersects(CH_1000, TG, sparse = FALSE), ]

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
# Cell processing function
#-----------------------------------------------------
process_cell <- function(i) {

  # Wrap the whole cell so one cell's error doesn't cancel the whole
  # future_lapply run - log it instead and move on, to be rerun separately
  tryCatch({

  # Get extent of cell, and a 125m-buffered version of it
  #-------------------------
  # 125m = 100m (largest neighborhood/coverage radius) + 25m (margin for a
  # large crown centered close to the cell border) - so every metric computed
  # for a crown ultimately kept (centroid within the true cell) is based on
  # complete, non-truncated data, regardless of how close it is to the border
  e <- ext(CH_1000[i, ])

  # Skip cells that were already processed in a previous run (e.g. after a
  # crash/hang), so a restart doesn't redo work that's already on disk
  fname <- paste0(
    treeseg_data_local_path,
    "CH1000_", xmin(e), "_", xmax(e), "_",
    ymin(e), "_", ymax(e), ".gpkg"
  )
  if (file.exists(fname)) {
    message("Cell ", i, " already processed, skipping")
    return(NULL)
  }

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
  if (is.null(LN_sub) || nrow(LN_sub)==0) {
    message("No LN polygons in cell ", i)
    return(NULL)
  }

  # Load the BWE parcels for the buffered perimeter, used to tag each crown
  # with the betriebsnummer(s) it falls within
  BWE_sub <- st_read(bwe_path, layer = "bewirtschaftungseinheit", wkt_filter = wkt)

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
  crowns_out <- ecological_val_tree(crowns, vhm_cell, dem_cell, forest_mask, settlement, bff_qual, bff_vern, perim_buf, LN_sub, BWE_sub)

  # Only now keep crowns whose centroid is both within an LN parcel and
  # within the true (unbuffered) extent of the processed cell
  centroids <- st_centroid(crowns_out)
  in_LN   <- lengths(st_intersects(centroids, LN_sub)) > 0
  in_cell <- lengths(st_intersects(centroids, CH_1000[i, ])) > 0

  crowns_in <- crowns_out[in_cell, ]
  crowns_in$in_LN <- in_LN[in_cell]

  if (nrow(crowns_in) == 0) {
    message("No crowns within LN parcels and cell extent for cell ", i)
    return(NULL)
  }

  # Round the result to save memory space
  crowns_in <- crowns_in %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
  
  # Write results
  st_write(crowns_in,
              dsn = fname,
              layer = "crowns",
              append = FALSE)

  return(NULL)

  }, error = function(e) {
    # Log the failure instead of letting it cancel the whole future_lapply
    # run, so failed cells can be identified and rerun separately afterwards
    log_line <- sprintf(
      "[%s] Cell %d failed: %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      i,
      gsub("[\r\n]+", " ", conditionMessage(e))
    )
    cat(log_line, "\n", sep = "",
        file = paste0(treeseg_data_local_path, "failed_cells.log"),
        append = TRUE)
    message(log_line)
    return(NULL)
  })
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
    bwe_path = bwe_path,
    forest_mask = forest_mask,
    settlement = settlement,
    bff_qual = bff_qual,
    bff_vern = bff_vern,
    vhm_cell_prep = vhm_cell_prep,
    dem_cell_prep = dem_cell_prep,
    segment_cell = segment_cell,
    ecological_val_tree = ecological_val_tree,
    neighborhood_val = neighborhood_val,
    coverage_val = coverage_val
  )
)
