#-----------------------------------------------------
# Libraries
#-----------------------------------------------------

library(terra)
library(sf)
library(dplyr)
library(lidR)        # locate_trees / lmf used in segment_cell
library(ForestTools) # mcws used in segment_cell

#-----------------------------------------------------
# Paths & config
#-----------------------------------------------------

# Project paths (CH_1000_path, LN_2025_path, trees_SWISS*_path)
source("../001_Initialization.R")

# Historical VHMs 
VHM_hist1_path <- "//speedy11-12-fs/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_hist_NFI/_data/vhm/vhm_1979_1985.tif"
VHM_hist2_path <- "//speedy11-12-fs/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_hist_NFI/_data/vhm/vhm_1985_1991.tif"
VHM_hist3_path <- "//speedy11-12-fs/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_hist_NFI/_data/vhm/vhm_1990_1998.tif"
VHM_hist4_path <- "//speedy11-12-fs/data_17/_GEOBASISDATEN/_ENVIDAT/VHM_hist_NFI/_data/vhm/vhm_1998_2006.tif"

# All time steps
# - crowns_path = NA -> crowns are segmented here on vhm_path
# - otherwise the crowns are loaded from the CH-wide segmentation
# (years of SWISS1/SWISS2 depend on the canton)
timesteps <- data.frame(
  step        = c("HIST1", "HIST2", "HIST3", "HIST4", "SWISS1", "SWISS2"),
  year        = c(1980, 1987, 1995, 2000, 2017, 2024),
  vhm_path    = c(VHM_hist1_path, VHM_hist2_path, VHM_hist3_path, VHM_hist4_path, NA, NA),
  crowns_path = c(NA, NA, NA, NA, trees_SWISS1_path, trees_SWISS2_path)
)

# LV95 coordinate of a point in the CH_1000 cell to test
cell_xy <- c(2680005, 1230560)
# c(2743458, 1264812)

# Mask the VHM to the LN parcels (+25m) before segmenting, as in 002 - keep
# TRUE so the historical crowns are comparable to the SWISS1/SWISS2 ones
mask_to_LN <- TRUE

# Apply the Gaussian smoothing of vhm_cell_prep to the historical VHMs too.
# At 1m it is a light 3x3 smoothing (sigma = 0.5m, centre weight ~0.62) -
# keep TRUE if SWISS1 (1m) was segmented with it, for comparable crowns
smooth_hist <- TRUE

# [m] Buffer around the cell for segmentation, so crowns on the cell border
# are not cut (only crowns with centroid in the cell are kept afterwards)
seg_buffer <- 25

# Output
out_dir <- "D:/temp/HIST_TREES/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#-----------------------------------------------------
# Functions
#-----------------------------------------------------

# Take vhm_cell_prep() and segment_cell() directly from 002 without running
# the script, so the segmentation here always stays identical to the CH run
seg_script <- parse("../002_SegmentTrees_ComputeEcolVal_CH.R", keep.source = FALSE)
for (ex in seg_script) {
  if (is.call(ex) && identical(ex[[1]], as.name("<-")) && is.name(ex[[2]]) &&
      as.character(ex[[2]]) %in% c("vhm_cell_prep", "segment_cell")) {
    eval(ex)
  }
}
stopifnot(exists("vhm_cell_prep"), exists("segment_cell"))

# Segment one historical time step over the buffered cell
segment_hist_step <- function(arg_vhm_path, arg_perim, arg_perim_buf, arg_LN_mask) {

  # Same VHM preparation (crop + Gaussian smoothing) as in 002, or crop only
  if (smooth_hist) {
    vhm <- vhm_cell_prep(ext(arg_perim_buf), arg_vhm_path)
  } else {
    vhm <- toMemory(crop(rast(arg_vhm_path), ext(arg_perim_buf)))
  }

  # Segment only over the LN parcels (+25m), as in 002
  vhm_seg <- vhm
  if (!is.null(arg_LN_mask)) {
    vhm_seg <- vhm %>%
      crop(arg_LN_mask) %>%
      mask(arg_LN_mask)
  }

  crowns <- segment_cell(vhm_seg)
  if (is.null(crowns)) return(NULL)

  if (st_crs(crowns) != st_crs(arg_perim)) crowns <- st_transform(crowns, st_crs(arg_perim))

  # Same basic geometry / height metrics as in ecological_val_tree (002)
  crowns$area_m2    <- as.numeric(st_area(crowns))
  crowns$diameter_m <- 2 * sqrt(crowns$area_m2 / pi)
  crowns$height_p90 <- terra::extract(
    vhm, vect(crowns),
    fun = function(x, ...) quantile(x, probs = 0.90, na.rm = TRUE)
  )[, 2]

  # Keep only crowns whose centroid is within the true cell
  in_cell <- lengths(st_intersects(st_centroid(st_geometry(crowns)), arg_perim)) > 0
  crowns[in_cell, ]
}

# Load the crowns of an already segmented time step (SWISS1 / SWISS2)
load_seg_step <- function(arg_crowns_path, arg_perim) {

  crowns <- st_read(arg_crowns_path,
                    wkt_filter = st_as_text(st_geometry(arg_perim)),
                    quiet = TRUE)
  if (nrow(crowns) == 0) return(NULL)

  if (st_crs(crowns) != st_crs(arg_perim)) crowns <- st_transform(crowns, st_crs(arg_perim))

  # wkt_filter also returns crowns of neighbouring cells touching the cell
  in_cell <- lengths(st_intersects(st_centroid(st_geometry(crowns)), arg_perim)) > 0
  crowns[in_cell, ]
}

# Common set of attributes for all time steps, so they can be bound together
standardise_crowns <- function(arg_crowns, arg_step, arg_year) {
  st_sf(
    step       = rep(arg_step, nrow(arg_crowns)),
    year       = rep(arg_year, nrow(arg_crowns)),
    treeID     = if ("treeID" %in% names(arg_crowns)) arg_crowns$treeID else seq_len(nrow(arg_crowns)),
    area_m2    = arg_crowns$area_m2,
    diameter_m = arg_crowns$diameter_m,
    height_p90 = arg_crowns$height_p90,
    geometry   = st_geometry(arg_crowns)
  )
}

#-----------------------------------------------------
# Crowns for the 6 time steps
#-----------------------------------------------------

if (any(is.na(timesteps$year))) stop("Set the acquisition year of every time step in 'timesteps'")
timesteps <- timesteps[order(timesteps$year), ]

# Load the grid and pick the cell containing cell_xy
CH_1000 <- rast(CH_1000_path) %>%
  as.polygons(values = TRUE, dissolve = FALSE) %>%
  st_as_sf()

cell_idx <- which(lengths(st_intersects(CH_1000, st_sfc(st_point(cell_xy), crs = st_crs(CH_1000)))) > 0)

perim     <- CH_1000[cell_idx, ]
perim_buf <- st_buffer(perim, dist = seg_buffer, joinStyle = "MITRE")
e         <- ext(perim)
cell_tag  <- paste0("CH1000_", xmin(e), "_", xmax(e), "_", ymin(e), "_", ymax(e))

# LN mask (+25m) for the segmentation
LN_mask <- NULL
if (mask_to_LN) {
  LN_sub <- st_read(LN_2025_path, wkt_filter = st_as_text(st_geometry(perim_buf)), quiet = TRUE)
  if (nrow(LN_sub) == 0) stop("No LN parcel in cell ", cell_idx, " - pick another cell or set mask_to_LN <- FALSE")
  LN_mask <- vect(st_union(st_buffer(LN_sub, 25)))
}

# Segment / load every time step
#-------------------------
crowns_list <- list()
for (k in seq_len(nrow(timesteps))) {

  ts <- timesteps[k, ]
  message("Time step ", ts$step, " (", ts$year, ")")

  if (is.na(ts$crowns_path)) {
    cr <- segment_hist_step(ts$vhm_path, perim, perim_buf, LN_mask)
  } else {
    cr <- load_seg_step(ts$crowns_path, perim)
  }

  if (is.null(cr) || nrow(cr) == 0) {
    message("  no crowns")
    next
  }
  message("  ", nrow(cr), " crowns")

  crowns_list[[ts$step]] <- standardise_crowns(cr, ts$step, ts$year)
}

crowns_all <- do.call(rbind, crowns_list) %>%
  st_make_valid()
rownames(crowns_all) <- NULL

st_write(crowns_all, paste0(out_dir, cell_tag, "_tracking.gpkg"),
         layer = "crowns_6steps", append = FALSE)
