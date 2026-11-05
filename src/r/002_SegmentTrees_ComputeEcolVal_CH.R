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
CH_1000 <- st_read(CH_1000_path, layer="grid_1000")

# Load the forest layer
forest_mask <- st_read(swisstlm3d_path, query="SELECT * FROM tlm_bb_bodenbedeckung t where t.OBJEKTART = 'Wald'")

# Load the open water layer
water_mask <- st_read(swisstlm3d_path, query="SELECT * FROM tlm_bb_bodenbedeckung t where t.OBJEKTART = 'Fliessgewaesser' OR t.OBJEKTART = 'Stehende Gewaesser'")

# Load the Gebeaude footprint layer
settlement <- st_read(settlement_path)

# SELECT SWISS1 OR SWISS2 RUN
SWISS_RUN <- "SWISS1"
stopifnot(SWISS_RUN %in% c("SWISS1", "SWISS2"))

if(SWISS_RUN == "SWISS1"){

  # Set the VHM of the corresponding ALS acquisition
  vhm_path <- VHM_S1_path

  # Load the BFF layers
  bff_qual <- st_read(bff_2021_path, layer = "bff_qualitaet_2_flaechen")
  bff_vern <- st_read(bff_2021_path, layer = "bff_vernetzung_flaechen")

  # Load the hedges
  hedges <- st_read(nf_2021_path, query = "select * from nutzungsflaechen where lnf_code in (852,857,858)")

  # Set the correct year for the bewirtschaftungseinheit layer
  bwe_path <- bwe_2021_path
  LN_path <- nf_2021_path

} else {

  # Set the VHM of the corresponding ALS acquisition
  vhm_path <- VHM_S2_path

  # Load the BFF layers
  bff_qual <- st_read(bff_2025_path, layer = "bff_qualitaet_2_flaechen")
  bff_vern <- st_read(bff_2025_path, layer = "bff_vernetzung_flaechen")

  # Load the hedges
  hedges <- st_read(nf_2025_path, query = "select * from nutzungsflaechen where lnf_code in (852,857,858)")

  # Set the correct year for the bewirtschaftungseinheit layer
  bwe_path <- bwe_2025_path
  LN_path <- nf_2025_path

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
}

# Reduce the context layers to their geometry plus the bounding box of each
# feature, so ecological_val_tree can preselect the features near a cell
# without a GEOS operation on the whole Switzerland-wide layer (also makes
# the copies sent to the parallel workers lighter)
forest_mask <- add_bbox_cols(forest_mask)
water_mask  <- add_bbox_cols(water_mask)
settlement  <- add_bbox_cols(settlement)
bff_qual    <- add_bbox_cols(bff_qual)
bff_vern    <- add_bbox_cols(bff_vern)
hedges      <- add_bbox_cols(hedges)

# Write each run to its own folder 
treeseg_data_local_path <- paste0(treeseg_data_local_path, SWISS_RUN, "/")
dir.create(treeseg_data_local_path, recursive = TRUE, showWarnings = FALSE)


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
  LN_sub <- st_read(LN_path, wkt_filter = wkt, layer = "nutzungsflaechen")

  # If no LN parcel skip this iteration
  if (is.null(LN_sub) || nrow(LN_sub)==0) {
    message("No LN polygons in cell ", i)
    return(NULL)
  }

  # The 2021 data keep the original BLW field names - align the ones used in
  # ecological_val_tree with the 2025 names (no-op for the 2025 data):
  #   - LNF_Code      -> lnf_code
  #   - Identifikator -> betriebsnummer (2021 "Betrieb" is a GUID, while
  #     "Identifikator" has the same "KTxxxx/ x/ xx" format as 2025 betriebsnummer)
  LN_sub <- rename(LN_sub, any_of(c(lnf_code = "LNF_Code")))

  # Load the BWE parcels for the buffered perimeter
  BWE_sub <- st_read(bwe_path, layer = "bewirtschaftungseinheit", wkt_filter = wkt) %>%
    rename(any_of(c(betriebsnummer = "Identifikator")))

  # Load the habitat map polygons for the buffered perimeter 
  habitat_sub <- st_read(habitatmap_local_path, layer = "habitatmap", wkt_filter = wkt)["TypoCH_NUM"]
  st_agr(habitat_sub) <- "constant"

  # Making sure layer is valid
  habitat_sub <- tryCatch(
    st_crop(habitat_sub, st_bbox(perim_buf)),
    error = function(e) st_crop(st_make_valid(habitat_sub), st_bbox(perim_buf))
  )
  habitat_sub <- habitat_sub[as.numeric(st_area(habitat_sub)) > 0, ]
  if (any(!st_is_valid(habitat_sub))) {
    habitat_sub <- st_make_valid(habitat_sub)
  }

  # Load the deadwood polygons for the buffered perimeter. 
  deadwood_sub <- st_read(deadwood_path, layer = "deadwood", wkt_filter = wkt)
  deadwood_sub$year <- as.integer(regmatches(deadwood_sub$source_gpkg, regexpr("[0-9]{4}", deadwood_sub$source_gpkg)))
  deadwood_sub <- deadwood_sub["year"]
  if (any(!st_is_valid(deadwood_sub))) {
    deadwood_sub <- st_make_valid(deadwood_sub)
  }

  # Have a buffered version to consider crowns overpassing ln parcels
  LN_sub_buff <- LN_sub %>%
    st_buffer(25) %>%
    st_union()

  # Get the VHM and topography of the buffered cell
  #-------------------------
  vhm_cell <- vhm_cell_prep(e_buf, vhm_path)
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
  crowns_out <- ecological_val_tree(crowns, vhm_cell, dem_cell, forest_mask, settlement, bff_qual, bff_vern, hedges, water_mask, perim_buf, LN_sub, BWE_sub, habitat_sub, deadwood_sub)

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
    vhm_path = vhm_path,
    dem_path = dhm25_path,
    treeseg_data_local_path = treeseg_data_local_path,
    LN_path = LN_path,
    bwe_path = bwe_path,
    habitatmap_local_path = habitatmap_local_path,
    deadwood_path = deadwood_path,
    forest_mask = forest_mask,
    water_mask = water_mask,
    settlement = settlement,
    bff_qual = bff_qual,
    bff_vern = bff_vern,
    hedges = hedges,
    vhm_cell_prep = vhm_cell_prep,
    dem_cell_prep = dem_cell_prep,
    segment_cell = segment_cell,
    ecological_val_tree = ecological_val_tree,
    neighborhood_val = neighborhood_val,
    covered_area_val = covered_area_val,
    coverage_val = coverage_val,
    habitat_val = habitat_val,
    bbox_subset = bbox_subset,
    nearest_dist_val = nearest_dist_val
  )
)
