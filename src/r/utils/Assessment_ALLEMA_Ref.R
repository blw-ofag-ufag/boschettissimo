#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

#-----------------------------------------------------
# Libraries
#-----------------------------------------------------

library(terra)
library(sf)
library(dplyr)
library(tidyr)

#-----------------------------------------------------
# Paths & config
#-----------------------------------------------------

source("src/r/001_Initialization.R")

# Hardcode which tree segmentation run to assess: "SWISS1" or "SWISS2"
seg_run <- "SWISS1"

trees_path <- switch(
  seg_run,
  SWISS1 = trees_SWISS1_path,
  SWISS2 = trees_SWISS2_path,
  stop("seg_run must be 'SWISS1' or 'SWISS2'")
)

out_dir <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ASSESSMENT_DATA/ALLEMA/"

#-----------------------------------------------------
# Load reference data
#-----------------------------------------------------

ALLEMA_EB <- st_read(
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_EB_ref.gpkg"
)

# Drop the vertical (LHN95 height) component of the CRS - ALLEMA_EB comes in
# as the compound CRS "LV95 + LHN95 height", which never compares equal to
# the plain 2D LV95 CRS used by LN_sub/EB_seg/ALLEMA_Q, even though the
# horizontal coordinates are identical. st_intersects() etc. require an
# exact CRS match, so normalize to plain LV95 (EPSG:2056) here.
ALLEMA_EB <- st_transform(ALLEMA_EB, 2056)

# ALLEMA_schafboden <- ALLEMA_EB[which(ALLEMA_EB$FK_Quadrat == 689214),]

# recompute area to have a "unit" object
ALLEMA_EB <- ALLEMA_EB %>%
  mutate(
    area_allema = st_area(geom),
  )

#-----------------------------------------------------
# Load quadrat boundaries
#-----------------------------------------------------

# Used to scope the LN/segmentation reads to the relevant area, and to
# define the "candidate crown" universe for the commission metric below

ALLEMA_Q <- st_read(
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_Q_ref.gpkg"
) %>%
  filter(ID_Quadrat %in% unique(ALLEMA_EB$FK_Quadrat)) %>%
  rename(FK_Quadrat = ID_Quadrat)

Q_extent_wkt <- ALLEMA_Q %>%
  st_geometry() %>%
  st_union() %>%
  st_as_text()

#-----------------------------------------------------
# Keep only reference trees within LN parcels
#-----------------------------------------------------

# The segmentation pipeline (002_SegmentTrees_ComputeEcolVal_CH.R) only ever
# ran over LN parcels, so reference trees outside LN parcels could never be
# detected and would unfairly count as missed. The SWISS1/SWISS2 crowns are
# already LN-restricted by construction and don't need re-filtering.

LN_sub <- st_read(LN_2025_path, wkt_filter = Q_extent_wkt)

in_LN <- lengths(st_intersects(st_centroid(ALLEMA_EB), LN_sub)) > 0

# Kept aside (not used in the assessment) purely to show the full picture in
# the "outside_LN" output layer
ALLEMA_EB_outside_LN <- ALLEMA_EB[!in_LN, ]

ALLEMA_EB <- ALLEMA_EB[in_LN, ]

#-----------------------------------------------------
# Load the segmented trees for the chosen run
#-----------------------------------------------------

# Load the segmented trees. fid_column_name = "fid" must be requested
# explicitly - st_read() does not expose GPKG's primary key as a column by
# default, so the first column here is actually "treeID", which is only
# unique within the cell it was originally segmented in and repeats
# constantly across this merged national file. "fid" is the value that's
# actually guaranteed globally unique.
EB_seg <- st_read(trees_path, wkt_filter = Q_extent_wkt, fid_column_name = "fid")

# Standardize crown ID column
EB_seg <- EB_seg %>%
  mutate(crown_id = as.integer(fid)) %>%
  select(-fid)

EB_seg <- EB_seg %>%
  st_transform(st_crs(ALLEMA_EB)) %>%
  mutate(
    area_crown = st_area(geom)
  )

#-----------------------------------------------------
# Centroid coordinates (for the centroid offset-distance metric)
#-----------------------------------------------------

ALLEMA_EB <- ALLEMA_EB %>%
  mutate(
    X_ref = st_coordinates(st_centroid(geom))[, 1],
    Y_ref = st_coordinates(st_centroid(geom))[, 2]
  )

EB_seg <- EB_seg %>%
  mutate(
    X_seg = st_coordinates(st_centroid(geom))[, 1],
    Y_seg = st_coordinates(st_centroid(geom))[, 2]
  )

#-----------------------------------------------------
# Intersections
#-----------------------------------------------------

intersections <- st_intersection(

  ALLEMA_EB %>%
    select(
      FK_Quadrat,
      OBJECT_ID,
      Gehoelztyp,
      roundness,
      area_allema,
      X_ref,
      Y_ref
    ),

  EB_seg %>%
    select(
      crown_id,
      area_crown,
      X_seg,
      Y_seg
    )
)

#-----------------------------------------------------
# Metrics
#-----------------------------------------------------

intersections <- intersections %>%
  mutate(

    area_intersection =
      st_area(geom),

    perc_ALLEMA =
      as.numeric(
        area_intersection / area_allema
      ) * 100,

    perc_crown =
      as.numeric(
        area_intersection / area_crown
      ) * 100,

    # IoU
    iou =
      as.numeric(
        area_intersection /
          (area_allema +
             area_crown -
             area_intersection)
      ),

    # Positional accuracy: distance between reference and crown centroids
    centroid_dist =
      sqrt(
        (X_ref - X_seg)^2 +
          (Y_ref - Y_seg)^2
      )
  )

#-----------------------------------------------------
# Final table
#-----------------------------------------------------

results_all <- intersections %>%
  st_drop_geometry() %>%
  select(
    FK_Quadrat,
    OBJECT_ID,
    crown_id,
    Gehoelztyp,
    roundness,
    perc_ALLEMA,
    perc_crown,
    iou,
    centroid_dist
  )

#=====================================================
# ANALYSIS
#=====================================================

#-----------------------------------------------------
# BEST MATCH PER REFERENCE TREE
#-----------------------------------------------------

# Keep best overlapping crown for each reference tree

best_matches <- results_all %>%
  group_by(
    FK_Quadrat,
    OBJECT_ID
  ) %>%
  slice_max(
    iou,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

#-----------------------------------------------------
# DETECTION TABLE
#-----------------------------------------------------

# Start from all reference trees (not just the ones that intersected a
# crown), so trees with zero overlap are kept as undetected

# No IoU threshold - a reference tree counts as detected as soon as any
# crown overlaps it at all. A hard IoU cutoff unfairly penalizes reference
# polygons that were digitized smaller than the true crown (well-positioned
# but low IoU purely from the size mismatch).

detection_table <- ALLEMA_EB %>%
  st_drop_geometry() %>%
  select(
    FK_Quadrat,
    OBJECT_ID
  ) %>%
  left_join(
    best_matches,
    by = c(
      "FK_Quadrat",
      "OBJECT_ID"
    )
  ) %>%
  mutate(
    detected =
      !is.na(iou)
  )

#=====================================================
# MEANINGFUL OVERLAPS (for over/under-segmentation only)
#=====================================================

# A crown merely touching ~2% of a reference tree's area (a nearby,
# unrelated crown) shouldn't count as splitting/merging that tree. Filter on
# perc_ALLEMA (% of the REFERENCE tree's own area covered) rather than IoU:
# unlike IoU, it isn't penalized when a reference polygon was digitized
# smaller than the true crown but still sits fully inside it (perc_ALLEMA
# stays close to 100% in that case, since it's normalized by the reference
# tree's own area, not the crown's). Detection (best_matches/detected) is
# unaffected - it still counts any overlap at all.

overlap_threshold <- 10 # percent

meaningful_overlaps <- results_all %>%
  filter(perc_ALLEMA >= overlap_threshold)

#=====================================================
# OVERSEGMENTATION
#=====================================================

# One reference tree meaningfully intersects many crowns

oversegmentation <- meaningful_overlaps %>%
  group_by(
    FK_Quadrat,
    OBJECT_ID
  ) %>%
  summarise(
    n_crowns =
      n_distinct(crown_id),
    .groups = "drop"
  )

#=====================================================
# UNDERSEGMENTATION
#=====================================================

# One segmented crown meaningfully intersects many refs

undersegmentation <- meaningful_overlaps %>%
  group_by(
    FK_Quadrat,
    crown_id
  ) %>%
  summarise(
    n_ref =
      n_distinct(OBJECT_ID),
    .groups = "drop"
  )

#=====================================================
# COMMISSION / FALSE POSITIVES
#=====================================================

# Segmented crowns within a quadrat that don't correspond to any reference
# tree at all - invisible in the metrics above, which are all reference-tree-
# centric

# Assign each crown to whichever quadrat it overlaps most (largest = TRUE)
# rather than requiring its centroid to fall inside one - a crown can
# genuinely overlap a quadrat while its centroid sits just outside it
EB_seg_q <- EB_seg %>%
  st_join(
    ALLEMA_Q %>% select(FK_Quadrat),
    join = st_intersects,
    largest = TRUE
  )

EB_seg$FK_Quadrat <- EB_seg_q$FK_Quadrat

EB_seg$matched <- EB_seg$crown_id %in% results_all$crown_id

# Non-spatial version for the per-quadrat counts below
commission_crowns <- EB_seg %>%
  st_drop_geometry() %>%
  filter(!is.na(FK_Quadrat))

# Spatial version (kept for the "commission" output layer)
commission_layer <- EB_seg %>%
  filter(!is.na(FK_Quadrat), !matched)

#=====================================================
# PER-TREE STATUS
#=====================================================

tree_status <- detection_table %>%

  left_join(
    oversegmentation,
    by = c(
      "FK_Quadrat",
      "OBJECT_ID"
    )
  ) %>%

  left_join(
    undersegmentation,
    by = c(
      "FK_Quadrat",
      "crown_id"
    )
  ) %>%

  mutate(

    status = case_when(

      !detected ~ "omission",

      n_crowns > 1 ~ "oversegmented",

      n_ref > 1 ~ "undersegmented",

      TRUE ~ "one_to_one"
    )

  )

#=====================================================
# SUMMARY PER QUADRANT
#=====================================================

# Spatial diagnostic (not a per-method rollup, since there's a single
# segmentation run here) - lets us see whether detection/over/under-
# segmentation quality varies by quadrat

tree_summary_by_quadrat <- tree_status %>%
  group_by(FK_Quadrat) %>%
  summarise(

    n_reference =
      n(),

    n_matched =
      sum(detected),

    n_omission =
      sum(status == "omission"),

    matched_rate =
      mean(detected),

    omission_rate =
      mean(status == "omission"),

    mean_iou =
      mean(iou, na.rm = TRUE),

    mean_centroid_dist =
      mean(centroid_dist, na.rm = TRUE),

    overseg_rate =
      mean(status == "oversegmented"),

    underseg_rate =
      mean(status == "undersegmented"),

    .groups = "drop"
  )

crown_summary_by_quadrat <- commission_crowns %>%
  group_by(FK_Quadrat) %>%
  summarise(

    n_segmented =
      n(),

    n_commission =
      sum(!matched),

    commission_rate =
      mean(!matched),

    .groups = "drop"
  )

summary_by_quadrat <- tree_summary_by_quadrat %>%
  left_join(
    crown_summary_by_quadrat,
    by = "FK_Quadrat"
  ) %>%
  select(
    FK_Quadrat,
    n_reference,
    n_segmented,
    n_matched,
    n_commission,
    n_omission,
    matched_rate,
    commission_rate,
    omission_rate,
    mean_iou,
    mean_centroid_dist,
    overseg_rate,
    underseg_rate
  )

#=====================================================
# EXPORT
#=====================================================

write.csv(
  summary_by_quadrat,
  paste0(out_dir, "summary_by_quadrat_", seg_run, ".csv"),
  row.names = FALSE
)

#=====================================================
# Linking it back to the ref trees / segmented crowns
#=====================================================

ALLEMA_assessment <- ALLEMA_EB %>%
  left_join(
    tree_status,
    by = c(
      "FK_Quadrat",
      "OBJECT_ID"
    )
  )

# Reference trees successfully matched to a crown, split by status so each
# case can be inspected on its own
detected_one_to_one <- ALLEMA_assessment %>%
  filter(status == "one_to_one")

detected_overseg <- ALLEMA_assessment %>%
  filter(status == "oversegmented")

detected_underseg <- ALLEMA_assessment %>%
  filter(status == "undersegmented")

# Reference trees with no adequate matching crown
omission_trees <- ALLEMA_assessment %>%
  filter(status == "omission")

# Segmented crowns with no matching reference tree at all (commission_layer,
# defined above)

gpkg_path <- paste0(out_dir, "ALLEMA_assessment_", seg_run, ".gpkg")

# Start from a clean file so re-runs don't accumulate stale layers
if (file.exists(gpkg_path)) {
  file.remove(gpkg_path)
}

st_write(detected_one_to_one, gpkg_path, layer = "detected_one_to_one", append = FALSE)
st_write(detected_overseg, gpkg_path, layer = "detected_overseg", append = FALSE)
st_write(detected_underseg, gpkg_path, layer = "detected_underseg", append = FALSE)
st_write(omission_trees, gpkg_path, layer = "omission", append = FALSE)
st_write(commission_layer, gpkg_path, layer = "commission", append = FALSE)
st_write(ALLEMA_EB_outside_LN, gpkg_path, layer = "outside_LN", append = FALSE)
