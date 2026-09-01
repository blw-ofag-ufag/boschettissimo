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
seg_run <- "SWISS1" # Actually SWISS2 has no Luftbild interpretation +/-1 year from the ALS data!

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

#-----------------------------------------------------
# Drop quadrats outside the 13 cantons the segmentation was run on
#-----------------------------------------------------

# Same 13 cantons as the "Keep only the 13 cantons..." filter in
# 002_SegmentTrees_ComputeEcolVal_CH.R (trees_path only has crowns there) 
KT_13 <- st_read(
  "//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
  query = "select * from TLM_KANTONSGEBIET"
) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  filter(NAME %in% c(
    "Genève", "Thurgau", "Schwyz", "Zürich", "Fribourg", "Glarus", "Appenzell Ausserrhoden",
    "Vaud", "Zug", "St. Gallen", "Schaffhausen", "Neuchâtel", "Appenzell Innerrhoden"
  )) %>%
  st_transform(st_crs(ALLEMA_Q))

n_Q_before <- nrow(ALLEMA_Q)

ALLEMA_Q <- ALLEMA_Q[lengths(st_intersects(ALLEMA_Q, KT_13)) > 0, ]

# Keep only reference trees belonging to a quadrat that survived the filter
ALLEMA_EB <- ALLEMA_EB %>%
  filter(FK_Quadrat %in% ALLEMA_Q$FK_Quadrat)

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

# The FileGDB driver doesn't use a spatial index for wkt_filter here (same
# issue as the missing attribute index elsewhere), so this scans most of the
# 1.9M-feature national layer - expect ~2 minutes, not instant
LN_sub <- st_read(LN_2025_path, wkt_filter = Q_extent_wkt, quiet = TRUE)

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
EB_seg <- st_read(trees_path, wkt_filter = Q_extent_wkt, fid_column_name = "fid", quiet = TRUE)

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

    detection_rate =
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
    detection_rate,
    omission_rate,
    overseg_rate,
    underseg_rate,
    commission_rate,
    mean_iou,
    mean_centroid_dist
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

#=====================================================
# MARKDOWN REPORT PER QUADRANT
#=====================================================

# One paste-able markdown block per quadrant

fmt_pct <- function(x) sub("\\.", ",", sprintf("%.2f%%", x * 100))
fmt_num <- function(x) sprintf("%.2f", x)

# Per-quadrant assessment map: SWISSIMAGE background with every reference
# tree/crown colored by its assessment status, thin white borders so
# adjacent polygons stay distinguishable over the aerial photo
map_dir <- file.path(out_dir, "quadrat_maps")
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

map_layers <- list(
  list(data = commission_layer,     col = "#c68fdf"), # segmented, no matching ref tree - drawn first (crowns), so ref tree layers stay visible on top
  list(data = ALLEMA_EB_outside_LN, col = "#d5aa44"),
  list(data = omission_trees,       col = "#d7607a"),
  list(data = detected_underseg,    col = "#92c263"),
  list(data = detected_overseg,     col = "#357453"),
  list(data = detected_one_to_one,  col = "#5e8f4a")
)

render_quadrat_map <- function(q) {

  layers_q <- lapply(map_layers, function(l) list(data = l$data %>% filter(FK_Quadrat == q), col = l$col))

  # Extent: the quadrat polygon plus anything from the layers above that
  # spills slightly outside it, buffered and squared to match the WMS request
  q_geom <- st_geometry(ALLEMA_Q %>% filter(FK_Quadrat == q))
  layer_geoms <- lapply(layers_q, function(l) if (nrow(l$data) > 0) st_geometry(l$data) else NULL)
  full_extent <- do.call(c, c(list(q_geom), layer_geoms[!sapply(layer_geoms, is.null)]))

  bbox <- st_bbox(full_extent)
  cx <- mean(c(bbox["xmin"], bbox["xmax"]))
  cy <- mean(c(bbox["ymin"], bbox["ymax"]))
  side <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]) * 1.1
  bbox <- c(xmin = cx - side / 2, ymin = cy - side / 2, xmax = cx + side / 2, ymax = cy + side / 2)

  img_path <- tempfile(fileext = ".jpg")
  wms_url <- paste0(
    "https://wms.geo.admin.ch/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap",
    "&LAYERS=ch.swisstopo.swissimage&STYLES=default&CRS=EPSG:2056",
    "&BBOX=", bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"],
    "&WIDTH=2000&HEIGHT=2000&FORMAT=image/jpeg"
  )
  download.file(wms_url, destfile = img_path, mode = "wb", quiet = TRUE)

  background <- rast(img_path)
  ext(background) <- ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
  crs(background) <- "EPSG:2056"

  out_file <- file.path(map_dir, paste0(q, ".png"))
  png(out_file, width = 2000, height = 2000)
  plotRGB(flip(background, direction = "vertical"))

  for (l in layers_q) {
    if (nrow(l$data) > 0) {
      plot(st_geometry(l$data), col = l$col, border = "white", lwd = 0.5, add = TRUE)
    }
  }

  dev.off()

  out_file
}

invisible(lapply(summary_by_quadrat$FK_Quadrat, render_quadrat_map))

render_quadrat_report <- function(q) {

  ts_q <- tree_status %>% filter(FK_Quadrat == q)
  cc_q <- commission_crowns %>% filter(FK_Quadrat == q)
  us_q <- undersegmentation %>% filter(FK_Quadrat == q)
  bm_q <- best_matches %>% filter(FK_Quadrat == q)
  sm_q <- summary_by_quadrat %>% filter(FK_Quadrat == q)

  n_reference <- nrow(ts_q)
  n_matched <- sum(ts_q$detected)
  n_omission <- sum(ts_q$status == "omission")
  n_one_to_one <- sum(ts_q$status == "one_to_one")
  n_oversegmented <- sum(ts_q$status == "oversegmented")
  n_undersegmented <- sum(ts_q$status == "undersegmented")

  n_segmented <- nrow(cc_q)
  n_commission <- sum(!cc_q$matched)

  # A crown that meaningfully overlaps only one ref tree (n_ref == 1) isn't
  # automatically a clean 1-to-1 match: if that one ref tree is itself
  # oversegmented (matched to several crowns), this crown is one of the
  # several splitting it, not a genuine 1-to-1 pair. Flag those separately so
  # the crown-side "one_to_one" count actually reconciles with the tree-side
  # one instead of silently absorbing oversegmentation's extra crowns
  overseg_tree_ids <- ts_q$OBJECT_ID[ts_q$status == "oversegmented"]
  overseg_crown_ids <- meaningful_overlaps %>%
    filter(FK_Quadrat == q, OBJECT_ID %in% overseg_tree_ids) %>%
    pull(crown_id) %>%
    unique()

  # Bucket every segmented crown by how it relates to the ref tree(s) it
  # meaningfully overlaps, so the crown-side counts add up to n_segmented
  # just like the tree-side counts add up to n_reference
  crown_class <- cc_q %>%
    left_join(us_q %>% select(crown_id, n_ref), by = "crown_id") %>%
    mutate(
      class = case_when(
        !matched ~ "commission",
        !is.na(n_ref) & n_ref > 1 ~ "undersegmentation_crown",
        crown_id %in% overseg_crown_ids ~ "oversegmentation_crown",
        TRUE ~ "one_to_one"
      )
    )

  n_one_to_one_crowns <- sum(crown_class$class == "one_to_one")
  busy_crowns <- crown_class %>% filter(class == "undersegmentation_crown")
  n_busy_crowns <- nrow(busy_crowns)
  n_overseg_crowns <- sum(crown_class$class == "oversegmentation_crown")

  # Mean containment: how much of each reference tree's own area falls
  # inside its matched crown. Reported alongside IoU rather than folded into
  # it, since IoU conflates this with the crown/reference size mismatch -
  # two quadrants can have the same IoU for very different reasons
  mean_containment <- mean(bm_q$perc_ALLEMA, na.rm = TRUE)

  overseg_note <- if (n_overseg_crowns > 0) {
    paste0(
      "  - ", n_overseg_crowns, " are split among the ", n_oversegmented,
      " reference tree", if (n_oversegmented != 1) "s" else "", " that were oversegmented\n"
    )
  } else {
    "  - 0 are split among an oversegmented reference tree\n"
  }

  underseg_note <- if (n_busy_crowns > 0) {
    paste0(
      "  - ", n_busy_crowns, " are containing the ", n_undersegmented,
      " reference trees that were undersegmented (mean of ",
      fmt_num(mean(busy_crowns$n_ref)), " reference trees per segmented crown)\n"
    )
  } else {
    "  - 0 are containing an undersegmented reference tree\n"
  }

  paste0(
    "FK Quadrat ", q, "\n",
    "----\n\n",
    "![FK Quadrat ", q, "](quadrat_maps/", q, ".png)\n\n",
    "In this quadrant:\n",
    "- ", n_reference, " ref trees are located on LN parcels. \n",
    "  - ", n_matched, " are intersecting a segmented crown:\n",
    "    - ", n_one_to_one, " with a 1-to-1 relation \n",
    "    - ", n_oversegmented, " were oversegmented\n",
    "    - ", n_undersegmented, " were undersegmented \n",
    "  - ", n_omission, " are not intersecting any segmented crown\n",
    "- ", n_segmented, " trees were segmented on the LN parcels. \n",
    "  - ", n_commission, " are not intersecting any reference tree\n",
    "  - ", n_one_to_one_crowns, " are intersecting exactly one reference tree\n",
    overseg_note,
    underseg_note, "\n",
    "The reference trees and segmentation crown that do intersect have a mean IoU of ",
    fmt_num(sm_q$mean_iou), ". On average, the segmented crown covers ",
    fmt_num(mean_containment), "% of the matched reference tree's area. ",
    "The centroid of the segmented trees are in mean shifted by ",
    fmt_num(sm_q$mean_centroid_dist), " m from the centroid of the reference tree.\n\n",
    "Translated in rates this gives:\n\n",
    "detection_rate | omission_rate | overseg_rate | underseg_rate | commission_rate | mean_iou | mean_centroid_dist\n",
    "-- | -- | -- | -- | -- | -- | --\n",
    fmt_pct(sm_q$detection_rate), " | ", fmt_pct(sm_q$omission_rate), " | ",
    fmt_pct(sm_q$overseg_rate), " | ", fmt_pct(sm_q$underseg_rate), " | ",
    fmt_pct(sm_q$commission_rate), " | ", fmt_num(sm_q$mean_iou), " | ",
    fmt_num(sm_q$mean_centroid_dist), "\n"
  )
}

quadrat_reports <- vapply(summary_by_quadrat$FK_Quadrat, render_quadrat_report, character(1))

writeLines(
  paste(quadrat_reports, collapse = "\n\n"),
  paste0(out_dir, "quadrat_reports_", seg_run, ".md")
)
