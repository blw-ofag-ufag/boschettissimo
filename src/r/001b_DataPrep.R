#-----------------------------------------------------
# Single time data preparation
#-----------------------------------------------------
# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")
library(sf)
library(dplyr)

#-----------------------------------------------------
# DHM25 - reproject from LV03 (no CRS tag) to LV95
#-----------------------------------------------------

dir.create(dirname(dhm25_path), recursive = TRUE, showWarnings = FALSE)

cmd <- sprintf(
  'gdalwarp -s_srs EPSG:21781 -t_srs EPSG:2056 -r bilinear -co COMPRESS=LZW -co TILED=YES -overwrite "%s" "%s"',
  dhm25_raw_path,
  dhm25_path
)
system(cmd)

#*******************************************************
# BLW DATA 2025 - PROXY SWISS2
#*******************************************************

#-----------------------------------------------------
# BFF - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Biodiversitätsförderflächen (BFF) as its own
# geopackage, all sharing the same two layers - union them into a single
# Switzerland-wide geopackage.

bff_2025_files <- list.files(bff_2025_raw_path, pattern = "\\.gpkg$", full.names = TRUE)
bff_2025_layers <- st_layers(bff_2025_files[1])$name

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(bff_2025_path)) unlink(bff_2025_path)

for (layer in bff_2025_layers) {

  message("Merging layer ", layer)

  merged <- bff_2025_files %>%
    lapply(st_read, layer = layer, quiet = TRUE) %>%
    bind_rows()

  st_write(
    merged,
    dsn = bff_2025_path,
    layer = layer,
    append = FALSE
  )
}

#-----------------------------------------------------
# BWE - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Bewirtschaftungseinheiten (BWE) as its own
# geopackage - union the "bewirtschaftungseinheit" layer into a single
# Switzerland-wide geopackage.

bwe_2025_files <- list.files(bwe_2025_raw_path, pattern = "\\.gpkg$", full.names = TRUE)

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(bwe_2025_path)) unlink(bwe_2025_path)

message("Merging layer bewirtschaftungseinheit")

merged <- bwe_2025_files %>%
  lapply(st_read, layer = "bewirtschaftungseinheit", quiet = TRUE) %>%
  bind_rows()

st_write(
  merged,
  dsn = bwe_2025_path,
  layer = "bewirtschaftungseinheit",
  append = FALSE
)


#-----------------------------------------------------
# NF - union all cantonal geopackages into one
#-----------------------------------------------------
# Each canton delivers its Nutzungsflaechen (NF) as its own
# geopackage - union the "nutzungsflaechen" layer into a single
# Switzerland-wide geopackage.

nf_2025_files <- list.files(nf_2025_raw_path, pattern = "nutzungsflaechen.*\\.gpkg$", full.names = TRUE)

# Start from a clean file so re-running this section doesn't leave stale data
if (file.exists(nf_2025_path)) unlink(nf_2025_path)

message("Merging layer nutzungsflaechen")

merged <- nf_2025_files %>%
  lapply(st_read, layer = "nutzungsflaechen", quiet = TRUE) %>%
  bind_rows()

st_write(
  merged,
  dsn = nf_2025_path,
  layer = "nutzungsflaechen",
  append = FALSE
)

#*******************************************************
# BLW DATA 2021 - PROXY SWISS1
#*******************************************************
# In 2021 each canton has its own folder (original_gdb/<KT>/) holding one file
# geodatabase per theme. All cantons share the same layers and schemas, so we
# union the relevant polygon layer(s) of each theme into a Switzerland-wide
# geopackage. The source has no canton attribute and the reference year is not
# 2021 everywhere (OW = 2022, VD BFF = 2020), so two columns are added:
#   - Kanton:     canton abbreviation, from the folder name
#   - Bezugsjahr: reference year, from the gdb's "Bezugsjahr" table
# Output layers are named as in the 2025 geopackages.

# layers: named vector, c(<output layer name> = "<2021 source layer name>")
merge_cantonal_gdbs <- function(gdb_name, layers, out_path) {

  kantone <- basename(list.dirs(blw_2021_raw_path, recursive = FALSE))
  gdb_paths <- setNames(file.path(blw_2021_raw_path, kantone, gdb_name), kantone)
  stopifnot(all(dir.exists(gdb_paths)))

  # Start from a clean file so re-running this section doesn't leave stale data
  if (file.exists(out_path)) unlink(out_path)

  for (out_layer in names(layers)) {

    layer <- layers[[out_layer]]
    message("Merging layer ", layer, " -> ", out_layer)

    merged <- lapply(kantone, function(kt) {

      # Non-spatial table; some gdbs list the same year twice
      bezugsjahr <- unique(suppressWarnings(
        st_read(gdb_paths[[kt]], layer = "Bezugsjahr", quiet = TRUE)
      )$Bezugsjahr)
      stopifnot(length(bezugsjahr) == 1)

      # type = 6 forces MULTIPOLYGON: a few features are curved polygons
      # (MULTISURFACE, e.g. UR), which would otherwise give a mixed layer
      st_read(gdb_paths[[kt]], layer = layer, type = 6, quiet = TRUE) %>%
        mutate(Kanton = kt, Bezugsjahr = bezugsjahr, .before = 1)
    }) %>%
      bind_rows()

    st_write(
      merged,
      dsn = out_path,
      layer = out_layer,
      append = FALSE
    )
  }
}

#-----------------------------------------------------
# BFF - union all cantonal geodatabases into one
#-----------------------------------------------------
merge_cantonal_gdbs(
  gdb_name = "Biodiversitaetsfoerderflaechen_Qualitaetsstufe_II_und_Vernetzung.gdb",
  layers = c(
    bff_qualitaet_2_flaechen = "BFF_Qualitaet_II_Flaeche",
    bff_vernetzung_flaechen  = "BFF_Vernetzung_Flaeche"
  ),
  out_path = bff_2021_path
)

#-----------------------------------------------------
# BWE - union all cantonal geodatabases into one
#-----------------------------------------------------
merge_cantonal_gdbs(
  gdb_name = "Bewirtschaftungseinheit.gdb",
  layers = c(bewirtschaftungseinheit = "BE_Bewirtschaftungseinheit"),
  out_path = bwe_2021_path
)

#-----------------------------------------------------
# NF - union all cantonal geodatabases into one
#-----------------------------------------------------
merge_cantonal_gdbs(
  gdb_name = "Nutzungsflaechen.gdb",
  layers = c(nutzungsflaechen = "LNF_Nutzung_Flaeche"),
  out_path = nf_2021_path
)

