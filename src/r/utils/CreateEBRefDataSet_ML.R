#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)
library(purrr)
library(rstac)        # SWISSIMAGE tiles from the STAC of data.geo.admin.ch
library(httr)         # flight dates and source resolution from the api3.geo.admin.ch identify service
library(jsonlite)
library(future.apply) # parallel download of the SWISSIMAGE tiles

# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Config
#-----------------------------------------------------

swissimage_selection   <- "closest_summer" # "closest": SWISSIMAGE year closest to LubiJahr_max
                                           # "closest_summer": closest SWISSIMAGE flown in summer_months,
                                           #   falls back to "closest" if there is none
summer_months          <- 5:9              # months considered as summer (leaf-on)
summer_max_extra_years <- 3                # a summer tile can be at most this many years further from
                                           #   LubiJahr_max than the closest tile (one flight cycle)
swissimage_gsd         <- 0.1              # [m] resolution of the tiles to download (0.1 or 2)

ALLEMA_ML_path <- paste0(orig_data_path, "ALLEMA_ML/")

#-----------------------------------------------------
# Prepping the ALS dataset
#-----------------------------------------------------

# Import the ALS data
ALS <- st_read("//speedy11-12-fs/data_17/_LIDAR/ALS_CH/ALS_CH_db/mv_als_ch_ndsm_20260511.gpkg")

# Filter to keep only nation-wide ALS
ALS <- ALS %>% filter(source %in% c("SWISS","SWISS2"))

ALS <- ALS %>%
  select(source, year, x_min, y_min) %>%
  st_drop_geometry()

# # Check that all coordinates round up to 10
# which(ALS$x_min != round(ALS$x_min, -1)) # yes
# which(ALS$y_min != round(ALS$y_min, -1)) # yes

#-----------------------------------------------------
# Prepping the ALLEMA dataset
#* Ex FK-Quadrant 581166 :
#*   OBJECTID 152189 has Erh_Jahr = 2024 and LubiJahr = 2017
#*   OBJECTID 152330 has Erh_Jahr = 2024 and LubiJahr = 2020
#*   --> If an object stayed the same in 2020 as in 2017 it will keep LubiJahr 2017 even though it is visible in 2020
#-----------------------------------------------------

# Import the ALLEMA data
ALLEMA_orig <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/Wald_Gehoelz_ErhZyk2.gdb",
               layer = "Wald_Gehoelz_ErhZyk2_3D")

# Add an ID
ALLEMA_orig$OBJECT_ID <- 1:nrow(ALLEMA_orig)

# # Identify geometries that were not attributed to an ALLEMA Quadrat
# nrow(unique(ALLEMA[which(is.na(ALLEMA$FK_Quadrat)),])) # 568

# Filter out geometries that were not assigned to an ALLEMA Quadrant
ALLEMA <- ALLEMA_orig %>%
  filter(!is.na(FK_Quadrat))

# Apply the latest LubiJahr to the whole Quadrant
ALLEMA <- ALLEMA %>%
  group_by(FK_Quadrat) %>%
  mutate(LubiJahr_max = max(LubiJahr)) %>%
  ungroup()

# Filter to keep only classes 37 (Obstanlagen), 38 (Hochstammobst) and 59 (Einzelbaum, Baumgruppe)
ALLEMA <- ALLEMA %>%
  filter(Gehoelztyp %in% c(37,38,59)) %>%
  select(FK_Quadrat, OBJECT_ID, Gehoelztyp, LubiJahr_max) %>%
  st_drop_geometry()

# Import the perimeter of the ALLEMA Quadrants
ALLEMA_Q_orig <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/ALLEMA_Quadranten.shp")

# Add the xmin ymin info to be able to match with the ALS quadrant
ALLEMA_Q <- ALLEMA_Q_orig %>%
  mutate(bbox = map(geometry, st_bbox)) %>%
  mutate(
    x_min = round(as.integer(map_dbl(bbox, "xmin")),-1),
    y_min = round(as.integer(map_dbl(bbox, "ymin")),-1)
  ) %>%
  select(-bbox) %>%
  select(ID_Quadrat, x_min, y_min) %>%
  st_drop_geometry()

# Join the ALLEMA_Q xmin, ymin info to the main ALLEMA table
ALLEMA <- left_join(ALLEMA, ALLEMA_Q, by=join_by(FK_Quadrat == ID_Quadrat))

# # Identify ALLEMA Quadrats that are not comprised in the perimeter dataset of the ALLEMA Quadrants
# unique(ALLEMA[which(is.na(ALLEMA$x_min)),"FK_Quadrat"]) # 9

ALLEMA <- ALLEMA %>%
  filter(!is.na(x_min)) # Filter out the geometries belonging to the 9 missing ALLEMA_Q perimeters

#-----------------------------------------------------
# Matching the closest ALS year with ALLEMA LubiJahr_max
#-----------------------------------------------------

# Join the ALLEMA and ALS datasets
ALLEMA_ALS_ref <- left_join(ALLEMA, ALS, by=c("x_min", "y_min"), relationship = "many-to-many")

# Keep for each ALLEMA single tree only the ALS acquisition whose year is the closest to LubiJahr_max
# (if two ALS years are equally close, the earlier one is kept)
ALLEMA_ALS_ref <- ALLEMA_ALS_ref %>%
  arrange(OBJECT_ID, abs(year - LubiJahr_max), year) %>%
  distinct(OBJECT_ID, .keep_all = TRUE) %>%
  rename(ALS_year = year)

# Get some idea about the ALS matching (one line per Quadrant)
ALLEMA_ALS_Q <- ALLEMA_ALS_ref %>% distinct(FK_Quadrat, LubiJahr_max, source, ALS_year)
table(ALLEMA_ALS_Q$source, useNA = "ifany") # SWISS is the closest for 159 Quadrants, SWISS2 for 5
table(abs(ALLEMA_ALS_Q$ALS_year - ALLEMA_ALS_Q$LubiJahr_max), useNA = "ifany") # 56 Quadrants within +/- 1 year (as before), up to 10 years

#-----------------------------------------------------
# Post-filtering of reference data
#-----------------------------------------------------

# Fetch the reference trees that should be considered
ALLEMA_EB <- ALLEMA_orig %>%
  filter(OBJECT_ID %in% unique(ALLEMA_ALS_ref$OBJECT_ID)) %>%
  st_zm(drop = TRUE, what = "ZM")

# Filter based on roundness
#---------------------------

# Add roundness info
ALLEMA_EB <- ALLEMA_EB %>%
  mutate(
    area_allema = st_area(Shape),
    perimeter = st_length(st_boundary(Shape)),
    roundness =
      as.numeric(
        (4 * pi * area_allema) / (perimeter^2)
      )
  )

# Keep only ref polygons with a roundness > 0.95
# Idea is to filter out polygons that do not represent a single tree but a group of trees
ALLEMA_EB <- ALLEMA_EB %>%
  filter(roundness > 0.95)

# Add the LubiJahr_max and the closest ALS year of each tree
ALLEMA_EB <- ALLEMA_EB %>%
  left_join(select(ALLEMA_ALS_ref, OBJECT_ID, LubiJahr_max, ALS_year), by = "OBJECT_ID")

# The perimeters of the Quadrants that still contain reference trees
ALLEMA_Q_ref <- ALLEMA_Q_orig %>%
  inner_join(distinct(st_drop_geometry(ALLEMA_EB), FK_Quadrat, LubiJahr_max),
             by = join_by(ID_Quadrat == FK_Quadrat))

#-----------------------------------------------------
# Selecting the SWISSIMAGE tile of each ALLEMA Quadrant
#* The ALLEMA Quadrants are 1km x 1km squares on the km grid, each Quadrant matches exactly one
#*   SWISSIMAGE tile per year
#* SWISSIMAGE 10 cm is made from 10 cm images in the plain areas and main alpine valleys, but from 25 cm images
#*   resampled to 10 cm over the Alps (source_gsd, from the swisstopo SWISSIMAGE tiling metadata)
#* The STAC only gives the year of the tiles (datetime is always the 1st of January), so the flight dates
#*   come from the swisstopo image strips (LUBIS, ch.swisstopo.lubis-bildstreifen) covering the Quadrant
#*   --> Only the strips with the source_gsd of the tile are considered (if there are some), e.g. the 10 cm
#*       TLM strips flown in April are not used for a 25 cm tile of the Alps flown in July
#*   --> A tile counts as summer only if all these strips were flown in summer_months
#-----------------------------------------------------

# Get stac source
stac_source <- rstac::stac("https://data.geo.admin.ch/api/stac/v1/")

# geo.admin identify service (for the flight dates and the source resolution)
identify_url <- "https://api3.geo.admin.ch/rest/services/api/MapServer/identify"

# Function listing the SWISSIMAGE tiles (one per year) of a Quadrant
getSwissimageTiles <- function(quad_geom, gsd){

  # Small bbox around the centre of the Quadrant, in WGS84 as expected by the STAC
  # (the bbox of the whole Quadrant would also return the neighbouring tiles)
  centre_bbox <- quad_geom %>%
    st_centroid() %>%
    st_buffer(10) %>%
    st_transform(4326) %>%
    st_bbox()

  stac_items <- rstac::stac_search(
    q = stac_source,
    collections = "ch.swisstopo.swissimage-dop10",
    bbox = as.numeric(centre_bbox),
    limit = 100
  ) %>%
    rstac::get_request()

  if (length(stac_items$features) == 0) {
    stop("No SWISSIMAGE tile found for the Quadrant at ", paste(st_bbox(quad_geom), collapse = ", "))
  }

  # Keep the year, the tile id and the url of the asset with the wanted resolution
  map(stac_items$features, function(item) {
    asset <- keep(item$assets, ~ .x$gsd == gsd)[[1]]
    tibble(
      swissimage_year = as.integer(substr(item$properties$datetime, 1, 4)),
      swissimage_tile = sub(".*_", "", item$id), # e.g. "2600-1199"
      swissimage_url = asset$href
    )
  }) %>%
    list_rbind()
}

# Function getting the resolution [m] of the images each SWISSIMAGE tile of a Quadrant was made from
getSourceGsd <- function(quad_geom, tile_id){

  response <- httr::RETRY(
    "GET",
    identify_url,
    query = list(
      geometryType = "esriGeometryPoint",
      geometry = paste(st_coordinates(st_centroid(quad_geom)), collapse = ","),
      layers = "all:ch.swisstopo.swissimage-product.metadata",
      sr = 2056,
      tolerance = 0,
      returnGeometry = "false",
      limit = 200
    ),
    quiet = TRUE
  )
  httr::stop_for_status(response)

  # One line per flight year and per year of the "journey through time", only the lines of the 1km tile are kept
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))$results$attributes %>%
    filter(kbnum == sub("-", "_", tile_id)) %>%
    group_by(swissimage_year = as.integer(flightyear)) %>%
    summarise(source_gsd = max(as.numeric(sub(" cm", "", gsd)) / 100))
}

# Function getting the flight dates of the swisstopo image strips a SWISSIMAGE tile was made from
getFlightDates <- function(quad_geom, year, source_gsd){

  # The identify service matches the flight line of the strips, not their footprint, so the strips are
  # searched in an envelope 5 km larger than the Quadrant and then intersected with the Quadrant
  response <- httr::RETRY(
    "GET",
    identify_url,
    query = list(
      geometryType = "esriGeometryEnvelope",
      geometry = paste(st_bbox(st_buffer(quad_geom, 5000)), collapse = ","),
      layers = "all:ch.swisstopo.lubis-bildstreifen",
      timeInstant = year,
      sr = 2056,
      tolerance = 0,
      returnGeometry = "true",
      geometryFormat = "geojson",
      limit = 200
    ),
    quiet = TRUE
  )
  httr::stop_for_status(response)

  strips <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$results

  if (length(strips) == 0) {
    return(as.Date(character()))
  }

  # Read the footprints as sf (the coordinates are in LV95, as asked with sr = 2056)
  strips <- list(
    type = "FeatureCollection",
    crs = list(type = "name", properties = list(name = "EPSG:2056")),
    features = strips
  ) %>%
    jsonlite::toJSON(auto_unbox = TRUE, digits = NA) %>%
    as.character() %>%
    st_read(quiet = TRUE)

  strips <- strips[lengths(st_intersects(strips, quad_geom)) > 0, ]

  # Keep only the strips with the resolution the tile was made from, if there are some
  if (!is.na(source_gsd)) {
    same_gsd <- which(as.numeric(strips$resolution) == source_gsd)
    if (length(same_gsd) > 0) {
      strips <- strips[same_gsd, ]
    }
  }

  sort(unique(as.Date(strips$flugdatum)))
}

# Function selecting the SWISSIMAGE tile to use for a Quadrant
selectSwissimageTile <- function(tiles, lubi_year, selection, max_extra_years){

  selection <- match.arg(selection, c("closest", "closest_summer"))

  # Order the tiles by year difference to LubiJahr_max (if two years are equally close, the earlier one first)
  tiles <- tiles %>%
    mutate(year_diff = abs(swissimage_year - lubi_year)) %>%
    arrange(year_diff, swissimage_year)

  if (selection == "closest_summer") {
    summer_tiles <- tiles %>%
      filter(summer, year_diff <= min(year_diff) + max_extra_years)

    if (nrow(summer_tiles) > 0) {
      return(summer_tiles[1, ])
    }
  }

  tiles[1, ]
}

# For each Quadrant, list its SWISSIMAGE tiles, flag the summer ones and select the one to use
SWISSIMAGE_Q <- map(seq_len(nrow(ALLEMA_Q_ref)), function(i) {

  quad_geom <- st_geometry(ALLEMA_Q_ref)[i]

  tiles <- getSwissimageTiles(quad_geom, swissimage_gsd)
  tiles <- left_join(tiles, getSourceGsd(quad_geom, tiles$swissimage_tile[1]), by = "swissimage_year")

  # Flight dates of the image strips each tile was made from
  tiles$flight_dates <- map2(tiles$swissimage_year, tiles$source_gsd, ~ getFlightDates(quad_geom, .x, .y))

  # A tile counts as summer only if all its image strips were flown in summer_months
  tiles$summer <- map_lgl(tiles$flight_dates, ~ length(.x) > 0 && all(as.integer(format(.x, "%m")) %in% summer_months))
  tiles$flight_dates <- map_chr(tiles$flight_dates, ~ paste(.x, collapse = ", "))

  selectSwissimageTile(tiles, ALLEMA_Q_ref$LubiJahr_max[i], swissimage_selection, summer_max_extra_years) %>%
    mutate(FK_Quadrat = ALLEMA_Q_ref$ID_Quadrat[i], LubiJahr_max = ALLEMA_Q_ref$LubiJahr_max[i], .before = 1)

}, .progress = TRUE) %>%
  list_rbind()

# Get some idea about the selected SWISSIMAGE tiles (numbers for closest_summer with May-September and 3 years)
table(SWISSIMAGE_Q$year_diff) # 98 of the 164 Quadrants get a tile of their LubiJahr_max ("closest": 109)
table(SWISSIMAGE_Q$summer) # 152 summer tiles, 12 Quadrants fell back to the closest tile
table(SWISSIMAGE_Q$source_gsd, useNA = "ifany") # 116 tiles made from 10 cm images, 46 from 25 cm images (Alps), 2 unknown

#-----------------------------------------------------
# Download the selected SWISSIMAGE tiles
#-----------------------------------------------------

# Local path of each selected tile
SWISSIMAGE_Q$SwissImage_link <- paste0(swissimage_path, basename(SWISSIMAGE_Q$swissimage_url))

# Make sure the destination folder exists
dir.create(swissimage_path, recursive = TRUE, showWarnings = FALSE)

# Set up parallel processing
n_workers <- 5
plan(multisession, workers = n_workers)

# Download each tile in parallel
invisible(future_lapply(
  unique(SWISSIMAGE_Q$swissimage_url),
  function(url) {

    dest <- paste0(swissimage_path, basename(url))

    # Skip files already downloaded
    if (file.exists(dest)) {
      return(NULL)
    }

    # A 10cm tile is ~60-80 MB, so allow more than the default 60 s
    options(timeout = 3600)

    ok <- tryCatch(
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE) == 0,
      error = function(e) FALSE,
      warning = function(w) FALSE
    )

    # Remove partial files, otherwise they would be skipped at the next run
    if (!ok) {
      unlink(dest)
      message("Failed to download ", url)
    }

    return(NULL)
  },
  future.seed = TRUE
))

plan(sequential)

# Check that all selected tiles are available locally (if not, rerun the download)
missing_tiles <- SWISSIMAGE_Q %>% filter(!file.exists(SwissImage_link))
if (nrow(missing_tiles) > 0) {
  warning(nrow(missing_tiles), " SWISSIMAGE tiles could not be downloaded, see missing_tiles")
}

#-----------------------------------------------------
# Export the data that will be used for the ML
#-----------------------------------------------------

# SwissImage_gsd [m]: resolution of the images the tile was made from (0.25 in the Alps), all tiles are delivered at 0.1
ALLEMA_EB_ML <- ALLEMA_EB %>%
  left_join(select(SWISSIMAGE_Q, FK_Quadrat, SwissImage_link, SwissImage_gsd = source_gsd), by = "FK_Quadrat") %>%
  select(FK_Quadrat, Gehoelztyp, LubiJahr = LubiJahr_max, ALS_year, SwissImage_link, SwissImage_gsd) %>%
  st_transform(2056) # same CRS as the SWISSIMAGE tiles (drops LHN95, the Z was dropped anyway, coordinates do not change)

dir.create(ALLEMA_ML_path, recursive = TRUE, showWarnings = FALSE)

st_write(ALLEMA_EB_ML, paste0(ALLEMA_ML_path, "ALLEMA_EB_ML_", swissimage_selection, ".gpkg"), append = FALSE)
