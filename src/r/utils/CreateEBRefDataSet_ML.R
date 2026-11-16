#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)
library(purrr)
library(rstac)        # SWISSIMAGE tiles from the STAC of data.geo.admin.ch
library(httr)         # LUBIS image strips from the api3.geo.admin.ch identify service
library(jsonlite)
library(future.apply) # parallel download of the SWISSIMAGE tiles

# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Config
#-----------------------------------------------------

swissimage_gsd <- 0.1 # [m] resolution of the tiles to download (0.1 or 2)

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
# --> the ALS tiles are actually all on the 1km grid (x_min and y_min multiples of 1000)

#-----------------------------------------------------
# Prepping the ALLEMA dataset
#* Ex FK-Quadrant 581166 :
#*   OBJECTID 152189 has Erh_Jahr = 2024 and LubiJahr = 2017
#*   OBJECTID 152330 has Erh_Jahr = 2024 and LubiJahr = 2020
#*   --> If an object stayed the same in 2020 as in 2017 it will keep LubiJahr 2017 even though it is visible in 2020
#* LubiJahr, LubiMonat and LubiTag give the date of the aerial image each object was digitised on
#*   (filled for all objects of classes 37, 38 and 59)
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
  st_zm(drop = TRUE, what = "ZM")

# Filter based on roundness
#---------------------------

# Add roundness info
ALLEMA <- ALLEMA %>%
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
ALLEMA <- ALLEMA %>%
  filter(roundness > 0.95)

# Date of the aerial image and 1km tile of each object
#------------------------------------------------------
#* The 1km tile is the one containing the centroid of the object, it is used for the SWISSIMAGE tiles
#*   (STAC tile id, e.g. "2677-1214") and for the ALS tiles (x_min, y_min)

centroids <- st_coordinates(st_centroid(st_geometry(ALLEMA)))

ALLEMA <- ALLEMA %>%
  mutate(
    Lubi_date = as.Date(sprintf("%d-%02d-%02d", LubiJahr, LubiMonat, LubiTag)),
    E_centroid = centroids[, "X"],
    N_centroid = centroids[, "Y"],
    x_min = as.integer(floor(E_centroid / 1000) * 1000),
    y_min = as.integer(floor(N_centroid / 1000) * 1000),
    tile_id = sprintf("%d-%d", x_min %/% 1000L, y_min %/% 1000L)
  )

#-----------------------------------------------------
# Finding the SWISSIMAGE tile of the LubiJahr of each object
#* The SWISSIMAGE tile of an object is the tile of its 1km tile whose flight year is its LubiJahr
#* The STAC only has the SWISSIMAGE 10 cm tiles, i.e. the flight years 2017 to 2025
#*   --> Objects with an older LubiJahr (or with no tile of their LubiJahr in the STAC) can only be seen in the
#*       "journey through time" (Zeitreise) of SWISSIMAGE, the images have to be asked to swisstopo (manual list)
#-----------------------------------------------------

# Get stac source
stac_source <- rstac::stac("https://data.geo.admin.ch/api/stac/v1/")

# geo.admin identify service (for the LUBIS image strips)
identify_url <- "https://api3.geo.admin.ch/rest/services/api/MapServer/identify"

# Function listing the SWISSIMAGE tiles (one per year) of a 1km tile in the STAC
getSwissimageTiles <- function(tile_geom, gsd){

  # Small bbox around the centre of the tile, in WGS84 as expected by the STAC
  # (the bbox of the whole tile would also return the neighbouring tiles)
  centre_bbox <- tile_geom %>%
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
    warning("No SWISSIMAGE tile found in the STAC for the tile at ", paste(st_bbox(tile_geom), collapse = ", "))
    return(tibble(swissimage_year = integer(), swissimage_tile = character(), swissimage_url = character()))
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

# Function creating 1km squares (LV95) from the coordinates (integers) of their lower left corner
kmSquares <- function(x_min, y_min){
  st_as_sfc(
    sprintf("POLYGON((%1$d %2$d, %3$d %2$d, %3$d %4$d, %1$d %4$d, %1$d %2$d))",
            x_min, y_min, x_min + 1000L, y_min + 1000L),
    crs = 2056
  )
}

# The 1km tiles of the objects
TILES <- ALLEMA %>%
  st_drop_geometry() %>%
  distinct(tile_id, x_min, y_min) %>%
  arrange(tile_id)

TILES <- st_sf(TILES, geometry = kmSquares(TILES$x_min, TILES$y_min))

# For each 1km tile, list its SWISSIMAGE tiles in the STAC
SWISSIMAGE_T <- map(seq_len(nrow(TILES)), function(i) {

  getSwissimageTiles(st_geometry(TILES)[i], swissimage_gsd) %>%
    mutate(tile_id = TILES$tile_id[i], .before = 1)

}, .progress = TRUE) %>%
  list_rbind()


# Attribute to each object the SWISSIMAGE tile of its LubiJahr
ALLEMA <- ALLEMA %>%
  left_join(select(SWISSIMAGE_T, -swissimage_tile),
            by = join_by(tile_id, LubiJahr == swissimage_year),
            relationship = "many-to-one") %>%
  mutate(image_source = if_else(is.na(swissimage_url), "manual", "stac"))

# Get some idea about the objects whose SWISSIMAGE tile can be downloaded
table(ALLEMA$LubiJahr, ALLEMA$image_source, useNA = "ifany")

#-----------------------------------------------------
# Download the SWISSIMAGE tiles available in the STAC
#-----------------------------------------------------

# Local path of the tile of each object
ALLEMA <- ALLEMA %>%
  mutate(SwissImage_link = if_else(image_source == "stac", paste0(swissimage_path, basename(swissimage_url)), NA_character_))

# Make sure the destination folder exists
dir.create(swissimage_path, recursive = TRUE, showWarnings = FALSE)

# Set up parallel processing
n_workers <- 5
plan(multisession, workers = n_workers)

# Download each tile in parallel
invisible(future_lapply(
  unique(na.omit(ALLEMA$swissimage_url)),
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

# Check that all tiles are available locally (if not, rerun the download)
missing_tiles <- ALLEMA %>%
  st_drop_geometry() %>%
  filter(image_source == "stac", !file.exists(SwissImage_link)) %>%
  distinct(SwissImage_link)
if (nrow(missing_tiles) > 0) {
  warning(nrow(missing_tiles), " SWISSIMAGE tiles could not be downloaded, see missing_tiles")
}

#-----------------------------------------------------
# List of the images to ask to swisstopo (manual download)
#* One request per ALLEMA quadrant and date of the aerial image (request_id, e.g. "773166_2012-08-28")
#* The swisstopo image strips (LUBIS, ch.swisstopo.lubis-bildstreifen) flown on the date of the aerial image and
#*   intersecting the quadrant are added (all of them if there are several), to identify the images without doubt
#* lubis_resolution [m]: resolution of the strips
#* lubis_project: project the strips were flown for (e.g. "SWISSIMAGE", "TLM" or "FOREST MONITORING")
#-----------------------------------------------------

# Function getting the swisstopo image strips flown in a year around a 1km square
getLubisStrips <- function(tile_geom, year){

  # The identify service matches the flight line of the strips, not their footprint, so the strips are
  # searched in an envelope 5 km larger than the square (their footprint is then intersected with the square)
  response <- httr::RETRY(
    "GET",
    identify_url,
    query = list(
      geometryType = "esriGeometryEnvelope",
      geometry = paste(st_bbox(st_buffer(tile_geom, 5000)), collapse = ","),
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
    return(NULL)
  }
  if (length(strips) == 200) {
    warning("200 image strips returned in ", year, " for the tile at ", paste(st_bbox(tile_geom), collapse = ", "),
            ", some strips might be missing")
  }

  # Keep the id of the strips (e.g. "20120828_1016_1308") as attribute
  strips <- map(strips, function(strip) {
    strip$properties$strip_id <- as.character(strip$id %||% strip$featureId)
    strip
  })

  # Read the footprints as sf (the coordinates are in LV95, as asked with sr = 2056)
  strips <- list(
    type = "FeatureCollection",
    crs = list(type = "name", properties = list(name = "EPSG:2056")),
    features = strips
  ) %>%
    jsonlite::toJSON(auto_unbox = TRUE, digits = NA) %>%
    as.character() %>%
    st_read(quiet = TRUE)

  # goal: project the strips were flown for (e.g. "SWISSIMAGE", "TLM", "FOREST MONITORING")
  strips %>%
    mutate(flugdatum = as.Date(flugdatum), resolution = as.numeric(resolution)) %>%
    select(strip_id, flugdatum, resolution, goal)
}

# Function pasting the unique values of a vector (NA if empty)
pasteUnique <- function(x){
  if (length(x) > 0) paste(sort(unique(x)), collapse = ", ") else NA_character_
}

# The objects whose SWISSIMAGE tile is not in the STAC
ALLEMA <- ALLEMA %>%
  mutate(SwissImage_request_id = if_else(image_source == "manual", paste(FK_Quadrat, format(Lubi_date), sep = "_"), NA_character_))

ALLEMA_manual <- ALLEMA %>%
  st_drop_geometry() %>%
  filter(image_source == "manual")

# The quadrants of these objects
QUADRANTS <- ALLEMA_manual %>%
  distinct(FK_Quadrat) %>%
  mutate(
    E_min = 2000000L + (FK_Quadrat %/% 1000L) * 1000L,
    N_min = 1000000L + (FK_Quadrat %% 1000L) * 1000L,
    E_max = E_min + 1000L,
    N_max = N_min + 1000L
  )

QUADRANTS <- st_sf(QUADRANTS, geometry = kmSquares(QUADRANTS$E_min, QUADRANTS$N_min))

# For each quadrant and LubiJahr, the image strips flown on the date of each request intersecting the quadrant
manual_quadrant_years <- ALLEMA_manual %>% distinct(FK_Quadrat, LubiJahr)

LUBIS_requests <- map(seq_len(nrow(manual_quadrant_years)), function(i) {

  quadrant_geom <- st_geometry(QUADRANTS)[QUADRANTS$FK_Quadrat == manual_quadrant_years$FK_Quadrat[i]]

  requests <- ALLEMA_manual %>%
    filter(FK_Quadrat == manual_quadrant_years$FK_Quadrat[i], LubiJahr == manual_quadrant_years$LubiJahr[i]) %>%
    distinct(SwissImage_request_id, Lubi_date)

  strips <- getLubisStrips(quadrant_geom, manual_quadrant_years$LubiJahr[i])

  if (is.null(strips)) {
    return(tibble(SwissImage_request_id = requests$SwissImage_request_id, lubis_strip_ids = NA_character_,
                  lubis_resolution = NA_character_, lubis_project = NA_character_))
  }

  strips <- strips[lengths(st_intersects(strips, quadrant_geom)) > 0, ]

  map(seq_len(nrow(requests)), function(j) {
    strips_req <- strips[which(strips$flugdatum == requests$Lubi_date[j]), ]
    tibble(
      SwissImage_request_id = requests$SwissImage_request_id[j],
      lubis_strip_ids = pasteUnique(strips_req$strip_id),
      lubis_resolution = pasteUnique(strips_req$resolution),
      lubis_project = pasteUnique(strips_req$goal)
    )
  }) %>%
    list_rbind()

}, .progress = TRUE) %>%
  list_rbind()

# One line per request, with the date, the quadrant and its coordinates (LV95), and the image strips to ask
SWISSIMAGE_manual_requests <- ALLEMA_manual %>%
  group_by(request_id = SwissImage_request_id, Lubi_date, LubiJahr, FK_Quadrat) %>%
  summarise(n_objects = n(), .groups = "drop") %>%
  left_join(st_drop_geometry(QUADRANTS), by = "FK_Quadrat") %>%
  left_join(LUBIS_requests, by = join_by(request_id == SwissImage_request_id)) %>%
  mutate(reason = if_else(LubiJahr < 2017, "Zeitreise only (< 2017)", "no STAC tile for LubiJahr")) %>%
  arrange(Lubi_date, FK_Quadrat) %>%
  select(request_id, Lubi_date, FK_Quadrat, E_min, N_min, E_max, N_max,
         lubis_strip_ids, lubis_resolution, lubis_project, n_objects, reason)

# Get some idea about the projects of the images to ask (e.g. "FOREST MONITORING" images are not in SWISSIMAGE)
table(SWISSIMAGE_manual_requests$lubis_project, SWISSIMAGE_manual_requests$reason, useNA = "ifany")

# Get some idea about the requests
table(SWISSIMAGE_manual_requests$reason, useNA = "ifany")
table(SWISSIMAGE_manual_requests$lubis_resolution, useNA = "ifany")
sum(is.na(SWISSIMAGE_manual_requests$lubis_strip_ids)) # requests without an image strip on their date
sum(grepl(",", SWISSIMAGE_manual_requests$lubis_strip_ids)) # requests with several image strips on their date

dir.create(ALLEMA_ML_path, recursive = TRUE, showWarnings = FALSE)

write.csv(SWISSIMAGE_manual_requests, paste0(ALLEMA_ML_path, "SWISSIMAGE_manual_requests.csv"), row.names = FALSE)

#-----------------------------------------------------
# Matching the closest ALS year with ALLEMA LubiJahr or LubiJahr_max
#* For each object the ALS acquisition of its 1km tile closest to either LubiJahr or LubiJahr_max is kept
#*   (if two ALS years are equally close, the earlier one is kept)
#* ALS_ref: which of LubiJahr and LubiJahr_max is the closest to the ALS year (LubiJahr if both are)
#* ALS_diff: ALS_year - ALS_ref year [years], i.e. negative if the ALS was flown before the reference year
#* Objects without ALS are kept (ALS attributes NA), no filter on ALS_diff
#-----------------------------------------------------

ALLEMA_ALS <- ALLEMA %>%
  st_drop_geometry() %>%
  select(OBJECT_ID, LubiJahr, LubiJahr_max, x_min, y_min) %>%
  inner_join(ALS, by = c("x_min", "y_min"), relationship = "many-to-many") %>%
  mutate(
    diff_lubi = year - LubiJahr,
    diff_lubi_max = year - LubiJahr_max
  ) %>%
  arrange(OBJECT_ID, pmin(abs(diff_lubi), abs(diff_lubi_max)), year) %>%
  distinct(OBJECT_ID, .keep_all = TRUE) %>%
  transmute(
    OBJECT_ID,
    ALS_source = source,
    ALS_year = year,
    ALS_ref = if_else(abs(diff_lubi) <= abs(diff_lubi_max), "LubiJahr", "LubiJahr_max"),
    ALS_diff = if_else(ALS_ref == "LubiJahr", diff_lubi, diff_lubi_max)
  )

ALLEMA <- ALLEMA %>%
  left_join(ALLEMA_ALS, by = "OBJECT_ID")

# Get some idea about the ALS matching
table(ALLEMA$ALS_source, useNA = "ifany")
table(ALLEMA$ALS_ref, useNA = "ifany")
table(ALLEMA$ALS_diff, useNA = "ifany")
table(ALLEMA$ALS_diff, ALLEMA$image_source, useNA = "ifany")

#-----------------------------------------------------
# Export the data that will be used for the ML
#* All objects are kept, image_source tells if their SWISSIMAGE tile is available ("stac", SwissImage_link)
#*   or has to be asked to swisstopo ("manual", SwissImage_request_id, see SWISSIMAGE_manual_requests.csv)
#* LubiJahr is the LubiJahr of the object (not LubiJahr_max anymore)
#-----------------------------------------------------

ALLEMA_EB_ML <- ALLEMA %>%
  select(OBJECT_ID, FK_Quadrat, Gehoelztyp, LubiJahr, Lubi_date, LubiJahr_max, tile_id,
         image_source, SwissImage_link, SwissImage_request_id,
         ALS_source, ALS_year, ALS_ref, ALS_diff) %>%
  st_transform(2056) # same CRS as the SWISSIMAGE tiles (drops LHN95, the Z was dropped anyway, coordinates do not change)

st_write(ALLEMA_EB_ML, paste0(ALLEMA_ML_path, "ALLEMA_EB_ML_temp.gpkg"), append = FALSE)


#-----------------------------------------------------
# Adding images from swisstopo prepared download
#* swisstopo prepared a manual downlaod based on SWISSIMAGE_manual_requests.csv
#* some images could however still not be found: 
#* request_id	Lubi_date	FK_Quadrat	E_min	N_min	E_max	N_max	lubis_strip_ids	lubis_resolution	lubis_project	n_objects	reason	Alternative Jahrgänge
#* 743206_2013-08-22	22.08.2013	743206	2743000	1206000	2744000	1207000	20130822_0929_1308, 20130822_1316_1308	0.5	TLM	389	Zeitreise only (< 2017)	nur 2014 oder 2011
#* 719222_2014-07-18	18.07.2014	719222	2719000	1222000	2720000	1223000	20140718_0825_30030, 20140718_0839_30030	0.5	TLM	11	Zeitreise only (< 2017)	nur 2013 oder 2016
#* 761198_2015-07-01	01.07.2015	761198	2761000	1198000	2762000	1199000	20150701_0749_1308, 20150701_0757_1308	0.2	FOREST MONITORING	70	Zeitreise only (< 2017)	nur 2014
#* 563190_2015-09-29	29.09.2015	563190	2563000	1190000	2564000	1191000	NA	NA	NA	7	Zeitreise only (< 2017)	nur 2016 oder 2013
#* 695270_2017-06-18	18.06.2017	695270	2695000	1270000	2696000	1271000	20170618_1355_12504, 20170618_1409_12504	0.25	LAND USE STATISTICS	86	no STAC tile for LubiJahr	nur 2019 oder 2014
#* 701262_2017-06-18	18.06.2017	701262	2701000	1262000	2702000	1263000	20170618_1424_12504, 20170618_1440_12504	0.25	LAND USE STATISTICS	148	no STAC tile for LubiJahr	nur 2019 oder 2014
#* 707254_2017-06-18	18.06.2017	707254	2707000	1254000	2708000	1255000	20170618_1514_12504, 20170618_1530_12504	0.25	LAND USE STATISTICS	96	no STAC tile for LubiJahr	nur 2019 oder 2016
#* 713278_2017-06-18	18.06.2017	713278	2713000	1278000	2714000	1279000	20170618_1331_12504	0.25	LAND USE STATISTICS	222	no STAC tile for LubiJahr	nur 2019 oder 2016
#* 719254_2017-06-18	18.06.2017	719254	2719000	1254000	2720000	1255000	20170618_1514_12504, 20170618_1530_12504	0.25	LAND USE STATISTICS	32	no STAC tile for LubiJahr	nur 2019 oder 2016
#* 731270_2017-06-18	18.06.2017	731270	2731000	1270000	2732000	1271000	20170618_1355_12504, 20170618_1409_12504	0.25	LAND USE STATISTICS	375	no STAC tile for LubiJahr	nur 2019 oder 2014
#* 755254_2017-06-18	18.06.2017	755254	2755000	1254000	2756000	1255000	20170618_1514_12504, 20170618_1530_12504	0.25	LAND USE STATISTICS	105	no STAC tile for LubiJahr	nur 2019 oder 2014
#* 725246_2017-06-26	26.06.2017	725246	2725000	1246000	2726000	1247000	20170626_1153_12504, 20170626_1205_12504	0.25	LAND USE STATISTICS	182	no STAC tile for LubiJahr	nur 2019 oder 2016
#* 569214_2018-06-22	22.06.2018	569214	2569000	1214000	2570000	1215000	20180622_1024_12501, 20180622_1042_12501	0.1	TLM	28	no STAC tile for LubiJahr	nur 2017 oder 2020
#* 689230_2018-08-16	16.08.2018	689230	2689000	1230000	2690000	1231000	20180816_0910_12501, 20180816_0920_12501	0.15	FOREST MONITORING	257	no STAC tile for LubiJahr	nur 2019 oder 2016
#* 581150_2018-09-27	27.09.2018	581150	2581000	1150000	2582000	1151000	20180927_0949_12504, 20180927_1003_12504	0.25	SWISSIMAGE	60	no STAC tile for LubiJahr	nur 2020 oder 2017
#* 713158_2019-09-14	14.09.2019	713158	2713000	1158000	2714000	1159000	20190914_0957_12504, 20190914_1022_12504, 20190914_1045_12504	0.25	SWISSIMAGE	754	no STAC tile for LubiJahr	nur 2021 oder 2018
#* 605238_2020-05-18	18.05.2020	605238	2605000	1238000	2606000	1239000	20200518_0847_12501, 20200518_0903_12501	0.1	TLM	1844	no STAC tile for LubiJahr	nur 2021 oder 2018
#* 575110_2021-07-02	02.07.2021	575110	2575000	1110000	2576000	1111000	20210702_0930_12501	0.1	FOREST MONITORING	193	no STAC tile for LubiJahr	nur 2023 oder 2020
#* 641134_2021-07-29	29.07.2021	641134	2641000	1134000	2642000	1135000	20210729_1020_12504	0.125	CRYOSPHERE MONITORING	638	no STAC tile for LubiJahr	nur 2023 oder 2020
#* 785150_2021-09-01	01.09.2021	785150	2785000	1150000	2786000	1151000	20210901_1019_12501, 20210901_1028_12501, 20210901_1035_12501	0.1, 0.125	CRYOSPHERE MONITORING	7628	no STAC tile for LubiJahr	nur 2022 oder 2019
#* 527190_2022-06-11	11.06.2022	527190	2527000	1190000	2528000	1191000	20220611_0932_12504	0.25	BIOTOPE MONITORING	183	no STAC tile for LubiJahr	nur 2023 oder 2020
#* 569198_2022-06-11	11.06.2022	569198	2569000	1198000	2570000	1199000	20220611_1307_12504, 20220611_1333_12504	0.25	BIOTOPE MONITORING	154	no STAC tile for LubiJahr	nur 2023 oder 2020
#* 629118_2022-07-15	15.07.2022	629118	2629000	1118000	2630000	1119000	20220715_0909_12501, 20220715_0926_12501, 20220715_0937_12501	0.125	CRYOSPHERE MONITORING	605	no STAC tile for LubiJahr	nur 2023 oder 2020
#* 539158_2022-08-13	13.08.2022	539158	2539000	1158000	2540000	1159000	20220813_0909_12501, 20220813_0926_12501	0.25	DISASTER MAPPING	65	no STAC tile for LubiJahr	nur 2023 oder 2020
#* 557150_2022-08-13	13.08.2022	557150	2557000	1150000	2558000	1151000	20220813_0805_12501, 20220813_0816_12501	0.25	DISASTER MAPPING	331	no STAC tile for LubiJahr	nur 2023 oder 2020
#* The images are organised as <year>/<request_id>/<name>.tif, the name of the tif changes between deliveries
#*   (e.g. DOP25_LV95_Mosaic.tif, DOP10_LV95_Mosaic.tif), so each folder is searched for its tif
#-----------------------------------------------------

swissimage_manual_path <- paste0(orig_data_path, "SWISSIMAGE-swisstopo-manual-download")

# Import the data for the ML
ALLEMA_EB_ML <- st_read(paste0(ALLEMA_ML_path, "ALLEMA_EB_ML_temp.gpkg"))

# The tifs delivered by swisstopo, with the request_id given by the name of their folder
# (".tif$" also leaves out the ".tif.aux.xml" files)
SWISSIMAGE_manual_tifs <- tibble(
  SwissImage_manual_link = list.files(swissimage_manual_path, pattern = "\\.tif$", recursive = TRUE,
                                      full.names = TRUE, ignore.case = TRUE)
) %>%
  mutate(
    SwissImage_request_id = basename(dirname(SwissImage_manual_link))
  )

# Fill the SwissImage_link of the objects whose request was delivered (the STAC links are kept)
# image_source becomes "swisstopo" for these objects, the objects still without an image stay "manual"
ALLEMA_EB_ML <- ALLEMA_EB_ML %>%
  left_join(SWISSIMAGE_manual_tifs, by = "SwissImage_request_id", relationship = "many-to-one") %>%
  mutate(
    image_source = if_else(is.na(SwissImage_link) & !is.na(SwissImage_manual_link), "swisstopo", image_source),
    SwissImage_link = coalesce(SwissImage_link, SwissImage_manual_link)
  ) %>%
  select(-SwissImage_manual_link)

# The objects still without an image, to have a look at them (they are kept in ALLEMA_EB_ML, their link will be filled manually)
ALLEMA_EB_ML_no_image <- ALLEMA_EB_ML %>%
  filter(is.na(SwissImage_link))

# Get some idea about the objects with an image
table(ALLEMA_EB_ML$image_source, has_image = !is.na(ALLEMA_EB_ML$SwissImage_link), useNA = "ifany")

st_write(ALLEMA_EB_ML, paste0(ALLEMA_ML_path, "ALLEMA_EB_ML_temp.gpkg"), append = FALSE)

#-----------------------------------------------------
# Adding the last image links manually from folder prepared by CG 
#* \\speedy12-37\data_17\_ORTHOBILDER\_ALLEMA
#-----------------------------------------------------

# Check out how many images need really to be manually attributed
missing_images <- unique(data.frame(FK_Quadrat = ALLEMA_EB_ML_no_image$FK_Quadrat,date =ALLEMA_EB_ML_no_image$Lubi_date)) 
nrow(missing_images) #55

# Get the FK_Quadrat Gemeinde kuerzel
ALLEMA_Q <- st_read(paste0(orig_data_path,"ALLEMA/ALLEMA_Quadranten.shp"))
missing_images <- missing_images %>%
  left_join(st_drop_geometry(ALLEMA_Q[,c("ID_Quadrat", "Gemde_d_k")]), by = join_by("FK_Quadrat" == "ID_Quadrat"))

write.csv(missing_images, paste0(ALLEMA_ML_path, "WSL_manual_requests.csv"), row.names = FALSE)

wsl_manual_link = list.files("//speedy12-37/data_17/_ORTHOBILDER/_ALLEMA", pattern = "\\.tif$", recursive = TRUE,
                                      full.names = TRUE, ignore.case = TRUE)

# Have a look at each kuerzel and manually complete csv
wsl_manual_link[grepl("CORS", wsl_manual_link)]

# Reload the filled csv
missing_images_filled <- read.csv(paste0(ALLEMA_ML_path, "WSL_manual_requests.csv"), sep=";")

# Keep only the urls which match the Lubi_year
missing_images_filled <- missing_images_filled %>%
  filter(correct_year == 1)

# Same date format as Lubi_date (the csv was saved with Excel, dates as dd.mm.yyyy)
missing_images_filled <- missing_images_filled %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  select(FK_Quadrat, date, WSL_intern_url)

# Reload the data for the ML
ALLEMA_EB_ML <- st_read(paste0(ALLEMA_ML_path, "ALLEMA_EB_ML_temp.gpkg"))

# Fill the SwissImage_link of the objects still without an image with the WSL intern image of their quadrant and date
# (the links already present are kept)
ALLEMA_EB_ML <- ALLEMA_EB_ML %>%
  left_join(missing_images_filled, by = join_by(FK_Quadrat, Lubi_date == date), relationship = "many-to-one") %>%
  mutate(
    image_source = if_else(is.na(SwissImage_link) & !is.na(WSL_intern_url), "wsl_intern", image_source),
    SwissImage_link = coalesce(SwissImage_link, WSL_intern_url)
  ) %>%
  select(-WSL_intern_url)

# Export it
st_write(ALLEMA_EB_ML, paste0(ALLEMA_ML_path, "ALLEMA_EB_ML.gpkg"), append = FALSE)

# Get some idea about the objects with an image
table(ALLEMA_EB_ML$image_source, has_image = !is.na(ALLEMA_EB_ML$SwissImage_link), useNA = "ifany")

# And again, check what is left
ALLEMA_EB_ML_no_image <- ALLEMA_EB_ML %>%
  filter(is.na(SwissImage_link))

missing_images <- unique(data.frame(FK_Quadrat = ALLEMA_EB_ML_no_image$FK_Quadrat,date =ALLEMA_EB_ML_no_image$Lubi_date))
nrow(missing_images)

#-----------------------------------------------------
# Adding the GSD of the images
#* SwissImage_gsd [m]: resolution of the images the image of each object was made from
#*   stac: the SWISSIMAGE 10 cm tiles are all delivered at 0.1, but over the Alps they are made from 25 cm images
#*         resampled to 10 cm --> resolution of the source images, from the swisstopo SWISSIMAGE tiling metadata
#*         (ch.swisstopo.swissimage-product.metadata, one line per 1km tile and flight year)
#*   swisstopo, wsl_intern: pixel size of the tif (delivered at their native resolution, e.g. DOP25 -> 0.25)
#*   manual (still no image): NA
#* Needs identify_url (see "Finding the SWISSIMAGE tile of the LubiJahr of each object")
#-----------------------------------------------------

# Function getting the resolution [m] of the images the SWISSIMAGE tiles (one per flight year) of a 1km tile were made from
getSourceGsd <- function(tile_id){

  # Centre of the 1km tile (tile_id gives the km coordinates of its lower left corner, e.g. "2677-1214")
  centre <- as.integer(strsplit(tile_id, "-")[[1]]) * 1000L + 500L

  response <- httr::RETRY(
    "GET",
    identify_url,
    query = list(
      geometryType = "esriGeometryPoint",
      geometry = paste(centre, collapse = ","),
      layers = "all:ch.swisstopo.swissimage-product.metadata",
      sr = 2056,
      tolerance = 0,
      returnGeometry = "false",
      limit = 200
    ),
    quiet = TRUE
  )
  httr::stop_for_status(response)

  attributes <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))$results$attributes

  if (is.null(attributes)) {
    warning("No SWISSIMAGE metadata found for the tile ", tile_id)
    return(tibble(tile_id = character(), LubiJahr = integer(), SwissImage_gsd = numeric()))
  }

  # One line per flight year and per year of the "journey through time" (other tile ids, e.g. "1187-44"),
  # only the lines of the 1km tile are kept (if a year has several resolutions, the coarsest one is kept)
  attributes %>%
    filter(kbnum == sub("-", "_", tile_id)) %>%
    group_by(LubiJahr = as.integer(flightyear)) %>%
    summarise(SwissImage_gsd = max(as.numeric(sub(" cm", "", gsd)) / 100), .groups = "drop") %>%
    mutate(tile_id = tile_id, .before = 1)
}

# Function getting the pixel size [m] of a tif (only its header is read)
getPixelSize <- function(tif){
  jsonlite::fromJSON(sf::gdal_utils("info", tif, options = "-json", quiet = TRUE))$geoTransform[2]
}

# Import the data for the ML
ALLEMA_EB_ML <- st_read(paste0(ALLEMA_ML_path, "ALLEMA_EB_ML.gpkg"))

# Resolution of the source images of the STAC tiles, for the 1km tiles of the objects whose image comes from the STAC
SWISSIMAGE_gsd <- ALLEMA_EB_ML %>%
  st_drop_geometry() %>%
  filter(image_source == "stac") %>%
  distinct(tile_id) %>%
  pull(tile_id) %>%
  map(getSourceGsd, .progress = TRUE) %>%
  list_rbind()

# Pixel size of the tifs delivered by swisstopo and of the WSL intern images
TIF_gsd <- ALLEMA_EB_ML %>%
  st_drop_geometry() %>%
  filter(image_source %in% c("swisstopo", "wsl_intern")) %>%
  distinct(SwissImage_link) %>%
  mutate(tif_gsd = map_dbl(SwissImage_link, getPixelSize, .progress = TRUE))

ALLEMA_EB_ML <- ALLEMA_EB_ML %>%
  left_join(SWISSIMAGE_gsd, by = join_by(tile_id, LubiJahr), relationship = "many-to-one") %>%
  left_join(TIF_gsd, by = "SwissImage_link", relationship = "many-to-one") %>%
  mutate(SwissImage_gsd = if_else(image_source == "stac", SwissImage_gsd, tif_gsd)) %>%
  select(-tif_gsd) %>%
  relocate(SwissImage_gsd, .after = SwissImage_link)

# Get some idea about the GSD (only the "manual" objects should be NA)
table(ALLEMA_EB_ML$image_source, gsd = ALLEMA_EB_ML$SwissImage_gsd, useNA = "ifany")

# Export it
st_write(ALLEMA_EB_ML, paste0(ALLEMA_ML_path, "ALLEMA_EB_ML_v4.gpkg"), append = FALSE)
