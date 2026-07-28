#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(future.apply)
library(sf)

# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Download SWISSIMAGE tiles
#-----------------------------------------------------

# Read the list of tile URLs
urls <- readLines("data/SWISSIMAGE_2025_TG.csv")

# Make sure the destination folder exists
dir.create(swissimage_path, recursive = TRUE, showWarnings = FALSE)

# Set up parallel processing
n_workers <- 5
plan(multisession, workers = n_workers)

# Download each tile in parallel
future_lapply(
  urls,
  function(url) {

    dest <- file.path(swissimage_path, basename(url))

    # Skip files already downloaded
    if (file.exists(dest)) {
      return(NULL)
    }

    tryCatch(
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
      error = function(e) message("Failed to download ", url, ": ", conditionMessage(e))
    )

    return(NULL)
  },
  future.seed = TRUE
)

#-----------------------------------------------------
# Flag tiles that intersect the LN parcels
#-----------------------------------------------------

# Build each tile's bounding box directly from its tile ID in the URL
tile_id <- regmatches(urls, regexpr("[0-9]{4}-[0-9]{4}", urls))
tile_xy <- do.call(rbind, strsplit(tile_id, "-"))
xmin <- as.numeric(tile_xy[, 1]) * 1000
ymin <- as.numeric(tile_xy[, 2]) * 1000

tile_wkt <- sprintf(
  "POLYGON((%1$f %2$f, %3$f %2$f, %3$f %4$f, %1$f %4$f, %1$f %2$f))",
  xmin, ymin, xmin + 1000, ymin + 1000
)
tiles <- st_sf(url = urls, geometry = st_as_sfc(tile_wkt, crs = 2056))

# Only load the LN parcels falling within the extent covered by the tiles
aoi_wkt <- st_as_text(st_as_sfc(st_bbox(tiles)))
LN <- st_read(LN_2025_path, wkt_filter = aoi_wkt)

# Flag tiles that intersect at least one LN parcel
tiles$LN_mask <- lengths(st_intersects(tiles, LN)) > 0

# Check out which lines do not have an intersecting LN parcel
tiles[which(tiles$LN_mask == FALSE),] # 148 out of 1155

# Save the updated tile list, keeping the url and LN_mask columns
write.csv(st_drop_geometry(tiles), "data/SWISSIMAGE_2025_TG.csv", row.names = FALSE)