#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(future.apply)

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
