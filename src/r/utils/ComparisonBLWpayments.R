#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(terra)
library(sf)
library(dplyr)

# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Cantons to assess
#-----------------------------------------------------

# The 13 cantons that have the newest lidar acquisition (stand 30.07.2026)
KT_13 <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
              query="select * from TLM_KANTONSGEBIET", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  filter(NAME %in% c(
    "Genève", "Thurgau", "Schwyz", "Zürich", "Fribourg", "Glarus", "Appenzell Ausserrhoden",
    "Vaud", "Zug", "St. Gallen", "Schaffhausen", "Neuchâtel", "Appenzell Innerrhoden"
  ))

# Set the crs (same, but had the Z mention for TG)
st_crs(KT_13) <- 2056

# Map each canton's NAME (as used in swissBOUNDARIES3D) to the acronym used
# in the BLW payments file
canton_acronyms <- c(
  "Genève" = "GE",
  "Thurgau" = "TG",
  "Schwyz" = "SZ",
  "Zürich" = "ZH",
  "Fribourg" = "FR",
  "Glarus" = "GL",
  "Appenzell Ausserrhoden" = "AR",
  "Vaud" = "VD",
  "Zug" = "ZG",
  "St. Gallen" = "SG",
  "Schaffhausen" = "SH",
  "Neuchâtel" = "NE",
  "Appenzell Innerrhoden" = "AI"
)

#-----------------------------------------------------
# BLW payments (2024 column = 11th column)
#-----------------------------------------------------

BLW_payments <- read.csv("data/BLW_EB_payments_2015_2024.csv", sep = ";", stringsAsFactors = FALSE)

#-----------------------------------------------------
# Loop over cantons
#-----------------------------------------------------

results <- vector("list", nrow(KT_13))

for (i in seq_len(nrow(KT_13))) {

  kt_row <- KT_13[i, ]
  kt_name <- kt_row$NAME
  kt_acronym <- canton_acronyms[[kt_name]]

  message("Processing canton: ", kt_name, " (", kt_acronym, ")")

  # Get the extent of the canton 
  KT_extent_wkt <- kt_row %>%
    st_geometry() %>%
    st_as_text()

  # Load the segmented trees
  # EB_all <- st_read(trees_SWISS2_path, wkt_filter = KT_extent_wkt, quiet = TRUE) # SWISS2
  EB_all <- st_read(trees_SWISS1_path, wkt_filter = KT_extent_wkt, quiet = TRUE)
  n_total <- nrow(EB_all)

  # Filter out trees that are on forest parcels
  EB_sub <- EB_all %>%
    filter(!grepl("(^|;)901($|;)", lnf_codes))
  n_after_forest <- nrow(EB_sub)

  # Filter out trees that are too close to the TLM forest mask
  EB_sub <- EB_sub %>%
    filter(dist_to_forest > 2)
  n_after_dist_to_forest <- nrow(EB_sub)

  # Filter out trees that are on productive orchard parcels (702/703/704)
  # and have a low neighbor height z-score
  EB_sub <- EB_sub %>%
    filter(!(grepl("(^|;)(702|703|704)($|;)", lnf_codes) & !is.na(neighbor_h90_z_56m) & neighbor_h90_z_56m < 1))
  n_after_orchard <- nrow(EB_sub)

  # Filter out year that should be compared
  # payment <- BLW_payments[BLW_payments$Kantone == kt_acronym, 11] # 2024
  payment <- BLW_payments[BLW_payments$Kantone == kt_acronym, 4] # 2017

  results[[i]] <- data.frame(
    canton = kt_acronym,
    payment = payment,
    n_total = n_total,
    n_after_forest = n_after_forest,
    n_after_dist_to_forest = n_after_dist_to_forest,
    n_after_orchard = n_after_orchard
  )
}

comparison_table <- bind_rows(results)

# write.csv(comparison_table, "D:/temp/comparisonBLWPayments_SWISS2.csv", row.names = FALSE)
write.csv(comparison_table, "D:/temp/comparisonBLWPayments_SWISS1.csv", row.names = FALSE)