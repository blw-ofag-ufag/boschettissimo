#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(terra)
library(sf)
library(dplyr)

# Sourcing initialization code (paths and such)
source("../001_Initialization.R") # source("src/r/001_Initialization.R")

# Load the cantons
KT <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
              query="select * from TLM_KANTONSGEBIET") %>%
  st_zm(drop = TRUE, what = "ZM")

# Add the canton abbreviation to KT object (ex KT$NAME == Genève --> KT$ACCRO = GE)
# (swissBOUNDARIES3D has no abbreviation field, so we map it from the official BFS canton number)
canton_accro <- c(
  "1" = "ZH", "2" = "BE", "3" = "LU", "4" = "UR", "5" = "SZ", "6" = "OW",
  "7" = "NW", "8" = "GL", "9" = "ZG", "10" = "FR", "11" = "SO", "12" = "BS",
  "13" = "BL", "14" = "SH", "15" = "AR", "16" = "AI", "17" = "SG", "18" = "GR",
  "19" = "AG", "20" = "TG", "21" = "TI", "22" = "VD", "23" = "VS", "24" = "NE",
  "25" = "GE", "26" = "JU"
)
KT$ACCRO <- canton_accro[as.character(KT$KANTONSNUMMER)]

# Output folder for the parcel snapshots
out_path <- "./Waldweiden"
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

# Loop through the cantons
for(i in 1:nrow(KT)){

  accro <- KT$ACCRO[i]

  # Select 10 random Waldweiden parcels in the canton
  # (the FileGDB driver doesn't support ORDER BY RANDOM(), even with dialect = "SQLite",
  # so all matching parcels are read first and then randomly sampled in R)
  LN_all <- st_read(LN_2025_path,
                     query = paste0(
                       "SELECT * FROM Landwirtschaftliche_Nutzungsflaechen_Schweiz_2025 ln WHERE ln.kanton = '",accro,"' AND ln.lnf_code = 618"),
                     quiet = TRUE)

  if(nrow(LN_all) == 0) next

  LN_sub <- LN_all[sample(nrow(LN_all), min(10, nrow(LN_all))), ]

  # Loop through the 10 (or less if not available) randomly selected parcels
  for(j in 1:nrow(LN_sub)){

    parcel <- LN_sub[j, ]

    # Buffer the parcel extent so it isn't cropped tight against the image edge,
    # and keep it square so it matches the WIDTH/HEIGHT of the WMS request below
    bbox <- st_bbox(parcel)
    cx <- mean(c(bbox["xmin"], bbox["xmax"]))
    cy <- mean(c(bbox["ymin"], bbox["ymax"]))
    side <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]) * 1.3
    bbox <- c(xmin = cx - side / 2, ymin = cy - side / 2, xmax = cx + side / 2, ymax = cy + side / 2)

    # Fetch a SWISSIMAGE aerial background for that extent from the swisstopo WMS
    img_path <- tempfile(fileext = ".jpg")
    wms_url <- paste0(
      "https://wms.geo.admin.ch/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap",
      "&LAYERS=ch.swisstopo.swissimage&STYLES=default&CRS=EPSG:2056",
      "&BBOX=", bbox["xmin"], ",", bbox["ymin"], ",", bbox["xmax"], ",", bbox["ymax"],
      "&WIDTH=800&HEIGHT=800&FORMAT=image/jpeg"
    )
    download.file(wms_url, destfile = img_path, mode = "wb", quiet = TRUE)

    background <- rast(img_path)
    ext(background) <- ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
    crs(background) <- "EPSG:2056"

    # Write it out to ./Waldweiden with naming KT_1, KT_2, etc
    png(file.path(out_path, paste0(accro, "_", j, ".png")), width = 800, height = 800)
    plotRGB(flip(background, direction="vertical"))
    plot(st_geometry(parcel), border = "cyan", lwd = 3, add = TRUE)

    # Info box (image center coordinates, betriebsnummer, nutzungsidentifikator)
    info_text <- c(
      sprintf("Center (LV95): %.0f, %.0f", cx, cy),
      sprintf("Betriebsnr: %s", parcel$betriebsnummer),
      sprintf("NutzungsID: %s", parcel$nutzungsidentifikator)
    )

    box_x <- grconvertX(c(0.02, 0.62), from = "ndc", to = "user")
    box_y <- grconvertY(c(0.02, 0.22), from = "ndc", to = "user")
    rect(box_x[1], box_y[1], box_x[2], box_y[2], col = adjustcolor("black", alpha.f = 0.55), border = NA)

    text(
      x = grconvertX(0.03, from = "ndc", to = "user"),
      y = grconvertY(seq(0.19, 0.05, length.out = length(info_text)), from = "ndc", to = "user"),
      labels = info_text,
      col = "white", adj = c(0, 0.5), cex = 1
    )

    dev.off()
  }

}


# Get a full idea of the number of Waldweiden per canton
LN_all <- st_read(LN_2025_path,
                     query = paste0(
                       "SELECT * FROM Landwirtschaftliche_Nutzungsflaechen_Schweiz_2025 ln WHERE ln.lnf_code = 618"),
                     quiet = TRUE)

table(LN_all$kanton)
  # AG   AR   BE   BL   FR   GR   JU   LU   NE   NW   OW   SG   SH   SO   TI   UR   VD   VS   ZG   ZH 
  #  2    1  719    8    3  186  325    1 1811   11   19  165    9    9  694   32  177  296   48   11 