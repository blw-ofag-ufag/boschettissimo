# Load libraries
library(sf)
library(leaflet)
library(leafgl)
library(terra)

# Preparation of the maps shown in R001_EB.qmd
#---------------------------------------------
tlm_eb_maps <- lapply(names(sa_vec), function(nm) {
  
  # Get study area bounding box
  layer_bbox <- st_read(sa_vec[[nm]]$path, layer = "bb_box", quiet = TRUE) %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON")
  
  # Get the tlm eb points
  layer_tlm_eb <- st_read(sa_vec[[nm]]$path, layer = "tlm_bb_einzelbaum_gebuesch", quiet = TRUE) %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    st_cast("MULTIPOINT") %>%
    st_cast("POINT")
  
  # Get the tlm eb vollstaendig points
  layer_tlm_ebv <- st_read(sa_vec[[nm]]$path, layer = "tlm_ebv", quiet = TRUE) %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    st_cast("MULTIPOINT") %>%
    st_cast("POINT")
  
  # Update the statistics of the layer
  # Get the habitat map EB&G layer
  layer_hm_ebug <- st_read(sa_vec[[nm]]$path, layer = "hm_ebug", quiet = TRUE) %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON")
  
  # Get the tree crown layer
  layer_crowns <- st_read(sa_vec[[nm]]$path, layer = "crowns", quiet = TRUE) %>% 
    st_transform(crs = "+proj=longlat +datum=WGS84") %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON")
  
  # Update the statistics of the vhm layer
  setMinMax(sa_tif_S1[[nm]])
  setMinMax(sa_tif_S2[[nm]])
  
  # Aggregate the vegetation height models (to be able to display it easier in interactive map)
  r_wgs_S1 <- aggregate(sa_tif_S1[[nm]], fun = "mean", na.rm = TRUE, fact = 4)
  r_wgs_S2 <- aggregate(sa_tif_S2[[nm]], fun = "mean", na.rm = TRUE, fact = 4)
  
  # Create a palette for the VHMs
  vals <- c(
    values(r_wgs_S1),
    values(r_wgs_S2)
  )
  vals <- vals[!is.na(vals)]
  pal <- colorNumeric("viridis", vals, na.color = "transparent")
  
  # Compose the map
  m <- leaflet() %>%
    addPolygons(
      data = layer_bbox,
      fillColor = "cyan",
      fillOpacity = 0,
      color = "cyan",
      weight = 2
    ) %>%
    addGlPolygons(
      data = layer_hm_ebug,
      color = "purple",
      group = "Habitat Map - Einzelbaum & Gebuesche"
    ) %>%
    addGlPolygons(
      data = layer_crowns,
      color = "pink",
      group = "NEW - Segmentierte Baume (watershed)"
    ) %>%
    addCircleMarkers(
      data = layer_tlm_eb,
      radius = 1,
      color = "red",
      fillOpacity = 0.5,
      group = "TLM - Einzelbaum & Gebuesch"
    )  %>%
    addCircleMarkers(
      data = layer_tlm_ebv,
      radius = 1,
      color = "gold",
      fillOpacity = 0.5,
      group = "TLM - Einzelbaum & Gebuesch (raw data)"
    )  %>%
    addWMSTiles(
      "https://wmts10.geo.admin.ch/1.0.0/ch.swisstopo.swissimage-product/default/current/3857/{z}/{x}/{y}.jpeg",
      layers = "swissimage-product",
      group = "Aerial imagery - swisstopo",
      layerId = "swissimage-product",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      attribution = "swisstopo"
    )  %>%
    addWMSTiles(
      "https://wmts10.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
      layers = "pixelkarte-farbe",
      group = "Location map color - swisstopo",
      layerId = "pixelkarte-farbe",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      attribution = "swisstopo"
    ) %>%
    addRasterImage(
      r_wgs_S1,
      colors = pal,
      opacity = 0.8,
      group = "VHM_SWISS1"
    ) %>%
    addRasterImage(
      r_wgs_S2,
      colors = pal,
      opacity = 0.8,
      group = "VHM_SWISS2"
    ) %>%
    addLayersControl(
      baseGroups = c("Aerial imagery - swisstopo", "Location map color - swisstopo"),
      overlayGroups = c("TLM - Einzelbaum & Gebuesch", "TLM - Einzelbaum & Gebuesch (raw data)", "Habitat Map - Einzelbaum & Gebuesche","NEW - Segmentierte Baume (watershed)","VHM_SWISS1","VHM_SWISS2"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addLegend(
      pal = pal,
      values = vals,
      title = "VHM"
    )

  
  return(m)
})

# Put the study areas as names of the maps
names(tlm_eb_maps) <- names(sa_vec)

