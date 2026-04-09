# Load libraries
library(sf)
library(leaflet)
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
  
  # Update the statistics of the layer
  setMinMax(sa_tif[[nm]])
  
  # If VHM is available, prepare raster layer
  if(!is.nan(minmax(sa_tif[[nm]])[1])){
    
    # Aggregate the vegetation height model (to be able to display it easier in interactive map)
    r_wgs <- aggregate(sa_tif[[nm]], fun = "mean", na.rm = TRUE, fact = 4)
    
    # Create a palette for the VHM
    pal <- colorNumeric("viridis", values(r_wgs), na.color = "transparent")
  }
  
  # Compose the map
  m <- leaflet() %>%
    addPolygons(
      data = layer_bbox,
      fillColor = "cyan",
      fillOpacity = 0,
      color = "cyan",
      weight = 2
    ) %>%
    addCircleMarkers(
      data = layer_tlm_eb,
      radius = 1,
      color = "red",
      fillOpacity = 0.5,
      group = "tlm_bb_einzelbaum_gebuesch"
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
    ) 
  
  if(!is.nan(minmax(sa_tif[[nm]])[1])){
    m <- m %>%
      addRasterImage(
        r_wgs,
        colors = pal,
        opacity = 0.8,
        group = "vhm"
      ) %>%
      addLayersControl(
        baseGroups = c("Location map color - swisstopo", "Aerial imagery - swisstopo"),
        overlayGroups = c("tlm_bb_einzelbaum_gebuesch","vhm"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = values(r_wgs),
        title = "VHM"
      )
  } else {
    m <- m %>%
      addLayersControl(
        baseGroups = c("Location map color - swisstopo", "Aerial imagery - swisstopo"),
        overlayGroups = c("tlm_bb_einzelbaum_gebuesch"),
        options = layersControlOptions(collapsed = TRUE)
      )
  }
  
  return(m)
})

names(tlm_eb_maps) <- names(sa_vec)
