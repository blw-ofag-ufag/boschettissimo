#-----------------------------------------------------
# VHM preparation - Gaussian smoothing
#-----------------------------------------------------
vhm_cell_prep <- function(arg_e_buf, arg_VHM_S2_path) {

  # Create temp file
  tmpfile <- tempfile("vhm_crop_")

  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(arg_e_buf), ymax(arg_e_buf), xmax(arg_e_buf), ymin(arg_e_buf),
    arg_VHM_S2_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Read cell VHM from temp file
  vhm_cell <- rast(paste0(tmpfile, ".vrt"))

  # Load the raster into memory (needed for lidr processing steps)
  vhm_cell <- toMemory(vhm_cell)

  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))

  # Smooth vhm
  g05 <- focalMat(vhm_cell, d = 0.5, type = "Gauss")
  vhm_cell <- focal(vhm_cell, w = g05, fun = sum)

  return(vhm_cell)

}

#-----------------------------------------------------
# Topography preparation
#-----------------------------------------------------
dem_cell_prep <- function(arg_e_buf, arg_dem_path) {

  # Create temp file
  tmpfile <- tempfile("dem_crop_")

  # Use gdal for faster raster cropping
  cmd <- sprintf(
    'gdal_translate -projwin %.2f %.2f %.2f %.2f -of VRT "%s" "%s.vrt"',
    xmin(arg_e_buf), ymax(arg_e_buf), xmax(arg_e_buf), ymin(arg_e_buf),
    arg_dem_path,
    tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Read cell DEM from temp file
  dem_cell <- rast(paste0(tmpfile, ".vrt"))

  # Load the raster into memory
  dem_cell <- toMemory(dem_cell)

  # Erase temp file
  unlink(paste0(tmpfile, ".vrt"))

  # Derive slope and aspect from the DEM
  topo_cell <- terrain(dem_cell, v = c("slope", "aspect"), unit = "degrees")

  # Combine altitude, slope and aspect into a single raster stack
  names(dem_cell) <- "altitude"
  c(dem_cell, topo_cell)

}
