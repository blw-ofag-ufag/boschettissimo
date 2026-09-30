#-----------------------------------------------------
# Segmentation
#-----------------------------------------------------
segment_cell <- function(arg_vhm) {

  # Define the function that should be used to find tree tops
  find_ttops <- function(h) {
    pmin(3.5 + 0.7*h, 15)
  }

  # Identify the tree tops
  ttops <- lidR::locate_trees(arg_vhm, lmf(find_ttops, shape="circular", hmin = 1.5)) %>%
    st_zm(drop = TRUE, what = "ZM")

  # If no tree tops detected skip this iteration
  if (is.null(ttops) || nrow(ttops) == 0) {
    return(NULL)
  }

  # Get the watershed crowns
  crowns <- ForestTools::mcws(
    treetops = ttops,
    CHM = arg_vhm,
    minHeight = 1.5,
    format = "polygons"
  )

  # If no detected crown skip this iteration
  if (is.null(crowns) || nrow(crowns) == 0) {
    return(NULL)
  }

  return(crowns)
}
