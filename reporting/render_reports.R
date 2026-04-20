#--------------------------------------------------------
# REPORT R001_EB
#--------------------------------------------------------
  
# Render the file
quarto::quarto_render("R001_EB.qmd")

# Move the report to speedy
file.copy(
  "R001_EB.html",
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/02_Reports/R001_EB.html",
  overwrite = TRUE
)
file.copy(
  "R001_EB_files",
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/02_Reports/",
  overwrite = TRUE,
  recursive = TRUE
)
