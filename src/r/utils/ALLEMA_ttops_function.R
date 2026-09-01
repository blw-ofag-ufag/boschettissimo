#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

library(terra)
library(sf)
library(dplyr)
library(tidyr)

source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Load reference data
#-----------------------------------------------------

ALLEMA_EB <- st_read(
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_EB_ref.gpkg"
)

ALLEMA_EB_poly <- ALLEMA_EB |>
  st_make_valid() |>
  (\(x) x[!st_is_empty(x), ])() |>
  st_cast("POLYGON")

#-----------------------------------------------------
# Output file
#-----------------------------------------------------

out_gpkg <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_EB_lin_reg.gpkg"

if (file.exists(out_gpkg)) {
  file.remove(out_gpkg)
}

#-----------------------------------------------------
# Load VHM once
#-----------------------------------------------------

VHM <- rast(VHM_S1_path)

DHM <- rast("//katze/geolib/swissALTI3D/5m/delivery_20201221/LV95/swissALTI3D_5M_CHLV95_LN02_2020.tif")

#-----------------------------------------------------
# Process all FK_Quadrat
#-----------------------------------------------------

fk_ids <- sort(unique(ALLEMA_EB_poly$FK_Quadrat))

first_write <- TRUE

for (i in seq_along(fk_ids)) {
  
  fk <- fk_ids[i]
  
  message(
    sprintf(
      "Processing FK_Quadrat %s (%s/%s)",
      fk, i, length(fk_ids)
    )
  )
  
  poly_sub <- ALLEMA_EB_poly |>
    filter(FK_Quadrat == fk)
  
  if (nrow(poly_sub) == 0) next
  
  ext_sample <- ext(vect(poly_sub))
  VHM_sub <- crop(VHM, ext_sample)
  
  g08 <- focalMat(VHM_sub, d = 0.5, type = "Gauss")
  VHM_sub <- focal(VHM_sub, w = g08, fun = sum)
  
  DHM_sub <- crop(DHM, ext_sample)
  
  height_metrics <- terra::extract(
    VHM_sub,
    vect(poly_sub),
    fun = function(x, ...) {
      c(
        h90  = quantile(x, 0.90, na.rm = TRUE),
        h95  = quantile(x, 0.95, na.rm = TRUE),
        h99  = quantile(x, 0.99, na.rm = TRUE),
        hmax = max(x, na.rm = TRUE)
      )
    }
  )
  
  height_metrics <- height_metrics |>
    as.data.frame() |>
    select(-ID)
  
  names(height_metrics) <- c(
    "h90",
    "h95",
    "h99",
    "hmax"
  )
  
  poly_sub <- bind_cols(
    poly_sub,
    height_metrics
  )
  
  poly_sub$diameter <- 2 * sqrt(
    as.numeric(st_area(poly_sub)) / pi
  )
  
  alt <- terra::extract(DHM_sub, vect(poly_sub), mean, na.rm=TRUE)
  
  alt <- alt |>
    as.data.frame() |>
    select(-ID)
  
  names(alt) <- c(
    "alt"
  )
  
  poly_sub <- bind_cols(
    poly_sub,
    alt
  )
  
  st_write(
    poly_sub,
    out_gpkg,
    append = !first_write,
    quiet = TRUE
  )
  
  first_write <- FALSE
  
  rm(poly_sub, VHM_sub, DHM_sub, height_metrics)
  gc()
}

#-----------------------------------------------------
# Quick comparison plots
#-----------------------------------------------------

ALLEMA_EB_poly <- st_read(out_gpkg)

par(mfrow = c(2, 2))

plot(
  ALLEMA_EB_poly$h90,
  log(ALLEMA_EB_poly$diameter),
  pch = 16,
  main = "h90"
)

plot(
  ALLEMA_EB_poly$h95,
  ALLEMA_EB_poly$diameter,
  pch = 16,
  main = "h95"
)

plot(
  ALLEMA_EB_poly$h99,
  ALLEMA_EB_poly$diameter,
  pch = 16,
  main = "h99"
)

plot(
  ALLEMA_EB_poly$hmax,
  ALLEMA_EB_poly$diameter,
  pch = 16,
  main = "hmax"
)

plot(
  ALLEMA_EB_poly$alt,
  ALLEMA_EB_poly$diameter,
  pch = 16,
  main = "alt"
)

library(ggplot2)
ALLEMA_EB_poly$Gehoelztyp <- as.factor(ALLEMA_EB_poly$Gehoelztyp)
ggplot(ALLEMA_EB_poly, aes(x = hmax, y = diameter, color = Gehoelztyp)) +
geom_point() +
labs(title = "hmax")


library(ggplot2)
library(ggpubr)

# Make sure the grouping variable is a factor with readable labels
ALLEMA_EB_poly$Gehoelztyp <- factor(
  ALLEMA_EB_poly$Gehoelztyp,
  levels = c(38, 59),
  labels = c("Obstbäume", "andere Einzelbäume")
)

# A colorblind-friendly, print-friendly palette
group_colors <- c("Obstbäume" = "#D55E00", "andere Einzelbäume" = "#0072B2")

p <- ggplot(
  ALLEMA_EB_poly,
  aes(x = hmax, y = diameter, color = Gehoelztyp, fill = Gehoelztyp)
) +
  geom_point(size = 1.6, alpha = 0.65, shape = 16) +
  geom_smooth(
    method = "lm", se = TRUE, alpha = 0.45, linewidth = 1.1
  ) +
  stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x.npc = 0.05,
    label.y.npc = c(0.97, 0.90),
    size = 4.5,
    show.legend = FALSE
  ) +
  stat_cor(
    aes(label = after_stat(rr.label)),
    label.x.npc = 0.05,
    label.y.npc = c(0.92, 0.85),
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Kronenhöhe vs. Kronendurchmesser",
    subtitle = "Vergleich zweier Gehölztypen (VHM-basierte Kronenmaxima)",
    x = "Maximale Vegetationshöhe in der Krone (hmax) [m]",
    y = "Kronendurchmesser [m]",
    color = "Gehölztyp",
    fill = "Gehölztyp"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "grey30", size = 13),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "grey50")
  )

print(p)

#-----------------------------------------------------
# Linear models
#-----------------------------------------------------

mmax_gh  <- lm(diameter ~ h99 + Gehoelztyp,  data = ALLEMA_EB_poly)

m90  <- lm(diameter ~ h90 + alt,  data = ALLEMA_EB_poly)
m95  <- lm(diameter ~ h95,  data = ALLEMA_EB_poly)
m99  <- lm(diameter ~ h99,  data = ALLEMA_EB_poly)
mmax <- lm(diameter ~ hmax + alt, data = ALLEMA_EB_poly)

# Test based on Popescu 2004 results. Doesn't work as good as simple h99.
mmax <- lm(diameter ~ I(h99^2), data = ALLEMA_EB_poly)

summary(m90)
summary(m95)
summary(m99)
summary(mmax)

m90_OB  <- lm(diameter ~ h99,  data = ALLEMA_EB_poly[which(ALLEMA_EB_poly$Gehoelztyp == 38),])
summary(m90_OB)

par(mfrow = c(2, 2))
plot(m90_OB)

m90_EB  <- lm(diameter ~ h99,  data = ALLEMA_EB_poly[which(ALLEMA_EB_poly$Gehoelztyp == 59),])
summary(m90_EB)


test_f <- function(h) {
  ifelse(h < 4, 3, 3 + 0.7*h)
}

ALLEMA_EB_poly$test <- test_f(ALLEMA_EB_poly$h90)
cor(ALLEMA_EB_poly$test, ALLEMA_EB_poly$diameter)

test_f2 <- function(h, a) {
  # ifelse(h < 4, 3, 3.5 + 0.7*h)
  # 1.464333 + 0.362441*h
  3.322 + 0.3443*h - 0.00113*a
}

ALLEMA_EB_poly$test2 <- test_f2(ALLEMA_EB_poly$h90, ALLEMA_EB_poly$alt)
cor(ALLEMA_EB_poly$test2, ALLEMA_EB_poly$diameter, use="complete.obs")
sum(sqrt((ALLEMA_EB_poly$diameter-ALLEMA_EB_poly$test2)^2), na.rm=T)

par(mfrow = c(2, 2))

selec_fks <- c(689214, 581150, 563190, 533198)

for(k in seq_along(selec_fks)) {
  
  allema_sub <- ALLEMA_EB_poly[
    ALLEMA_EB_poly$FK_Quadrat == selec_fks[k], ]
  
  m90 <- lm(diameter ~ h90, data = allema_sub)
  
  # Extract coefficients and adjusted R²
  coefs <- coef(m90)
  adj_r2 <- summary(m90)$adj.r.squared
  
  eq_text <- paste0(
    "y = ",
    round(coefs[1], 2),
    ifelse(coefs[2] >= 0, " + ", " - "),
    round(abs(coefs[2]), 2),
    "x"
  )
  
  r2_text <- paste0("Adj. R² = ", round(adj_r2, 3))
  
  plot(
    allema_sub$h90,
    allema_sub$diameter,
    pch = 16,
    main = paste("FK", selec_fks[k]),
    xlab = "h90",
    ylab = "diameter"
  )
  
  # Add fitted line
  abline(m90, col = "red", lwd = 2)
  
  # Add equation and adjusted R²
  legend(
    "topleft",
    legend = c(eq_text, r2_text),
    bty = "n"
  )
}

#-----------------------------------------------------
# Check regression model for Nadel / Laub differentiation for crown diameter
#-----------------------------------------------------

# Load Nadel Laub dataset done by Christian
NadelLaub <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/ALLEM_EB_ref_ReturnALS_VHM.gpkg")
#* COMMENT: Weird, i cannot plot the polygons, neither in R nor in QGIS...

# Fetch previously computed h90 etc values
ALLEMA_EB_poly <- st_read(out_gpkg)

# Join back to original ALLEMA object to be able to do the regression 
NadelLaub_ALLEMA <- inner_join(
  ALLEMA_EB_poly[,c("FK_Quadrat","OBJECT_ID","h90","h99","hmax","diameter","geom")],
  st_drop_geometry(NadelLaub[,c("FK_Quadrat","OBJECT_ID","evergreen","MEAN_RETURN", "evergreen_ALS")]),
  by = c("FK_Quadrat","OBJECT_ID")
)


library(ggplot2)
library(ggpubr)

# Make sure the grouping variable is a factor with readable labels
NadelLaub_ALLEMA$evergreen <- factor(
  NadelLaub_ALLEMA$evergreen,
  levels = c(0,1,9),
  labels = c("Laub", "Nadel", "NA")
)

# A colorblind-friendly, print-friendly palette
group_colors <- c("Laub" = "#90ee90", "Nadel" = "#013220", "NA"="#999999")

p <- ggplot(
  NadelLaub_ALLEMA[which(NadelLaub_ALLEMA$evergreen != "NA"),],
  aes(x = hmax, y = diameter, color = evergreen, fill = evergreen)
) +
  geom_point(size = 1.6, alpha = 0.65, shape = 16) +
  geom_smooth(
    method = "lm", se = FALSE, linewidth = 1.1
  ) +
  stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x.npc = 0.05,
    label.y.npc = c(0.97, 0.85, 0.73),
    size = 4.5,
    show.legend = FALSE
  ) +
  stat_cor(
    aes(label = after_stat(rr.label)),
    label.x.npc = 0.05,
    label.y.npc = c(0.92, 0.80, 0.68),
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Kronenhöhe vs. Kronendurchmesser",
    subtitle = "Vergleich Laub/Nadel (VHM-basierte Kronenmaxima)",
    x = "Maximale Vegetationshöhe in der Krone (hmax) [m]",
    y = "Kronendurchmesser [m]",
    color = "Laub / Nadel",
    fill = "Laub / Nadel"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "grey30", size = 13),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "grey50")
  )

print(p)


mLaub  <- lm(diameter ~ h99,  
             data = NadelLaub_ALLEMA[which(NadelLaub_ALLEMA$evergreen == "Laub"),])
summary(mLaub)

mNadel <- lm(diameter ~ h99,  
             data = NadelLaub_ALLEMA[which(NadelLaub_ALLEMA$evergreen == "Nadel"),])
summary(mNadel)

mh99 <- lm(diameter ~ h99,  
           data = NadelLaub_ALLEMA)
summary(mh99)

#-----------------------------------------------------
# Check regression model for Nadel / Laub differentiation
#-----------------------------------------------------

# Join back to original ALLEMA object to be able to do the regression 
NL_ALLEMA <- inner_join(
  ALLEMA_EB_poly[,c("FK_Quadrat","OBJECT_ID","h90","h99","hmax","diameter","geom")],
  st_drop_geometry(NadelLaub[,c("FK_Quadrat","OBJECT_ID","evergreen","MEAN_RETURN", "evergreen_ALS")]),
  by = c("FK_Quadrat","OBJECT_ID")
)

# Clean out all rows that contain NA values
NL_ALLEMA <- NL_ALLEMA[which(NL_ALLEMA$evergreen != "NA"),]
NL_ALLEMA <- NL_ALLEMA[which(NL_ALLEMA$evergreen != 9),]
NL_ALLEMA <- NL_ALLEMA[which(!is.na(NL_ALLEMA$MEAN_RETURN)),]
NL_ALLEMA <- NL_ALLEMA[which(!is.na(NL_ALLEMA$evergreen_ALS)),]

# Set the factors
NL_ALLEMA$evergreen  <- factor(
  NL_ALLEMA$evergreen,
  levels = c(0,1),
  labels = c("Laub", "Nadel")
)

mFunkGroup <- glm(evergreen ~ MEAN_RETURN + evergreen_ALS,
                  data = NL_ALLEMA,
                  family = binomial(link = "logit"))
summary(mFunkGroup)


m_mr   <- glm(evergreen ~ MEAN_RETURN,    data = NL_ALLEMA, family = binomial(link = "logit"))
m_als  <- glm(evergreen ~ evergreen_ALS,  data = NL_ALLEMA, family = binomial(link = "logit"))
m_both <- glm(evergreen ~ MEAN_RETURN + evergreen_ALS, data = NL_ALLEMA, family = binomial(link = "logit"))

anova(m_mr, m_both, test = "Chisq")   # does adding evergreen_ALS help significantly?
anova(m_als, m_both, test = "Chisq")  # does adding MEAN_RETURN help significantly?
AIC(m_mr, m_als, m_both)              # lower AIC = better tradeoff of fit vs complexity

NL_ALLEMA$pct_Laub <- predict(m_both, type = "response") * 100


library(pROC)
roc_obj <- roc(NL_ALLEMA$evergreen, NL_ALLEMA$pct_Laub)
auc(roc_obj)
plot(roc_obj)

# Confusion matrix at 50% cutoff
pred_class <- ifelse(NL_ALLEMA$pct_Laub > 50, "Laub", "Nadel")
table(Predicted = pred_class, Actual = NL_ALLEMA$evergreen)
mean(pred_class == NL_ALLEMA$evergreen, na.rm = TRUE)

#-----------------------------------------------------
# DO regression model for ALLEMA outside of processed 14 cantons
#-----------------------------------------------------

# Load the canton
KT <- st_read("//katze/geolib/swissBOUNDARIES3D/2024/fgdb/swissBOUNDARIES3D_1_5_LV95_LN02.gdb",
              query="select * from TLM_KANTONSGEBIET") %>%
  st_zm(drop = TRUE, what = "ZM")

# Keep only the 13 cantons that have the newest lidar aquisition (stand 30.07.2026)
KT <- KT[which(KT$NAME %in% c("Genève", "Thurgau", "Schwyz", "Zürich", "Fribourg", "Glarus", "Appenzell Ausserrhoden",
  "Vaud", "Zug", "St. Gallen", "Schaffhausen", "Neuchâtel", "Appenzell Innerrhoden" 
)),]

# Set the crs (same, but had the Z mention for TG)
st_crs(KT) <- st_crs(ALLEMA_EB_poly)

# Keep only non-intersecting polygons
ALLEMA_EB_poly_sub <- ALLEMA_EB_poly[lengths(st_intersects(ALLEMA_EB_poly, KT)) == 0, ]


ALLEMA_EB_poly_sub$Gehoelztyp <- as.factor(ALLEMA_EB_poly_sub$Gehoelztyp)

library(ggplot2)
library(ggpubr)

# Make sure the grouping variable is a factor with readable labels
ALLEMA_EB_poly_sub$Gehoelztyp <- factor(
  ALLEMA_EB_poly_sub$Gehoelztyp,
  levels = c(38, 59),
  labels = c("Obstbäume", "andere Einzelbäume")
)

# A colorblind-friendly, print-friendly palette
group_colors <- c("Obstbäume" = "#D55E00", "andere Einzelbäume" = "#0072B2")

p <- ggplot(
  ALLEMA_EB_poly_sub,
  aes(x = hmax, y = diameter, color = Gehoelztyp, fill = Gehoelztyp)
) +
  geom_point(size = 1.6, alpha = 0.65, shape = 16) +
  geom_smooth(
    method = "lm", se = TRUE, alpha = 0.45, linewidth = 1.1
  ) +
  stat_regline_equation(
    aes(label = after_stat(eq.label)),
    label.x.npc = 0.05,
    label.y.npc = c(0.97, 0.90),
    size = 4.5,
    show.legend = FALSE
  ) +
  stat_cor(
    aes(label = after_stat(rr.label)),
    label.x.npc = 0.05,
    label.y.npc = c(0.92, 0.85),
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Kronenhöhe vs. Kronendurchmesser",
    subtitle = "Vergleich zweier Gehölztypen (VHM-basierte Kronenmaxima)",
    x = "Maximale Vegetationshöhe in der Krone (hmax) [m]",
    y = "Kronendurchmesser [m]",
    color = "Gehölztyp",
    fill = "Gehölztyp"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "grey30", size = 13),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "grey50")
  )

print(p)
