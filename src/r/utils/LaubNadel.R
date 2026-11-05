#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

# Libraries
library(sf)
library(dplyr)
library(mapview)

# Sourcing initialization code (paths and such)
source("src/r/001_Initialization.R")

#-----------------------------------------------------
# Loading necessary dataset
#-----------------------------------------------------

# Load the evergreen dataset prepared by Christian
ALLEMA_evergreen <- st_read("//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ORIG_DATA/ALLEMA/ALLEM_EB_ref_ReturnALS_VHM.gpkg")

# Assign EPSG:2056 to make the geometries work (workaround needed because of arcgis)
st_crs(ALLEMA_evergreen) <- 2056
st_write(ALLEMA_evergreen, "D:/temp/test_return2.gpkg")

# Have an overview of what was classified
table(ALLEMA_evergreen[,c("FK_Quadrat","evergreen")]%>%st_drop_geometry())
#           evergreen
# FK_Quadrat    0    1    9
#     503158   93  118    0
#     533182   16    0    2
#     533198  162  326    0
#     563110    0    0  414
#     563190   89    0    0
#     575110  521   66    0
#     575126   10  796    0
#     581086  113  320    0
#     581150   60   89    0
#     587110    0  337    0
#     605118    0  296    0
#     629118    0    0  509
#     635262  342    0    0
#     641102    0  854    0
#     641134    0    0  839
#     641182   12  386    0
#     647126    3  110    0
#     653134    0    0  981
#     653198    1  129    0
#     659206  368   78    0
#     683150    0    0 2983
#     683270  131   13    0
#     689142    0    0 3380
#     689214  933   49    0
#     689230  222   11    0
#     695110    0    0  338
#     695222  807  104    0
#     695238  445    9    0
#     695270   71   11    0
#     701198    0    0 2508
#     701262  127   10    0
#     707110  214   24    0
#     707254   99    4    0
#     713102  230   17    0
#     713158    0    0 1158
#     713278  114    0    0
#     719126   82    8    0
#     719174    0    0  622
#     719238   46   28    0
#     719254   28    0    0
#     725102    0    0  179
#     725110    0    0   89
#     725246  179   22    0
#     731174    0    0  550
#     731222    0    0  172
#     731270  350    0    0
#     743158    0    0  137
#     749166    0    0  393
#     749246    0    0   91
#     755254    0    0   96
#     761134    0    0  584
#     761182    0    0  381
#     773198    0    0  433
#     779174    0    0 2002
#     785150    0    0 3650
#     785198    0    0    5

mapview(ALLEMA_evergreen[which(ALLEMA_evergreen$FK_Quadrat == 725246),], zcol="evergreen")

test <- lm( evergreen ~ STD_RETURN, data = ALLEMA_evergreen)
summary(test)

# Remove the ALLEMA which were not classified
ALLEMA_test <- ALLEMA_evergreen[which(ALLEMA_evergreen$evergreen %in% c(0,1)),]
test <- lm( evergreen ~ PCT90_VHM, data = ALLEMA_test)
summary(test)

# This is an example where a lot more laub than nadel, but at least every thing seems correct... And still doesn't work...
test <- lm( evergreen ~ STD_RETURN, data = ALLEMA_evergreen[which(ALLEMA_evergreen$FK_Quadrat == 689214),])
summary(test)
