@echo off

REM Stop on error
setlocal EnableExtensions EnableDelayedExpansion

REM Arguments
set OUT_GPKG=%1
set INPUT1=%2
set INPUT2=%3
set RASTER=%4
set ULE=%5
set ULN=%6
set LRE=%7
set LRN=%8

echo Creating study area package: %OUT_GPKG%

REM Create a buffered bounding box (100m)
set /a XMIN=%ULE%-100
set /a YMIN=%LRN%-100
set /a XMAX=%LRE%+100
set /a YMAX=%ULN%+100

REM --- Make sure to remove previous package ---
IF EXIST "%OUT_GPKG%" DEL "%OUT_GPKG%"

REM --- Study area polygon layer ---
ogr2ogr -overwrite -f GPKG "%OUT_GPKG%" ^
-nln bb_box ^
-nlt POLYGON ^
-a_srs EPSG:2056 ^
-dialect SQLite ^
-sql "SELECT 'bb_box' AS id, ST_GeomFromText('POLYGON((%XMIN% %YMIN%, %XMIN% %YMAX%, %XMAX% %YMAX%, %XMAX% %YMIN%, %XMIN% %YMIN%))', 2056) AS geom" ^
:memory:

REM --- Nutzungsflaechen ---
ogr2ogr -update -append -f GPKG "%OUT_GPKG%" "%INPUT1%" ^
-t_srs EPSG:2056 ^
-nln nutzungsflaechen ^
-spat %XMIN% %YMIN% %XMAX% %YMAX% ^
-clipsrc %XMIN% %YMIN% %XMAX% %YMAX% ^
-nlt PROMOTE_TO_MULTI ^
-lco GEOMETRY_NAME=geom ^
-lco SPATIAL_INDEX=YES

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- swissTLM3D Einzelbaum & Gebuesche ---
ogr2ogr -update -append "%OUT_GPKG%" "%INPUT2%" tlm_bb_einzelbaum_gebuesch ^
-t_srs EPSG:2056 ^
-nln tlm_bb_einzelbaum_gebuesch ^
-spat %XMIN% %YMIN% %XMAX% %YMAX% ^
-clipsrc %XMIN% %YMIN% %XMAX% %YMAX% ^
-nlt PROMOTE_TO_MULTI ^
-lco GEOMETRY_NAME=geom ^
-lco SPATIAL_INDEX=YES

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- VHM raster ---
REM Replace .gpkg with _vhm.tif
set OUT_RASTER=%OUT_GPKG:.gpkg=_vhm.tif%

gdalwarp ^
-t_srs EPSG:2056 ^
-te %XMIN% %YMIN% %XMAX% %YMAX% ^
-of GTiff ^
-co COMPRESS=LZW -co TILED=YES ^
"%RASTER%" "%OUT_RASTER%"

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

echo Done.