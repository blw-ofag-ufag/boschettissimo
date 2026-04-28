@echo off

REM Stop on error
setlocal EnableExtensions EnableDelayedExpansion

REM Arguments
set OUT_GPKG=%1
set LWN_SRC=%2
set EB_SRC=%3
set EBv_SRC=%4
set RASTER_S1=%5
set COORDS=%6
set HM_SRC=%7
set RASTER_S2=%8

for /f "tokens=1-4 delims=," %%a in (%COORDS%) do (
    set ULE=%%a
    set ULN=%%b
    set LRE=%%c
    set LRN=%%d
)

echo Creating or updating study area package: %OUT_GPKG%

REM Create a buffered bounding box (100m)
set /a XMIN=%ULE%-100
set /a YMIN=%LRN%-100
set /a XMAX=%LRE%+100
set /a YMAX=%ULN%+100

REM --- Study area polygon layer ---
ogr2ogr -update -overwrite -f GPKG "%OUT_GPKG%" ^
-nln bb_box ^
-nlt POLYGON ^
-a_srs EPSG:2056 ^
-dialect SQLite ^
-sql "SELECT 'bb_box' AS id, ST_GeomFromText('POLYGON((%XMIN% %YMIN%, %XMIN% %YMAX%, %XMAX% %YMAX%, %XMAX% %YMIN%, %XMIN% %YMIN%))', 2056) AS geom" ^
:memory:

REM --- Nutzungsflaechen ---
ogr2ogr -update -overwrite -f GPKG "%OUT_GPKG%" "%LWN_SRC%" ^
-t_srs EPSG:2056 ^
-nln nutzungsflaechen ^
-spat %XMIN% %YMIN% %XMAX% %YMAX% ^
-clipsrc %XMIN% %YMIN% %XMAX% %YMAX% ^
-nlt PROMOTE_TO_MULTI ^
-lco GEOMETRY_NAME=geom ^
-lco SPATIAL_INDEX=YES

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- Habitat Map Einzelbaum und Gebuesche ---
ogr2ogr -update -overwrite -f GPKG "%OUT_GPKG%" "%HM_SRC%" ^
-t_srs EPSG:2056 ^
-nln hm_ebug ^
-spat %XMIN% %YMIN% %XMAX% %YMAX% ^
-clipsrc %XMIN% %YMIN% %XMAX% %YMAX% ^
-nlt PROMOTE_TO_MULTI ^
-lco GEOMETRY_NAME=geom ^
-lco SPATIAL_INDEX=YES

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- swissTLM3D Einzelbaum & Gebuesche ---
ogr2ogr -update -overwrite "%OUT_GPKG%" "%EB_SRC%" tlm_bb_einzelbaum_gebuesch ^
-t_srs EPSG:2056 ^
-nln tlm_bb_einzelbaum_gebuesch ^
-spat %XMIN% %YMIN% %XMAX% %YMAX% ^
-clipsrc %XMIN% %YMIN% %XMAX% %YMAX% ^
-nlt PROMOTE_TO_MULTI ^
-lco GEOMETRY_NAME=geom ^
-lco SPATIAL_INDEX=YES

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- swissTLM3D Einzelbaum & Gebuesche vollständig ---
ogr2ogr -update -overwrite "%OUT_GPKG%" "%EBv_SRC%" ^
-t_srs EPSG:2056 ^
-nln tlm_ebv ^
-spat %XMIN% %YMIN% %XMAX% %YMAX% ^
-clipsrc %XMIN% %YMIN% %XMAX% %YMAX% ^
-nlt PROMOTE_TO_MULTI ^
-lco GEOMETRY_NAME=geom ^
-lco SPATIAL_INDEX=YES

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- VHM raster SWISS1 ---
REM Replace .gpkg with _vhm.tif
set OUT_RASTER=%OUT_GPKG:.gpkg=_vhm_S1.tif%

gdalwarp ^
-overwrite ^
-t_srs EPSG:2056 ^
-te %XMIN% %YMIN% %XMAX% %YMAX% ^
-of GTiff ^
-co COMPRESS=LZW -co TILED=YES ^
"%RASTER_S1%" "%OUT_RASTER%"

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- VHM raster in WGS84 (derived from clipped raster) ---
set OUT_RASTER_WGS84=%OUT_GPKG:.gpkg=_vhm_S1_wgs84.tif%

gdalwarp ^
-overwrite ^
-s_srs EPSG:2056 ^
-t_srs EPSG:4326 ^
-co COMPRESS=LZW -co TILED=YES ^
"%OUT_RASTER%" "%OUT_RASTER_WGS84%"

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- VHM raster SWISS2 ---
REM Replace .gpkg with _vhm.tif
set OUT_RASTER2=%OUT_GPKG:.gpkg=_vhm_S2.tif%

gdalwarp ^
-overwrite ^
-t_srs EPSG:2056 ^
-te %XMIN% %YMIN% %XMAX% %YMAX% ^
-of GTiff ^
-co COMPRESS=LZW -co TILED=YES ^
"%RASTER_S2%" "%OUT_RASTER2%"

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

REM --- VHM raster in WGS84 (derived from clipped raster) ---
set OUT_RASTER2_WGS84=%OUT_GPKG:.gpkg=_vhm_S2_wgs84.tif%

gdalwarp ^
-overwrite ^
-s_srs EPSG:2056 ^
-t_srs EPSG:4326 ^
-co COMPRESS=LZW -co TILED=YES ^
"%OUT_RASTER2%" "%OUT_RASTER2_WGS84%"

IF %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

echo Done.