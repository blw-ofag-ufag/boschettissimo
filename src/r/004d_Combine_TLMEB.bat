@echo off

REM ==========================================
REM Paths
REM ==========================================

set OUTFILE=D:\BOSCHETTISSIMO\ORIG_DATA\TLM_EB\combined_tlm_eb.gpkg
set INPUTDIR=\\speedy12-37\data_25\Vegetationmaxima\07_WSL\05_Nordostschweiz
set CLIPFILE=D:\BOSCHETTISSIMO\ORIG_DATA\TG.gpkg

REM Delete old geopackage if it exists
if exist "%OUTFILE%" del "%OUTFILE%"

REM ==========================================
REM Create a geopackage for the canton TG
REM ==========================================
ogr2ogr -f GPKG -overwrite %CLIPFILE% "\\katze\geolib\swissBOUNDARIES3D\2024\fgdb\swissBOUNDARIES3D_1_5_LV95_LN02.gdb" TLM_KANTONSGEBIET -where "NAME = 'Thurgau'"

REM ==========================================
REM First layer (creates the GeoPackage)
REM ==========================================
echo Layer 1
ogr2ogr -f GPKG "%OUTFILE%" "%INPUTDIR%\1093_EB_Vorbereitungsdaten_2024.gdb" LK1093_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

REM ==========================================
REM Append remaining layers
REM ==========================================

echo Layer 2
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1073_EB_Vorbereitungsdaten_2024.gdb" LK1073_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 3
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1074_EB_Vorbereitungsdaten_2024.gdb" LK1074_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 4
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1075_EB_Vorbereitungsdaten_2024.gdb" LK1075_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 5
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1055_EB_Vorbereitungsdaten_2024.gdb" LK1055_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 6
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1054_EB_Vorbereitungsdaten_2024.gdb" LK1054_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 7
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1053_EB_Vorbereitungsdaten_2024.gdb" LK1053_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 8
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1034_EB_Vorbereitungsdaten_2024.gdb" LK1034_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 9
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1033_EB_Vorbereitungsdaten_2024.gdb" LK1033_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 10
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1052_EB_Vorbereitungsdaten_2024.gdb" LK1052_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Layer 11
ogr2ogr -f GPKG -update -append "%OUTFILE%" "%INPUTDIR%\1032_EB_Vorbereitungsdaten_2024.gdb" LK1032_LokalMax3D_LIDAR_5m -nln LokalMax3D_LIDAR_5m -clipsrc "%CLIPFILE%"

echo Creating spatial index...
ogrinfo "%OUTFILE%" -sql "SELECT CreateSpatialIndex('LokalMax3D_LIDAR_5m', 'Shape')"

echo Cleaning up
ogrinfo "%OUTFILE%" -sql "VACUUM"


echo Done!
pause