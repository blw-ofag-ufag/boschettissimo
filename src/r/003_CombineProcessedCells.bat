@echo off
setlocal enabledelayedexpansion
 
REM === ANPASSEN ===
set "ROOT=D:\BOSCHETTISSIMO\PROCESSED_DATA\TREE_SEG\"
set "OUT=D:\BOSCHETTISSIMO\PROCESSED_DATA\TREE_SEG_merged.gpkg"
set "LAYER=crowns"
 
REM Falls Output schon existiert, löschen
if exist "%OUT%" del "%OUT%"
 
set FIRST=1
 
for %%F in ("%ROOT%\*.gpkg") do (
    echo Verarbeite: %%F
 
    if !FIRST!==1 (
        ogr2ogr -f GPKG "%OUT%" "%%F" "%LAYER%" -nln "%LAYER%"
        set FIRST=0
    ) else (
        ogr2ogr -f GPKG -update -append "%OUT%" "%%F" "%LAYER%" -nln "%LAYER%"
    )
)
 
echo Fertig: %OUT%
pause
