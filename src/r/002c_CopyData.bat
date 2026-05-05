@echo off
setlocal

set "SRC=%~1"
set "DST=%~2"
set "VRT=%~3"

echo Copying VHM Swiss2 tiles...
robocopy "%SRC%" "%DST%" *.tif *.tif.ovr /NJH /NJS /NDL /NC /NS /NP /R:3 /W:5

echo Building VRT...
gdalbuildvrt -overwrite "%VRT%" "%DST%*.tif"

echo Done.
exit /b 0