
@echo off
echo Limpiando cache

echo Para borrar los Archivos temporales de internet:
RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 8

echo Para eliminar el historial:
RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 1

echo Borrar todo:
RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 255

exit
