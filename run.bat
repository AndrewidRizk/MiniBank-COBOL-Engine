@echo off
REM Run from MSYS2 environment
set "MSYS_ROOT=C:\msys64"
set "COB_CONFIG_DIR=%MSYS_ROOT%\mingw64\share\gnucobol\config"
set "PATH=%MSYS_ROOT%\mingw64\bin;%MSYS_ROOT%\usr\bin;%PATH%"

echo Writing deposit file with Python...
cd api
python deposit_stub.py
cd ..

echo Compiling COBOL with GnuCOBOL in MSYS2 MinGW64...
cobc -x -o minibank.exe src\minibank.cob

echo Running program...
minibank.exe

pause
