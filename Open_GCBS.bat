@echo off
setlocal

cd /d "%~dp0"

where Rscript >nul 2>nul
if errorlevel 1 (
  echo R is required to run GCBS.
  echo Install R from https://cran.r-project.org, then run this file again.
  pause
  exit /b 1
)

Rscript "%~dp0launch_app.R"
pause
