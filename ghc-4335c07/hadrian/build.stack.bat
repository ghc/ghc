@echo off
setlocal
rem Change the current directory to the one containing this script
cd %~dp0

rem Build Hadrian and dependencies and exit the script if the build failed
stack build
if %errorlevel% neq 0 exit /B %errorlevel%

rem Run Hadrian in GHC top directory forwarding additional user arguments
stack exec hadrian -- --lint --directory ".." %*
