@echo off
setlocal
rem Change the current directory to the one containing this script
cd %~dp0

rem Build and run Hadrian in GHC top directory forwarding additional user arguments
stack run hadrian --cwd=.. -- %*
