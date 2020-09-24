@echo off
setlocal
rem Change the current directory to the one containing this script
cd %~dp0

if "%STACK%"=="" (
    set STACK=stack
)

rem Build and run Hadrian in GHC top directory forwarding additional user arguments
%STACK% run hadrian --cwd=.. -- %*
