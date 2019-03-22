@echo off
setlocal enableDelayedExpansion
set "drives=ABCDEFGHIJKLMNOPQRSTUVWXYZ"
for /f "delims=:" %%A in ('wmic logicaldisk get caption') do set "drives=!drives:%%A=!"

rem all unused letters = %drives%
rem next unused letter = %drives:~0,1%
rem last unused letter = %drives:~-1%
echo %drives:~-1%