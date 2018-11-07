@echo off
setlocal
cd %~dp0

rem By default on Windows we build Hadrian using Stack
./build.stack.bat %*
