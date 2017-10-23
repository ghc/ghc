@echo off
setlocal
cd %~dp0
mkdir bin 2> nul

set ghcArgs=--make                     ^
            -Wall                      ^
            -fno-warn-name-shadowing   ^
            -XRecordWildCards          ^
            src\Main.hs                ^
            -threaded                  ^
            -isrc                      ^
            -i..\libraries\Cabal\Cabal ^
            -rtsopts                   ^
            -with-rtsopts=-I0          ^
            -outputdir=bin             ^
            -j                         ^
            -O                         ^
            -o bin\hadrian

set hadrianArgs=--lint      ^
                --directory ^
                ".."        ^
                %*

ghc %ghcArgs%

if %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

rem Unset GHC_PACKAGE_PATH variable, as otherwise ghc-cabal complains
set GHC_PACKAGE_PATH=
bin\hadrian %hadrianArgs%
