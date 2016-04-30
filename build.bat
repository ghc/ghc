@cd %~dp0
@mkdir ../_build 2> nul

@set ghcArgs=--make                       ^
             -Wall                        ^
             -fno-warn-name-shadowing     ^
             -XRecordWildCards            ^
             src/Main.hs                  ^
             -threaded                    ^
             -isrc                        ^
             -rtsopts                     ^
             -with-rtsopts=-I0            ^
             -outputdir=../_build/hadrian ^
             -j                           ^
             -O                           ^
             -o ../_build/hadrian

@set hadrianArgs=--lint      ^
                 --directory ^
                 ".."        ^
                 %*


@ghc %ghcArgs%

@if %ERRORLEVEL% NEQ 0 EXIT /B %ERRORLEVEL%

@rem Unset GHC_PACKAGE_PATH variable, as otherwise ghc-cabal complains
@set GHC_PACKAGE_PATH=
@..\_build\hadrian %hadrianArgs%
