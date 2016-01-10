@cd %~dp0
@mkdir .shake 2> nul

@set ghcArgs=--make                   ^
             -Wall                    ^
             -fno-warn-name-shadowing ^
             src/Main.hs              ^
             -isrc                    ^
             -rtsopts                 ^
             -with-rtsopts=-I0        ^
             -outputdir=.shake        ^
             -j                       ^
             -O                       ^
             -o .shake/build

@set shakeArgs=--lint      ^
               --directory ^
               ".."        ^
               %*


@ghc %ghcArgs%

@if %ERRORLEVEL% EQU 0 (
    @rem Unset GHC_PACKAGE_PATH variable, as otherwise ghc-cabal complains
    @set GHC_PACKAGE_PATH=
    @.shake\build %shakeArgs%
)
