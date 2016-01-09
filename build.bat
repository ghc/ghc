@mkdir .shake 2> nul

@set ghcArgs=--make            ^
             -Wall             ^
             src/Main.hs       ^
             -isrc             ^
             -rtsopts          ^
             -with-rtsopts=-I0 ^
             -outputdir=.shake ^
             -j                ^
             -O                ^
             -o .shake/build

@set shakeArgs=--lint      ^
               --directory ^
               ".."        ^
               %*

@rem Unset GHC_PACKAGE_PATH variable, as otherwise ghc-cabal complains
@if defined GHC_PACKAGE_PATH ( set GHC_PACKAGE_PATH )

@ghc %ghcArgs% && .shake\build %shakeArgs%
