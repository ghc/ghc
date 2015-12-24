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

@ghc %ghcArgs% && .shake\build %shakeArgs%
