@mkdir .shake 2> nul
@ghc --make -Wall src/Main.hs -isrc -rtsopts -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && .shake\build --lint --directory ".." %*
