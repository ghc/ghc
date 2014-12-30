@mkdir _shake 2> nul
@ghc --make -fwarn-tabs -fwarn-unused-imports src/Main.hs -isrc -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake\build --lint --directory ".." %*
