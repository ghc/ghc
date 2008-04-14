all:	Check.hs Core.hs Driver.hs Env.hs Interp.hs ParsecParser.hs ParseGlue.hs Prep.hs PrimCoercions.hs Prims.hs Printer.hs
	ghc -O2 --make -fglasgow-exts -o Driver Driver.hs

# Run this when the primops.txt file changes
prims:  ../../compiler/prelude/primops.txt
	../genprimopcode/genprimopcode --make-ext-core-source < ../../compiler/prelude/primops.txt > PrimEnv.hs

#Parser.hs: Parser.y
#	happy -ad -ihappy.debug -o Parser.hs Parser.y

# The following assumes that you've built all the GHC libs with -fext-core...
libtest: all
	./Driver `find ../../libraries -print | grep .hcr$$ | grep -v bootstrapping`

# ...or built all the nofib programs with -fext-core.
nofibtest: all
	./Driver `find ../../nofib -print | grep .hcr$$`

clean:
	rm -f Driver *.hi *.o

reallyclean: clean
	rm PrimEnv.hs