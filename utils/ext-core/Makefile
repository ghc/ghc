all:	Check.hs Core.hs Driver.hs Env.hs Interp.hs ParsecParser.hs ParseGlue.hs Prep.hs Prims.hs Printer.hs
	ghc --make -fglasgow-exts -o Driver Driver.hs

#Parser.hs: Parser.y
#	happy -ad -ihappy.debug -o Parser.hs Parser.y