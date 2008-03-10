all:	Check.hs Core.hs Driver.hs Env.hs Interp.hs Lex.hs ParseGlue.hs Parser.hs Prep.hs Prims.hs Printer.hs
	ghc --make -fglasgow-exts -o Driver Driver.hs

Parser.hs: Parser.y
	happy -o Parser.hs Parser.y