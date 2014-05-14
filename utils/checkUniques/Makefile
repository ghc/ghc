
GHC = ghc

PREL_NAMES = ../../compiler/prelude/PrelNames.lhs
DS_META = ../../compiler/deSugar/DsMeta.hs

.PHONY: check

check: checkUniques
	./checkUniques mkPreludeClassUnique   $(PREL_NAMES)
	./checkUniques mkPreludeTyConUnique   $(PREL_NAMES) $(DS_META)
	./checkUniques mkPreludeDataConUnique $(PREL_NAMES)
	./checkUniques mkPreludeMiscIdUnique  $(PREL_NAMES) $(DS_META)

checkUniques: checkUniques.hs
	$(GHC) -O -XHaskell2010 --make $@
