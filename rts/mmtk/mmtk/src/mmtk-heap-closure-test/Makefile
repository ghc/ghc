GHC ?= ghc

all : Main

c_printClosure.o : c_printClosure.c
	${GHC} -c $< -o $@ -g3 -optc-g3

cmm_printClosure.o : cmm_printClosure.cmm
	${GHC} -c $< -o $@ -g3

Main.o : Main.hs
	${GHC} -c $< -o $@ -g3

Main : Main.o cmm_printClosure.o c_printClosure.o
	${GHC} ${EXTRA_HC_OPTS} $+ -g3 -debug -mmtk -o $@
