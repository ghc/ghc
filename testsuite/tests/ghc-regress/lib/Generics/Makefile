TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Everything below should be removed ultimately.

drift	=	DrIFT-Strafunski
ghc	=	../../../../../ghc/compiler/stage2/ghc-inplace -fglasgow-exts
ghci	=	${ghc} --interactive

%.hs: %.ds
	${drift} -g Data $< > $@

local-build:
	gmake gen002
	gmake gen003

local-clean:
	rm -f *.o
	rm -f *.script
	rm -f *~
	rm -f *.hi
	rm -f *.prof
	rm -f *.comp.*
	rm -f *.run.*
	rm -f *.interp.*

local-very-clean: local-clean
	rm -f CompanyDatatypes.hs

gen002: gen002.hs CompanyDatatypes.hs
	${ghc} --make gen002.hs -o gen002 
	./gen002 > gen002.stdout

gen003: gen003.hs CompanyDatatypes.hs
	${ghc} --make gen003.hs -o gen003 
	./gen003 > gen003.stdout
