TOP=../../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Use a stage2 compiler
stage2=${TOP}/../ghc/compiler/stage2/ghc-inplace

# A tweaked main target to use a stage 2 compiler
tweak:
	gmake TEST_HC=${stage2}

# Just do the ghci way for testing during development
fast:
	gmake TEST_HC=${stage2} WAY=ghci

# Should be reused from somewhere else but Ralf cannot find it
clean:
	rm -f *~
	rm -f *.o
	rm -f *.script
	rm -f *.comp.*
	rm -f *.interp.*
	rm -f *.run.*
	rm -f *.hi
	rm -f *.prof


