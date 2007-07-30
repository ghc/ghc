
# We don't include any of the boilerplate Makefiles as we are used
# by configure. GHC should be overridden on the command line to the
# GHC that you want to use.

GHC=ghc

.PHONY: all clean

all:
	$(GHC) --make pwd

clean:
	rm -f *.o
	rm -f *.hi
	rm -f pwd
	rm -f pwd.exe

