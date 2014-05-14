
GHC = ghc

compare: *.hs
	"$(GHC)" -O -XHaskell2010 --make -Wall -Werror $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.hi
	rm -f compare compare.exe

