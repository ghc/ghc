
GHC = ghc

compare: *.hs
	"$(GHC)" --make -Wall -Werror $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.hi
	rm -f compare compare.exe

