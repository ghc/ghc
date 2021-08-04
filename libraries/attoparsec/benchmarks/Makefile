all: med.txt tiny

tiny: Tiny.hs
	ghc -O --make -o $@ $<

%: %.bz2
	bunzip2 -k $<

clean:
	-rm -f *.o *.hi tiny
