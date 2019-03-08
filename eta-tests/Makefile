EXTRA_HC_OPTS=

all: differentSat.bin zipLocal.bin zipWit2.bin zipWit3.bin zipWithPartial.bin

%.bin: %.hs
	../inplace/bin/ghc-stage2 $(EXTRA_HC_OPTS) -O2 -o $@ $<

clean:
	rm -rf *.hi *.o *.bin *~
