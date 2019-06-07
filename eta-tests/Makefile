EXTRA_HC_OPTS=

all: differentSat.bin zipLocal.bin zipWit2.bin zipWit3.bin zipWithPartial.bin fac.bin

%.bin: %.hs
	../inplace/bin/ghc-stage2 $(EXTRA_HC_OPTS) -dverbose-core2core -O2 -feta-arity -o $@ $< 2>&1 | tee $@.log

clean:
	rm -rf *.hi *.o *.bin *~ *.log
