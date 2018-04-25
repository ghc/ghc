

OPTS= -O2 -Wall -XCPP

all: lib bench

lib:
	(cd .. && ghc $(OPTS) --make System/Random.hs)

bench:
	ghc $(OPTS) -i.. --make SimpleRNGBench.hs 

# When benchmarking against other packages installed via cabal it is
# necessary to IGNORE the System/Random.hs files in the current directory.
# (Otherwise instances of RandomGen are confused.)
bench2:
	ghc $(OPTS) -DTEST_COMPETITORS --make SimpleRNGBench.hs 

clean:
	rm -f *.o *.hi SimpleRNGBench
#	cabal clean
#	(cd Benchmark/ && rm -f *.o *.hi SimpleRNGBench)
#	(cd System/    && rm -f *.o *.hi SimpleRNGBench)
