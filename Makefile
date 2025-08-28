export CABAL := $(shell cabal update 2>&1 >/dev/null && cabal list-bin -v0 --project-dir libraries/Cabal cabal-install:exe:cabal)

CPUS=$(shell mk/detect-cpu-count.sh)

# Use CPU cores + 1 if not already set
THREADS=${THREADS:-$((CPUS + 1))}

all: $(CABAL) ./booted
	GHC=ghc-9.8.4 ./Build.hs

cabal: $(CABAL)

$(CABAL):
	cabal build --project-dir libraries/Cabal cabal-install:exe:cabal

./booted:
	./boot
	touch $@

clean:
	rm -f ./booted
	rm -rf _build

test: all
	echo "using THREADS=${THREADS}" >&2
	TEST_HC=`pwd`/_build/bindist/bin/ghc \
	METRICS_FILE=`pwd`/_build/test-perf.csv \
	SUMMARY_FILE=`pwd`/_build/test-summary.txt \
	JUNIT_FILE=`pwd`/_build/test-junit.xml \
	make -C testsuite/tests test THREADS=${THREADS}
