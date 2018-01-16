GHC=            ghc
GHCFLAGS=       -cpp -O2 -funbox-strict-fields -v0 -fglasgow-exts -O2 -funbox-strict-fields -fdicts-cheap -fno-method-sharing -fmax-simplifier-iterations10 -fcpr-off

#PKG=            -package fps

#GHC=            /usr/obj/build/compiler/stage2/ghc-inplace
#PKG=            

BIN=            run-utests run-qtests

all:  prop prop-fusion-compiled prop-compiled prop-opt units-compiled
fast: prop-fast

everything: prop hugs prop-compiled prop-fusion-compiled runbench fusionbench wc spellcheck letter_frequency linesort fuse down-fuse inline zipwith unpack groupby run-lazybuild run-lazybuildcons run-lazyread

# ---------------------------------------------
# Boot in-place

.PHONY: boot-in-place

boot-in-place:
	cd .. && ${GHC} ${GHCFLAGS} --make Data/*hs Data/*/*hs Data/*/*/*hs

# ---------------------------------------------
# HUnit

# compiled
.PHONY: units-compiled
units-compiled:
	@echo "Compiled, rules off"
	${GHC} ${PKG} ${GHCFLAGS} -fno-rewrite-rules --make Units.hs -o u -package HUnit
	time ./u

# ---------------------------------------------
# ByteString Builders

hpc-builder: dist/build/test-builder/test-builder
	rm -f test-builder.tix
	dist/build/test-builder/test-builder
	hpc markup test-builder.tix



# ---------------------------------------------
# QuickCheck

# interpreted
.PHONY: prop hugs
prop:: 
	@echo "Interpreted"	
	runhaskell -i../cbits/fpstring.o Properties.hs

hugs::
	runhugs -98 Properties.hs

# interpreted fast. Just a quick check.
.PHONY: prop-fast
prop-fast::
	runhaskell -i../cbits/fpstring.o Properties.hs 30 

# compiled
.PHONY: prop-compiled
prop-compiled: 
	@echo "Compiled, rules off"	
	${GHC} ${PKG} ${GHCFLAGS} -fno-rewrite-rules --make Properties.hs -o p -package QuickCheck
	time ./p

# rules-on
.PHONY: prop-opt
prop-opt: 
	@echo "Compiled, rules on"	
	${GHC} ${PKG} ${GHCFLAGS} --make Properties.hs -o p -package QuickCheck
	time ./p

# rules-on
.PHONY: hpc-lite
hpc-lite: 
	@echo "Compiled, rules on, with hpc"	
	rm -f p.tix
	${GHC} ${GHCFLAGS} ../dist/build/cbits/fpstring.o -ddump-simpl-stats -fhpc --make -i.. Properties.hs -o p -O -fno-ignore-asserts -fasm -frewrite-rules
	rm Rules.o Rules.hi
	${GHC} ${GHCFLAGS} -hide-package bytestring ../dist/build/cbits/fpstring.o -ddump-simpl-stats -i.. Rules.hs -O -fno-ignore-asserts -fasm -frewrite-rules ../Data/ByteString/*.o ../Data/ByteString/*/*.o ../Data/ByteString.o -package QuickCheck QuickCheckUtils.o -c -no-recomp
	${GHC} ${GHCFLAGS} -package QuickCheck ../dist/build/cbits/fpstring.o Properties.o Rules.o QuickCheckUtils.o ../Data/ByteString/*.o ../Data/ByteString/*/*.o ../Data/ByteString.o -o p
	time ./p 40
	hpc markup p --exclude=Main --exclude=QuickCheckUtils --exclude=Data.ByteString.Fusion

#
# test rewriting is correct and working.
#
.PHONY: prop-fusion-compiled
prop-fusion-compiled:
	${GHC} ${PKG} ${GHCFLAGS} --make FusionProperties.hs -o fp -package QuickCheck
	time ./fp

# ---------------------------------------------------------
# Benchmarking

.PHONY: runbench
runbench: bench
	./bench +RTS -H64m

bench::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} ${GHCFLAGS} --make Bench.hs -o bench

fusionbench::
	${GHC} ${PKG} ${GHCFLAGS} --make FusionBench.hs -ddump-simpl-stats -o fusionbench

# ---------------------------------------------------------
# Profiling
.PHONY: prof

prof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} ${GHCFLAGS} -prof -fprof-auto --make Bench.hs -o bench
	./bench +RTS -H64m -p

fusionprof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} ${GHCFLAGS} -prof -fprof-auto --make FusionBench.hs -o fusionbench
	./bench +RTS -H64m -p

# ---------------------------------------------------------
# Small programs

wc: wc.o
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	time ./wc bigdata
wc.o: wc.hs
	${GHC} ${PKG} ${GHCFLAGS} --make -ddump-simpl-stats wc.hs -o wc

spellcheck: spellcheck.o
	time ./spellcheck < spellcheck-input.txt
spellcheck.o: spellcheck.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o spellcheck

letter_frequency: letter_frequency.o
	time ./letter_frequency < Usr.Dict.Words
letter_frequency.o: letter_frequency.hs
	${GHC} ${PKG} ${GHCFLAGS} --make  $< -o letter_frequency -ddump-simpl-stats

linesort: linesort.o
	time ./linesort < Usr.Dict.Words
linesort.o: linesort.hs
	${GHC} ${PKG} ${GHCFLAGS} --make  $< -o linesort

fuse: fuse.o
	time ./fuse < Usr.Dict.Words
fuse.o: fuse.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o fuse -ddump-simpl-stats

down-fuse: down-fuse.o
	time ./down-fuse
down-fuse.o: down-fuse.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o ./down-fuse -ddump-simpl-stats

.PHONY: run-lazybuild
run-lazybuild: lazybuild
	time ./lazybuild 100000000 > /dev/null
lazybuild: lazybuild.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o lazybuild

.PHONY: run-lazybuildcons
run-lazybuildcons: lazybuildcons
	time ./lazybuildcons 100000000 > /dev/null
lazybuildcons: lazybuildcons.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o lazybuildcons

.PHONY: run-lazyread
run-lazyread: lazyread
	./lazyread > /dev/null
lazyread: lazyread.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o lazyread

lazylines: lazylines.o
	time ./lazylines < bigdata
lazylines.o: lazylines.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o lazylines

inline: inline.o
	time ./inline
inline.o: inline.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o inline

zipwith: zipwith.o
	time ./zipwith
zipwith.o: zipwith.hs
	${GHC} ${PKG} ${GHCFLAGS} --make -ddump-simpl-stats $< -o zipwith

unpack: unpack.o
	time ./unpack
unpack.o: unpack.hs
	${GHC} ${PKG} ${GHCFLAGS} --make -ddump-simpl-stats $< -o unpack

groupby: groupby.o
	time ./groupby
groupby.o: groupby.hs
	${GHC} ${PKG} ${GHCFLAGS} --make $< -o groupby

# ---------------------------------------------------
# Plotting graphs

results: bench
	./bench +RTS -H64m > results

.PHONY: plot
plot:: results
	echo -e 'set terminal png          \n \
             set output "results.png"  \n \
             set yrange [0:2]          \n \
             plot "results" using ($$1):($$2) with boxes, "" using ($$1):($$3) with boxes' |\
             gnuplot ;
	xv results.png    

# ---------------------------------------------------
# Cleaning

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) bench */*.o */*.hi */*~ 
	rm -f wc letter_frequency spellcheck linesort fuse inline p u fp lazylines lazybuild lazybuildcons
	rm -f down-fuse zipwith fusionbench unpack groupby
	rm -rf *.tix *.html .hpc check
