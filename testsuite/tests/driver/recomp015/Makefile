TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f Test$(exeext)

# bug #8144

# All compilations except the first should print
# compilation IS NOT required

recomp015: clean
	ghc -c Test.hs -optP-include -optPcabal_macros.h
	touch cabal_macros.h
	ghc -c Test.hs -optP-include -optPcabal_macros.h
	touch cabal_macros.h
	ghc -c Test.hs -optP-include -optPcabal_macros.h
	touch cabal_macros.h
	ghc -c Test.hs -optP-include -optPcabal_macros.h
	touch cabal_macros.h
	ghc -c Test.hs -optP-include -optPcabal_macros.h
	touch cabal_macros.h
	sleep 2
	ghc -c Test.hs -optP-include -optPcabal_macros.h
