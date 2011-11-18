TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Test recompilation bug #1959, which failed in 6.8.1 (and earlier)

compile ::
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c -O E.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c -O D.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c -O -funfolding-use-threshold=0 C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -c -O B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) E.o D.o C.o B.o -o prog

clean ::
	rm -f *.o *.hi E.hs

dotest ::
	$(MAKE) clean
	cp E1.hs E.hs
	$(MAKE) compile
	./prog
	sleep 1
	cp E2.hs E.hs
	$(MAKE) compile
	./prog
