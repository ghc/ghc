TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

clean:
	rm -f *.o*
	rm -f *.hi*
	rm -f C.hs

recomp005: clean
	cp C1.hs C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make E.hs -O
	sleep 1
	cp C2.hs C.hs
	# Should recompile E, because C is now an orphan
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make E.hs -O
