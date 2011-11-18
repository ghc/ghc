TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f B.hs C

# Removing an instance declaration should force recompilation
# failed in GHC 7.0

T5147: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make -v0 A.hs
	sleep 1
	cp B2.hs B.hs
	if '$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make -v0 A.hs; then exit 1; else exit 0; fi
# Reverse the sense of the error; we *expect* 
# the compilation to fail, and then we want to succeed
