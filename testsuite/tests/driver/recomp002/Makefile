TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o*
	rm -f *.hi*

# Only the first invocation should print any "Compiling" messages

recomp002: clean
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make Q.hs
	echo Middle >&2
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) --make Q.hs
