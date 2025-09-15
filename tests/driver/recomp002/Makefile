TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o*
	rm -f *.hi*

# Only the first invocation should print any "Compiling" messages

recomp002: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Q.hs
	echo Middle >&2
	'$(TEST_HC)' $(TEST_HC_OPTS) --make Q.hs
