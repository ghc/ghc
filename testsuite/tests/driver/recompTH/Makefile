TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o*
	rm -f *.dyn_o*
	rm -f *.hi*

# Test that using a TH splice in a module causes recompilation when the *source*
# of a dependency changes.

recompTH: clean
	'$(CP)' B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
	'$(CP)' B2.hs B.hs
	# Should print that A has been recompiled.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
