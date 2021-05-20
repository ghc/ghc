TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o*
	rm -f *.dyn_o*
	rm -f *.hi*

# If the source changes, but not the ABI, then only B should be recompiled.

recompNoTH: clean
	'$(CP)' B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
	'$(CP)' B2.hs B.hs
	# Should print that only B has been recompiled.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
