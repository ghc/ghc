TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o*
	rm -f *.dyn_o*
	rm -f *.hi*

# Touching a file should not cause recompilation

recompHash: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
	touch B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs
