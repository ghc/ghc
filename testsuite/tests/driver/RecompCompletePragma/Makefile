# Test for recompilation triggered by adding COMPLETE pragma

TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: clean

clean:
	rm -f *.o *.hi *.o-boot *.hi-boot

RecompCompletePragma:
	# First compilation
	rm -f *.o *.hi RecompCompletePragmaA.hs.modified
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaA.hs -Wall
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall
	cp RecompCompletePragmaB.hi RecompCompletePragmaB.hi.first

	# Modify A with a COMPLETE pragma and verify B is recompiled
	cat RecompCompletePragmaA.hs > RecompCompletePragmaA.hs.modified
	echo "{-# COMPLETE P #-}" >> RecompCompletePragmaA.hs.modified
	mv RecompCompletePragmaA.hs.modified RecompCompletePragmaA.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaA.hs -Wall

	# Compile B again - should be recompiled due to ABI change, and not emit a warning
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall



