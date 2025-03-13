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
	# First time there are two warnings
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall

	echo "Test 1: Adding COMPLETE causes recompilation"
	cp A1.hs RecompCompletePragmaA.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaA.hs -Wall
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall

	echo "Test 2: Adding irrelevant COMPLETE does not cause recompilation"
	cp A2.hs RecompCompletePragmaA.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaA.hs -Wall
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall

	echo "Test 3: Modifying relevant COMPLETE does cause recompilation"
	cp A3.hs RecompCompletePragmaA.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaA.hs -Wall
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall

	echo "Test 4: Changing order of COMPLETE does not cause recompilation"
	cp A4.hs RecompCompletePragmaA.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaA.hs -Wall
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompletePragmaB.hs -Wall


RecompCompleteFixity:
	# First compilation
	rm -f *.o *.hi RecompCompleteFixityA.hs.modified
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompleteFixityA.hs -Wall
	# First time there are two warnings
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompleteFixityB.hs -Wall

	echo "Test 1: Removing fixity pragma, causes failure"
	cp B1.hs RecompCompleteFixityA.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompleteFixityA.hs -Wall
	# This command now fails.
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompCompleteFixityB.hs -Wall && exit 1 \
		|| echo "Test succeeded, recompilation occured"

