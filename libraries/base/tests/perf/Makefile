# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk


T17752:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O --make T17752 -rtsopts -ddump-simpl -ddump-to-file -dsuppress-uniques -dsuppress-all
	# All occurences of elem should be optimized away.
	# For strings these should result in loops after inlining foldCString.
	# For lists it should result in a case expression.
	echo $$(cat T17752.dump-simpl | grep "elem" -A4 )
