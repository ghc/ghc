# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

hpc_ghc_ghci:
	rm -f A.o A.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhpc -c A.hs
	echo b | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) B.hs

