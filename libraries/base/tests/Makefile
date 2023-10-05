# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T23697
T23697:
	'$(HSC2HS)' T23697.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -v0 T23697.hs
	./T23697
