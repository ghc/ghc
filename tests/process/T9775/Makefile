# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T9775
T9775:
	'$(TEST_CC)' $(TEST_CC_OPTS) ok.c -o ok.exe
	'$(TEST_CC)' $(TEST_CC_OPTS) main.c -o main.exe
