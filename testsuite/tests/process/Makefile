# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: process007_fd
process007_fd:
	'$(TEST_HC)' -optc='-Wall' -no-hs-main -no-auto-link-packages process007_fd.c -o process007_fd

.PHONY: T3994app
T3994app:
	'$(TEST_HC)' $(TEST_HC_OPTS) T3994app.hs -threaded
