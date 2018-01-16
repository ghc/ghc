# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

build-tix:
	rm -Rf .hpc hpc001.o a.out
	'$(TEST_HC)' -fhpc hpc001.hs
	./a.out
	mv a.out.tix hpc_sample.tix

