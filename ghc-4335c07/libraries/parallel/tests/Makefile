# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk


.PHONY: par003
par003 :
	@echo Compiling
	"$(TEST_HC)" -v0 -fforce-recomp --make par003.hs -o par003 -threaded -rtsopts
	@echo Running
	yes abqszzzq 2>/dev/null | head -n 11111 | ./par003 +RTS -N2
	@echo Done
