TOP=../../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

build-tix:
	rm -Rf .hpc *.o Main
	'$(TEST_HC)' -fhpc -i.. --make Main.hs
	./Main
	mv Main.tix hpc_sample.tix

