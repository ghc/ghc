TOP=../../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

build-tix:
	rm -Rf .hpc *.o Main
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -fhpc -i.. --make Main.hs
	./Main
	mv Main.tix hpc_sample.tix

