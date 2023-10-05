TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

hpc_ghc_ghci:
	rm -f A.o A.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhpc -c A.hs
	echo b | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) B.hs

