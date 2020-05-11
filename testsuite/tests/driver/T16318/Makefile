TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

test_pe = test-package-environment

T16318:
	$(GHC_PKG) latest base >  $(test_pe)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -package-env $(test_pe) -e "putStrLn \"Hello\"" > out 2>&1
	C=`cat out | grep "Loaded package environment" -c` ; \
	if [ $$C != "1" ]; then false; fi
