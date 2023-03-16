TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

test_pe = test-package-environment
test_lib = containers

T18125:
	"$(GHC_PKG)" field base id --simple-output >  $(test_pe)
	"$(GHC_PKG)" field $(test_lib) id --simple-outpu >> $(test_pe)
	"$(TEST_HC)" $(TEST_HC_OPTS) -Wunused-packages -package-env $(test_pe) T18125.hs > out 2>&1
	C=`cat out | grep "$(test_lib)" -c` ; \
	if [ $$C != "1" ]; then false; fi
