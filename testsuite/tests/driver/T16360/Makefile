TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=package.conf.d

clean:
	rm -f test/*.o test/*.hi *.o *.hi
	rm -rf $(LOCAL_PKGCONF)

.PHONY: T16360
T16360:
	@rm -rf $(LOCAL_PKGCONF)
	"$(TEST_HC)" $(TEST_HC_OPTS) -this-unit-id test-1.0 -c test/Test.hs
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(GHC_PKG)" --no-user-package-db -f $(LOCAL_PKGCONF) register test/test.pkg -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) -package-db $(LOCAL_PKGCONF)/ -c Hello.hs
