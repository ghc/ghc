TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=test.package.conf.d

clean:
	rm -f test/*.o test/*.hi *.o *.hi
	rm -rf $(LOCAL_PKGCONF)

.PHONY: T26423
T26423:
	@rm -rf $(LOCAL_PKGCONF)
	"$(TEST_HC)" $(TEST_HC_OPTS) -this-unit-id test-1.0 -c test/Test.hs
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(GHC_PKG)" --no-user-package-db -f $(LOCAL_PKGCONF) register test/test.pkg -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) -package-db $(LOCAL_PKGCONF)/  -package ghc  T26423.hs
	./T26423$(exeext) "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"
