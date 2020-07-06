TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

SETUP='$(TEST_WRAPPER)' Setup -v0

T13350:
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	cd boolean && "$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make Setup.hs
	cd boolean && $(SETUP) configure --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd boolean && $(SETUP) build
	cd boolean && $(SETUP) register --inplace
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T13350.hs -package-db $(LOCAL_PKGCONF)
