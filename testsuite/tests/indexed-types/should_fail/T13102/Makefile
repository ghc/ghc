TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

SETUP='$(TEST_WRAPPER)' Setup -v0

T13102:
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	cd orphan && "$(TEST_HC)" -v0 --make Setup.hs
	cd orphan && $(SETUP) configure --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd orphan && $(SETUP) build
	cd orphan && $(SETUP) register --inplace
	! "$(TEST_HC)" $(TEST_HC_OPTS) -c A.hs B.hs -package-db $(LOCAL_PKGCONF)
