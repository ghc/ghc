TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

T13350:
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	cd boolean && "$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make Setup.hs
	cd boolean && ./Setup configure -v0 --with-compiler="$(TEST_HC)" --ghc-options='$(filter-out -rtsopts,$(TEST_HC_OPTS))' --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd boolean && ./Setup build -v0
	cd boolean && ./Setup register -v0 --inplace
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T13350.hs -package-db $(LOCAL_PKGCONF)
