TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

clean:
	rm -f Setup$(exeext)
	rm -rf a1/dist a2/dist b/dist
	rm -f *.o *.hi
	rm -f clean.out prep.out
	rm -rf $(LOCAL_PKGCONF)

recomp007:
	$(MAKE) clean
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(TEST_HC)" -v0 --make Setup.hs
	$(MAKE) prep.a1
	$(MAKE) prep.b
	"$(GHC_PKG)" unregister --package-db=$(LOCAL_PKGCONF) a-1.0
	$(MAKE) prep.a2
	cd b && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd b && ../Setup build

prep.%:
	cd $* && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd $* && ../Setup build -v0
	cd $* && ../Setup register -v0 --inplace
