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
	$(MAKE) -s --no-print-directory clean
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(TEST_HC)" -v0 --make Setup.hs
	$(MAKE) -s --no-print-directory prep.a1
	$(MAKE) -s --no-print-directory prep.b
	./b/dist/build/test/test
	"$(GHC_PKG)" unregister --package-db=$(LOCAL_PKGCONF) a-1.0
	$(MAKE) -s --no-print-directory prep.a2
	cd b && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF) --ipid b
	cd b && ../Setup build
	./b/dist/build/test/test

prep.%:
	cd $* && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)  --ipid $*
	cd $* && ../Setup build -v0
	cd $* && ../Setup register -v0 --inplace
