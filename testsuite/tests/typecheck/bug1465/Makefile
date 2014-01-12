TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

PKG=bug1465

clean:
	rm -f v1/setup v1/Setup.o v1/Setup.hi
	rm -f v2/setup v2/Setup.o v2/Setup.hi
	rm -rf v1/dist v2/dist
	rm -f *.o *.hi
	rm -f $(LOCAL_PKGCONF)

bug1465:
	$(MAKE) clean
	$(MAKE) prep
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(LOCAL_PKGCONF) -c C.hs || exit 0
	$(MAKE) clean

prep:
	echo "[]" >$(LOCAL_PKGCONF)
	$(MAKE) prep.v1
	$(MAKE) prep.v2
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(LOCAL_PKGCONF) -c -package $(PKG)-1.0 B1.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(LOCAL_PKGCONF) -c -package $(PKG)-2.0 B2.hs

prep.%:
	cd $* && '$(TEST_HC)' -v0 --make -o setup Setup.hs
	cd $* && ./setup configure $(CABAL_MINIMAL_BUILD) -v0 --with-compiler='$(TEST_HC)' --with-ghc-pkg='$(GHC_PKG)' --package-db ../$(LOCAL_PKGCONF)
	cd $* && ./setup build -v0
	cd $* && ./setup register -v0 --inplace
