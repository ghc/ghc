TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

clean:
	rm -f p1/setup p1/Setup.o p1/Setup.hi
	rm -f p2/setup p2/Setup.o p2/Setup.hi
	rm -rf p1/dist p2/dist
	rm -f *.o *.hi
	rm -f clean.out prep.out
	rm -f p1/A.hs
	rm -rf $(LOCAL_PKGCONF)

T1372:
	$(MAKE) -s --no-print-directory clean
	$(MAKE) -s --no-print-directory prep
# This should recompile Main.hs, because A in package p1 has changed
# and recompiling Main.hs will now fail.
	-cd p2 && ./setup build -v0
	$(MAKE) -s --no-print-directory clean

prep:
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	cp p1/A1.hs p1/A.hs
	$(MAKE) -s --no-print-directory prep.p1
	$(MAKE) -s --no-print-directory prep.p2
	cp p1/A2.hs p1/A.hs
	cd p1 && ./setup build -v0

prep.%:
	cd $* && "$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -o setup Setup.hs
	cd $* && ./setup configure $(CABAL_MINIMAL_BUILD) -v0 --with-compiler="$(TEST_HC)" --ghc-options="$(TEST_HC_OPTS)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd $* && ./setup build -v0
	cd $* && ./setup register -v0 --inplace

