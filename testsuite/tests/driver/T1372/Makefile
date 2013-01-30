TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

LOCAL_PKGCONF=local.package.conf

clean:
	rm -f p1/setup p1/Setup.o p1/Setup.hi
	rm -f p2/setup p2/Setup.o p2/Setup.hi
	rm -rf p1/dist p2/dist
	rm -f *.o *.hi
	rm -f clean.out prep.out
	rm -f p1/A.hs
	rm -f $(LOCAL_PKGCONF)

T1372:
	$(MAKE) clean
	$(MAKE) prep
# This should recompile Main.hs, because A in package p1 has changed
# and recompiling Main.hs will now fail.
	-cd p2 && ./setup build -v0
	$(MAKE) clean

prep:
	echo "[]" >$(LOCAL_PKGCONF)
	cp p1/A1.hs p1/A.hs
	$(MAKE) prep.p1
	$(MAKE) prep.p2
	cp p1/A2.hs p1/A.hs
	cd p1 && ./setup build -v0

prep.%:
	cd $* && "$(TEST_HC)" $(TEST_HC_OPTS_NO_RECOMP) -v0 --make -o setup Setup.hs
	cd $* && ./setup configure $(CABAL_MINIMAL_BUILD) -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF)
	cd $* && ./setup build -v0
	cd $* && ./setup register -v0 --inplace

