TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKGCONF01=local01.package.conf
LOCAL_GHC_PKG01 = $(GHC_PKG_INPLACE) -f $(PKGCONF01)

# a bunch of tests for ghc-pkg
ghcpkg01 :
	@rm -f $(PKGCONF01)
	echo "[]" >$(PKGCONF01)
	$(LOCAL_GHC_PKG01) list
	$(LOCAL_GHC_PKG01) register --force test.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) describe testpkg
	$(LOCAL_GHC_PKG01) describe testpkg-1.2.3.4
	$(LOCAL_GHC_PKG01) field testpkg-1.2.3.4 import-dirs

	@: # test2.pkg is a later version of testpkg-1.2.3.4
	$(LOCAL_GHC_PKG01) register --force test2.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) list
	$(LOCAL_GHC_PKG01) describe testpkg-2.0
	$(LOCAL_GHC_PKG01) describe testpkg-*
	$(LOCAL_GHC_PKG01) field testpkg-* version

	@: # test hiding/exposing
	$(LOCAL_GHC_PKG01) hide testpkg-1.2.3.4
	$(LOCAL_GHC_PKG01) field testpkg-1.2.3.4 exposed | grep False
	$(LOCAL_GHC_PKG01) expose testpkg-2.0
	$(LOCAL_GHC_PKG01) field testpkg-2.0 exposed | grep True
	$(LOCAL_GHC_PKG01) hide testpkg-*
	$(LOCAL_GHC_PKG01) field testpkg-* exposed

	@: # test3.pkg  depends on testpkg-2.0
	$(LOCAL_GHC_PKG01) register test3.pkg

	@: # unregistering testpkg-2.0 should fail now, testpkg-3.0 depends on it
	if $(LOCAL_GHC_PKG01) unregister testpkg-2.0; then false; else true; fi

	$(LOCAL_GHC_PKG01) unregister testpkg-3.0

	$(LOCAL_GHC_PKG01) unregister testpkg-2.0
	$(LOCAL_GHC_PKG01) describe testpkg
	$(LOCAL_GHC_PKG01) unregister testpkg-*
	$(LOCAL_GHC_PKG01) list

	@: # registering test3.pkg should fail now, its dependency has gone
	if $(LOCAL_GHC_PKG01) register test3.pkg; then false; else true; fi

PKGCONF03=local03.package.conf
LOCAL_GHC_PKG03 = $(GHC_PKG_INPLACE) -f $(PKGCONF03)

ghcpkg03 :
	@rm -f $(PKGCONF03)
	echo "[]" >$(PKGCONF03)
	@: # update should hide conflicting packages
	$(LOCAL_GHC_PKG03) register --force test.pkg
	$(LOCAL_GHC_PKG03) register --force test2.pkg

	@: # test4.pkg depends on testpkg-2.0, which conflicts with testpkg-1.2.3.4,
	@: # ghc-pkg will happily register it now (it didn't in GHC 6.4)
	$(LOCAL_GHC_PKG03) register test4.pkg
	$(LOCAL_GHC_PKG03) update test4.pkg

	@: # and another update of test.pkg will now hide dep-2.0
	$(LOCAL_GHC_PKG03) update --force test.pkg

PKGCONF04=local04.package.conf
LOCAL_GHC_PKG04 = $(GHC_PKG_INPLACE) -f $(PKGCONF04)

# Test that importing a module exposed by two packages reports a conflict
ghcpkg04 :
	rm -f $(PKGCONF04)
	echo "[]" >$(PKGCONF04)
	$(LOCAL_GHC_PKG04) register --force test.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG04) update --force test5.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG04) expose testpkg-1.2.3.4  >/dev/null 2>&1
	@: # testpkg-1.2.3.4 and newtestpkg-2.0 are both exposed now
	$(TEST_HC) -package-conf $(PKGCONF04) -c ghcpkg04.hs || true

# Test stacking of package.confs (also #2441)
PKGCONF05a=local05a.package.conf
PKGCONF05b=local05b.package.conf
LOCAL_GHC_PKG05a = $(GHC_PKG_INPLACE) -f $(PKGCONF05a)
LOCAL_GHC_PKG05b = $(GHC_PKG_INPLACE) -f $(PKGCONF05a) -f $(PKGCONF05b)

ghcpkg05 :
	rm -f $(PKGCONF05a) $(PKGCONF05b)
	echo "[]" >$(PKGCONF05a)
	echo "[]" >$(PKGCONF05b)
	$(LOCAL_GHC_PKG05a) register --force test2.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG05b) register --force test3.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG05b) check
	$(LOCAL_GHC_PKG05b) list
	$(LOCAL_GHC_PKG05b) expose testpkg-2.0
	$(LOCAL_GHC_PKG05b) hide   testpkg-2.0
	if $(LOCAL_GHC_PKG05b) unregister testpkg-2.0; then false; else true; fi
	$(LOCAL_GHC_PKG05b) unregister testpkg-3.0
	$(LOCAL_GHC_PKG05b) unregister testpkg-2.0

# Test for #2330: reject duplicate dependencies
PKGCONF06=local06.package.conf
LOCAL_GHC_PKG06 = $(GHC_PKG_INPLACE) -f $(PKGCONF06)

ghcpkg06 :
	rm -f $(PKGCONF06)
	echo "[]" >$(PKGCONF06)
	$(LOCAL_GHC_PKG06) register --force test.pkg >/dev/null 2>&1 
	if $(LOCAL_GHC_PKG06) register testdup.pkg; then false else true; fi
	$(LOCAL_GHC_PKG06) register --force testdup.pkg

PKGCONF1750=local1750.package.conf
LOCAL_GHC_PKG1750 = $(GHC_PKG_INPLACE) -f $(PKGCONF1750)

1750:
	rm -f $(PKGCONF1750) 1750.hs 1750.o 1750.hi 1750.out
	echo "[]" >$(PKGCONF1750)
	$(LOCAL_GHC_PKG1750) register --force 1750A.pkg >1750.out 2>&1
	$(LOCAL_GHC_PKG1750) register --force 1750B.pkg >1750.out 2>&1
	GHC_PACKAGE_PATH=$(PKGCONF1750) $(GHC_PKG_INPLACE) list
# GHC_PACKAGE_PATH trick is to make this work with 6.8.2 which doesn't have
# the patch "Change the command-line semantics for query commands" to
# ghc-pkg
	echo "main = return ()" >1750.hs
	$(TEST_HC) -package-conf $(PKGCONF1750) -package 1750A 1750.hs || true

# -----------------------------------------------------------------------------
# Try piping the output of "ghc-pkg describe" into "ghc-pkg update" for
# every package we know about.  This is for testing the pretty printing/parsing
# of InstalledPackageInfo.

COPY_PACKAGE_CONF = package.conf.copy
GHC_PKG_COPY = $(GHC_PKG_INPLACE) --global-conf=$(COPY_PACKAGE_CONF)

ghcpkg02:
	cp $(INPLACE_DATA_DIR)/package.conf $(COPY_PACKAGE_CONF)
	for i in `$(GHC_PKG_COPY) list --global --simple-output`; do \
		echo Updating $$i; \
		$(GHC_PKG_COPY) describe --global $$i | $(GHC_PKG_COPY) update --global --force -; \
	done
