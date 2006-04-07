TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKGCONF=local.package.conf
GHC_PKG= $(GHC_PKG_INPLACE) -f $(PKGCONF)

# a bunch of tests for ghc-pkg
ghcpkg01 :
	@rm -f $(PKGCONF)
	echo "[]" >$(PKGCONF)
	$(GHC_PKG) list
	$(GHC_PKG) register --force test.pkg
	$(GHC_PKG) describe testpkg
	$(GHC_PKG) describe testpkg-1.2.3.4
	$(GHC_PKG) field testpkg-1.2.3.4 import-dirs

	@: # test2.pkg is a later version of testpkg-1.2.3.4
	$(GHC_PKG) register --force test2.pkg
	$(GHC_PKG) list
	$(GHC_PKG) describe testpkg-2.0
	$(GHC_PKG) describe testpkg-*
	$(GHC_PKG) field testpkg-* version

	@: # test hiding/exposing
	$(GHC_PKG) hide testpkg-1.2.3.4
	$(GHC_PKG) field testpkg-1.2.3.4 exposed | grep False
	$(GHC_PKG) expose testpkg-2.0
	$(GHC_PKG) field testpkg-2.0 exposed | grep True
	$(GHC_PKG) hide testpkg-*
	$(GHC_PKG) field testpkg-* exposed

	@: # test3.pkg  depends on testpkg-2.0
	$(GHC_PKG) register test3.pkg
	$(GHC_PKG) unregister testpkg-3.0

	$(GHC_PKG) unregister testpkg-2.0
	$(GHC_PKG) describe testpkg
	$(GHC_PKG) unregister testpkg-*
	$(GHC_PKG) list

	@: # registering test3.pkg should fail now, its dependency has gone
	if $(GHC_PKG) register test3.pkg; then false; else true; fi

ghcpkg03 :
	@rm -f $(PKGCONF)
	echo "[]" >$(PKGCONF)
	@: # update should hide conflicting packages
	$(GHC_PKG) register --force test.pkg
	$(GHC_PKG) register --force test2.pkg

	@: # test4.pkg depends on testpkg-2.0, which conflicts with testpkg-1.2.3.4,
	@: # so ghc-pkg will refuse to install it when registering, and hide
	@: # testpkg-1.2.3.4 when updating.
	if $(GHC_PKG) register test4.pkg; then false; else true; fi
	$(GHC_PKG) update test4.pkg

	@: # and another update of test.pkg will now hide dep-2.0
	$(GHC_PKG) update --force test.pkg


# Test that importing a module exposed by two packages reports a conflict
ghcpkg04 :
	@rm -f $(PKGCONF)
	@echo "[]" >$(PKGCONF)
	@$(GHC_PKG) register --force test.pkg >/dev/null 2>&1 
	@$(GHC_PKG) update --force test5.pkg >/dev/null 2>&1 
	@$(GHC_PKG) expose testpkg-1.2.3.4  >/dev/null 2>&1
	@: # testpkg-1.2.3.4 and newtestpkg-2.0 are both exposed now
	@$(TEST_HC) -package-conf $(PKGCONF) -c ghcpkg04.hs || true

# -----------------------------------------------------------------------------
# Try piping the output of "ghc-pkg describe" into "ghc-pkg update" for
# every package we know about.  This is for testing the pretty printing/parsing
# of InstalledPackageInfo.

COPY_PACKAGE_CONF = package.conf.copy
GHC_PKG_COPY = $(GHC_PKG_INPLACE) --global-conf=$(COPY_PACKAGE_CONF)

copy :
	cp $(GHC_DRIVER_DIR)/package.conf.inplace $(COPY_PACKAGE_CONF)

ghcpkg02 : copy
	for i in `$(GHC_PKG_COPY) list --global --simple-output`; do \
		echo Updating $$i; \
		$(GHC_PKG_COPY) describe --global $$i | $(GHC_PKG_COPY) update --global --force -; \
	done

