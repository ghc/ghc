TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKGCONF=local.package.conf
GHC_PKG= $(FPTOOLS_TOP)/ghc/utils/ghc-pkg/ghc-pkg-inplace -f $(PKGCONF)

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

	@: # cannot use testpkg as a package name, it is ambiguous now
	if $(GHC_PKG) describe testpkg; then false; else true; fi

	@: # test hiding/exposing
	$(GHC_PKG) hide testpkg-1.2.3.4
	$(GHC_PKG) field testpkg-1.2.3.4 exposed | grep False
	$(GHC_PKG) expose testpkg-2.0
	$(GHC_PKG) field testpkg-2.0 exposed | grep True
	$(GHC_PKG) hide testpkg-*
	$(GHC_PKG) field testpkg-* exposed

	@: # test3.pkg  depends on testpkg-2.0
	$(GHC_PKG) register test3.pkg
	$(GHC_PKG) unregister dep-1.0

	$(GHC_PKG) unregister testpkg-2.0
	$(GHC_PKG) describe testpkg
	$(GHC_PKG) unregister testpkg-*
	$(GHC_PKG) list

	@: # registering test3.pkg should fail now, its dependency has gone
	if $(GHC_PKG) register test3.pkg; then false; else true; fi
