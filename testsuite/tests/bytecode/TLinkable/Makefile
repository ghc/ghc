TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: TLinkable_Prep
TLinkable_Prep:
	./genSplices TLinkable
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -fprefer-byte-code -v0 TLinkable.hs

.PHONY: TLinkable2_Prep
TLinkable2_Prep:
	./genSplices TLinkable2
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -fprefer-byte-code -fwrite-byte-code -v0 TLinkable2.hs

.PHONY: linkable_bytecodelib_Prep linkable_bytecodelib
PKGCONF01=bytecode.package.conf
LOCAL_GHC_PKG01='$(GHC_PKG)' --no-user-package-db -f $(PKGCONF01)
MAIN_MOD=BytecodeUsage
linkable_bytecodelib_Prep:
	$(LOCAL_GHC_PKG01) init $(PKGCONF01)
	mkdir outdir
	cd outdir && ../genSplices2 $(MAIN_MOD) 50 500
	mv outdir/$(MAIN_MOD).hs $(MAIN_MOD).hs
	cd outdir && $(TEST_HC) -bytecodelib -hisuf=$(ghciWayExt) $(ghciWayFlags) \
	   -o testpkg-1.2.3.4-XXX.bytecodelib -fbyte-code -fwrite-interface -fwrite-byte-code -this-unit-id=testpkg-1.2.3.4-XXX \
	   *.hs
	$(LOCAL_GHC_PKG01) register --force outdir/bytecode.pkg

linkable_bytecodelib:
	$(TEST_HC) $(TEST_HC_OPTS) $(ghcThWayFlags) --make -fprefer-byte-code $(MAIN_MOD) -package testpkg -package-db $(PKGCONF01) +RTS -l -hT -i0.001 -RTS
