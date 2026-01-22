TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: LinkableUsage01_Prep
PKGCONF01=bytecode.package.conf
LOCAL_GHC_PKG01='$(GHC_PKG)' --no-user-package-db -f $(PKGCONF01)
MAIN_MOD01=LinkableUsage01
TEMPLATE_MOD01=BCOTemplate
LinkableUsage01_Prep:
	$(LOCAL_GHC_PKG01) init $(PKGCONF01)
	mkdir outdir
	cd outdir && ../genLinkables.sh $(MAIN_MOD01) ../$(TEMPLATE_MOD01).hs
	mv outdir/$(MAIN_MOD01).hs $(MAIN_MOD01).hs
	cd outdir && '$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) \
	  	-bytecodelib -hisuf=$(ghciWayExt) $(ghciWayFlags) \
		  -o testpkg-1.2.3.4-XXX.bytecodelib -fbyte-code -fwrite-interface -fwrite-byte-code \
			-this-unit-id=testpkg-1.2.3.4-XXX \
	   	*.hs
	$(LOCAL_GHC_PKG01) register --force outdir/bytecode.pkg

.PHONY: LinkableUsage02_Prep
MAIN_MOD02=LinkableUsage02
TEMPLATE_MOD02=BCOTemplate
LinkableUsage02_Prep:
	./genLinkables.sh $(MAIN_MOD02) $(TEMPLATE_MOD02).hs

PKGCONF03=bytecode.package.conf
LOCAL_GHC_PKG03='$(GHC_PKG)' --no-user-package-db -f $(PKGCONF03)
MAIN_MOD03=LinkableSpaceTest
TEMPLATE_MOD03=BCOTemplate
.PHONY: linkable-space_Prep
linkable-space_Prep:
	$(LOCAL_GHC_PKG03) init $(PKGCONF03)
	mkdir outdir
	cd outdir && ../genLinkables.sh $(MAIN_MOD03) ../$(TEMPLATE_MOD03).hs
	mv outdir/$(MAIN_MOD03).hs $(MAIN_MOD03).hs
	cd outdir && '$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) \
	  	-bytecodelib -hisuf=$(ghciWayExt) $(ghciWayFlags) \
		  -o testpkg-1.2.3.4-XXX.bytecodelib -fbyte-code -fwrite-interface -fwrite-byte-code \
			-this-unit-id=testpkg-1.2.3.4-XXX \
	   	*.hs
	$(LOCAL_GHC_PKG03) register --force outdir/bytecode.pkg

.PHONY: linkable-space
linkable-space:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcThWayFlags) -Wall -v0 linkable-space.hs -rtsopts -package ghc -threaded
	./linkable-space$(exeext) "`'$(TEST_HC)' --print-libdir | tr -d '\r'`" $(ghciWayFlags) +RTS -hT --no-automatic-heap-samples -RTS

