# -----------------------------------------------------------------------------
# Main compiler Makefile

# Targets:
#
#	all	builds stage1 compiler
#
#	boot stage=N   generate build dirs and dependencies for stage N.
#		       NB. Must be done before 'make stageN'.
#		       NB. Cannot 'make boot stage=2' until stage1 has
#		           been built (similarly for stage3).
#
#	stage1  (or stage=1) builds stage1 compiler
#	stage2  (or stage=2) builds stage2 compiler
#	stage3  (or stage=3) builds stage3 compiler
#

TOP = ..

ifeq "$(stage)" ""
stage=1
endif

include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

ifeq "$(GhcThreaded) $(GhcProfiled)" "YES YES"
$(error Cannot make GHC both threaded and profiled)
endif

CONFIG_HS 	= main/Config.hs
PRIMOP_BITS = primop-data-decl.hs-incl        \
              primop-tag.hs-incl              \
              primop-list.hs-incl             \
              primop-has-side-effects.hs-incl \
              primop-out-of-line.hs-incl      \
              primop-commutable.hs-incl       \
              primop-needs-wrapper.hs-incl    \
              primop-can-fail.hs-incl         \
              primop-strictness.hs-incl       \
              primop-primop-info.hs-incl

boot:: boot.stage.$(stage)

all:: build.stage.$(stage)

doc:: doc.stage.$(stage)

stage1 ::
	$(MAKE) stage=1

stage2 ::
	$(MAKE) stage=2

stage3 ::
	$(MAKE) stage=3

ifeq "$(CLEAN_ALL_STAGES)" "YES"
clean distclean maintainer-clean::
	$(RM) -f prelude/primops.txt
	$(RM) -f $(PRIMOP_BITS)
	$(RM) -f $(CONFIG_HS)
	$(RM) -f parser/Parser.y
	$(RM) -rf stage1 stage2plus
	$(RM) -f $(STAGE3_PACKAGE_CONF)
endif

ifeq "$(CLEAN_ALL_STAGES)" "YES"
clean distclean maintainer-clean:: clean.stage.1 clean.stage.2 clean.stage.3
else
clean distclean maintainer-clean:: clean.stage.$(stage)
endif

ifeq "$(CLEAN_ALL_STAGES)" "YES"
distclean maintainer-clean::
	$(RM) -f ghc.cabal
endif

clean.stage.%:
	$(RM) -f Makefile-stage$*
# This is a bit naughty. We ought to use:
#	-$(CABAL) clean --distpref dist-stage$*
# but that won't work if the Cabal file (a generated file) doesn't
# exist. So we do this instead:
	$(RM) -rf dist-stage$*

CONFIGURE_FLAGS_STAGE1 += --flags=stage1
CONFIGURE_FLAGS_STAGE2 += --flags=-stage1

ifeq "$(GhcWithNativeCodeGen)" "YES"
CONFIGURE_FLAGS_STAGE1 += --flags=ncg
CONFIGURE_FLAGS_STAGE2 += --flags=ncg
endif

ifeq "$(GhcWithInterpreter)" "YES"
CONFIGURE_FLAGS_STAGE2 += --flags=ghci

ifeq "$(BuildSharedLibs)" "YES"
CONFIGURE_FLAGS_STAGE2 += --enable-shared
# If we are going to use dynamic libraries instead of .o files for ghci,
# we will need to always retain CAFs in the compiler.
# ghci/keepCAFsForGHCi contains a GNU C __attribute__((constructor))
# function which sets the keepCAFs flag for the RTS before any Haskell
# code is run.
CONFIGURE_FLAGS_STAGE2 += --flags=dynlibs
endif

ifeq "$(GhcEnableTablesNextToCode) $(GhcUnregisterised)" "YES NO"
# Should GHCI be building info tables in the TABLES_NEXT_TO_CODE style
# or not?
# XXX This should logically be a CPP option, but there doesn't seem to
# be a flag for that
CONFIGURE_FLAGS_STAGE2 += --ghc-option=-DGHCI_TABLES_NEXT_TO_CODE
endif

# Should the debugger commands be enabled?
ifeq "$(GhciWithDebugger)" "YES"
CONFIGURE_FLAGS_STAGE2 += --ghc-option=-DDEBUGGER
endif

# Enable editline if:
#   - we're building stage 2/3, and we have built the editline package
#
# But we don't enable editline on Windows, as Windows terminals have
# editline-like support builtin.
#
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ifeq "$(wildcard $(FPTOOLS_TOP_ABS)/libraries/editline/unbuildable)" ""
CONFIGURE_FLAGS_STAGE2 += --flags=editline
endif
endif
endif

ifeq "$(GhcWithNativeCodeGen)" "NO"
# XXX This should logically be a CPP option, but there doesn't seem to
# be a flag for that
COMMON_CONFIGURE_FLAGS += --ghc-option=-DOMIT_NATIVE_CODEGEN
endif

ifeq "$(TargetOS_CPP)" "openbsd"
COMMON_CONFIGURE_FLAGS += --ld-options=-E
endif

ifeq "$(GhcUnregisterised)" "NO"
ifeq "$(HOSTPLATFORM)" "ia64-unknown-linux"
# needed for generating proper relocation in large binaries: trac #856
COMMON_CONFIGURE_FLAGS += --ld-option=-Wl,--relax
endif
endif

# We need to turn on profiling either if we have been asked to
# (GhcLibProfiled = YES) or if we want GHC itself to be compiled with
# profiling enabled (GhcProfiled = YES).
ifneq "$(GhcLibProfiled) $(GhcProfiled)" "NO NO"
CONFIGURE_FLAGS_STAGE2 += --enable-library-profiling
# And if we're profiling GHC then we want lots of SCCs.
# We also don't want to waste time building the non-profiling library,
# either normally or for ghci. Unfortunately this means that we have to
# tell ghc-pkg --force as it gets upset when libHSghc-6.9.a doesn't
# exist.
ifeq "$(GhcProfiled)" "YES"
CONFIGURE_FLAGS_STAGE2 += --ghc-option=-auto-all
CONFIGURE_FLAGS_STAGE2 += --disable-library-vanilla
CONFIGURE_FLAGS_STAGE2 += --disable-library-for-ghci
CONFIGURE_FLAGS_STAGE2 += --ghc-pkg-option=--force
endif
endif

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
# The #include is vital for the via-C route with older compilers, else the C
# compiler doesn't realise that the stcall foreign imports are indeed
# stdcall, and doesn't generate the Foo@8 name for them
# As it's only important for older compilers we don't need to do anything
# for stage2+.
CONFIGURE_FLAGS_STAGE1 += --ghc-option='-\#include'    \
                          --ghc-option='"<windows.h>"' \
                          --ghc-option='-\#include'    \
                          --ghc-option='"<process.h>"'
endif

# ghc_strlen percolates through so many modules that it is easier to get its
# prototype via a global option instead of a myriad of per-file OPTIONS.
# Again, this is only important for older compilers, so we don't do it in
# stage 2+.
CONFIGURE_FLAGS_STAGE1 += --ghc-options='-\#include "cutils.h"'

CONFIGURE_FLAGS_STAGE3 = $(CONFIGURE_FLAGS_STAGE2)
CONFIGURE_FLAGS_STAGE1 += $(USE_BOOT_CONFIGURE_FLAGS)
CONFIGURE_FLAGS_STAGE2 += $(USE_STAGE1_CONFIGURE_FLAGS)
CONFIGURE_FLAGS_STAGE3 += $(USE_STAGE2_CONFIGURE_FLAGS)

# In a source dist we don't need to worry about Parser.y(.pp) as we have
# the .hs file pre-generated
ifneq "$(wildcard parser/Parser.y.pp)" ""
PARSER_Y = parser/Parser.y
endif

boot.stage.%: $(PRIMOP_BITS) $(CONFIG_HS) $(PARSER_Y)
	test -f $(STAGE3_PACKAGE_CONF) || echo "[]" > $(STAGE3_PACKAGE_CONF)
	$(CABAL) configure --distpref dist-stage$* \
	                   $(CONFIGURE_FLAGS_STAGE$*) \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(COMMON_CONFIGURE_FLAGS) \
	                   --ghc-option=-DSTAGE=$*
	$(RM) -f Makefile-stage$*
	$(CABAL) makefile  --distpref dist-stage$* -f Makefile-stage$*

build.stage.%:
	$(MAKE) -f Makefile-stage$* stage=$*
	$(CABAL) register  --distpref dist-stage$* --inplace
	$(MAKE) -C ../ghc stage=$*

doc.stage.%:
	$(CABAL) haddock --distpref dist-stage$* \
	                 --haddock-option=--optghc=-DSTAGE=$* \
	                 --with-haddock=$(FPTOOLS_TOP_ABS)/utils/haddock/install-inplace/bin/haddock

install:
	$(INSTALL_PACKAGE) install '$(GHC_PKG_INSTALL_PROG)' '$(DESTDIR)$(datadir)/package.conf' '$(DESTDIR)' '$(prefix)' '$(iprefix)' '$(ibindir)' '$(ilibdir)' '$(ilibexecdir)' '$(idynlibdir)' '$(idatadir)' '$(idocdir)' '$(ihtmldir)' '$(ihaddockdir)' --distpref dist-stage2

# -----------------------------------------------------------------------------
# Create compiler configuration
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk
	@$(RM) -f $(CONFIG_HS)
	@echo "Creating $(CONFIG_HS) ... "
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cProjectName          :: String" >> $(CONFIG_HS)
	@echo "cProjectName          = \"$(ProjectName)\"" >> $(CONFIG_HS)
	@echo "cProjectVersion       :: String" >> $(CONFIG_HS)
	@echo "cProjectVersion       = \"$(ProjectVersion)\"" >> $(CONFIG_HS)
	@echo "cProjectVersionInt    :: String" >> $(CONFIG_HS)
	@echo "cProjectVersionInt    = \"$(ProjectVersionInt)\"" >> $(CONFIG_HS)
	@echo "cProjectPatchLevel    :: String" >> $(CONFIG_HS)
	@echo "cProjectPatchLevel    = \"$(ProjectPatchLevel)\"" >> $(CONFIG_HS)
	@echo "cBooterVersion        :: String" >> $(CONFIG_HS)
	@echo "cBooterVersion        = \"$(GhcVersion)\"" >> $(CONFIG_HS)
	@echo "cStage                :: String" >> $(CONFIG_HS)
	@echo "cStage                = show (STAGE :: Int)" >> $(CONFIG_HS)
	@echo "cHscIfaceFileVersion  :: String" >> $(CONFIG_HS)
	@echo "cHscIfaceFileVersion  = \"$(HscIfaceFileVersion)\"" >> $(CONFIG_HS)
	@echo "cSplitObjs            :: String" >> $(CONFIG_HS)
	@echo "cSplitObjs            = \"$(SplitObjs)\"" >> $(CONFIG_HS)
	@echo "cGhcWithInterpreter   :: String" >> $(CONFIG_HS)
	@echo "cGhcWithInterpreter   = \"$(GhcWithInterpreter)\"" >> $(CONFIG_HS)
	@echo "cGhcWithNativeCodeGen :: String" >> $(CONFIG_HS)
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> $(CONFIG_HS)
	@echo "cGhcWithSMP           :: String" >> $(CONFIG_HS)
	@echo "cGhcWithSMP           = \"$(GhcWithSMP)\"" >> $(CONFIG_HS)
	@echo "cGhcRTSWays           :: String" >> $(CONFIG_HS)
	@echo "cGhcRTSWays           = \"$(GhcRTSWays)\"" >> $(CONFIG_HS)
	@echo "cGhcUnregisterised    :: String" >> $(CONFIG_HS)
	@echo "cGhcUnregisterised    = \"$(GhcUnregisterised)\"" >> $(CONFIG_HS)
	@echo "cGhcEnableTablesNextToCode :: String" >> $(CONFIG_HS)
	@echo "cGhcEnableTablesNextToCode = \"$(GhcEnableTablesNextToCode)\"" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    :: String" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> $(CONFIG_HS)
	@echo "cRAWCPP_FLAGS         :: String" >> $(CONFIG_HS)
	@echo "cRAWCPP_FLAGS         = \"$(RAWCPP_FLAGS)\"" >> $(CONFIG_HS)
	@echo "cGCC                  :: String" >> $(CONFIG_HS)
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> $(CONFIG_HS)
	@echo "cMKDLL                :: String" >> $(CONFIG_HS)
	@echo "cMKDLL                = \"$(BLD_DLL)\"" >> $(CONFIG_HS)
	@echo "cLdIsGNULd            :: String" >> $(CONFIG_HS)
	@echo "cLdIsGNULd            = \"$(LdIsGNULd)\"" >> $(CONFIG_HS)
	@echo "cLD_X		     :: String" >> $(CONFIG_HS)
	@echo "cLD_X		     = \"$(LD_X)\"" >> $(CONFIG_HS)
	@echo "cGHC_DRIVER_DIR_REL   :: String" >> $(CONFIG_HS)
	@echo "cGHC_DRIVER_DIR_REL   = \"$(GHC_DRIVER_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_PGM       :: String" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_PGM       = \"$(GHC_TOUCHY_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_DIR_REL   :: String" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_DIR_REL   = \"$(GHC_TOUCHY_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_PGM        :: String" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_PGM        = \"$(GHC_UNLIT_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_DIR_REL    :: String" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_DIR_REL    = \"$(GHC_UNLIT_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_PGM      :: String" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_PGM      = \"$(GHC_MANGLER_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_DIR_REL  :: String" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_DIR_REL  = \"$(GHC_MANGLER_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_PGM        :: String" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_PGM        = \"$(GHC_SPLIT_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_DIR_REL    :: String" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_DIR_REL    = \"$(GHC_SPLIT_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_PGM       :: String" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_PGM       = \"$(GHC_SYSMAN)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_DIR_REL   :: String" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_DIR_REL   = \"$(GHC_SYSMAN_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_CP               :: String" >> $(CONFIG_HS)
	@echo "cGHC_CP               = \"$(GHC_CP)\"" >> $(CONFIG_HS)
	@echo "cGHC_PERL             :: String" >> $(CONFIG_HS)
	@echo "cGHC_PERL             = \"$(GHC_PERL)\"" >> $(CONFIG_HS)
	@echo "cEnableWin32DLLs      :: String" >> $(CONFIG_HS)
	@echo "cEnableWin32DLLs      = \"$(EnableWin32DLLs)\"" >> $(CONFIG_HS)
	@echo "cCONTEXT_DIFF         :: String" >> $(CONFIG_HS)
	@echo "cCONTEXT_DIFF         = \"$(CONTEXT_DIFF)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_NAMES       :: String" >> $(CONFIG_HS)
	@echo "cUSER_WAY_NAMES       = \"$(USER_WAY_NAMES)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_OPTS        :: String" >> $(CONFIG_HS)
	@echo "cUSER_WAY_OPTS        = \"$(USER_WAY_OPTS)\"" >> $(CONFIG_HS)
	@echo "cDEFAULT_TMPDIR       :: String" >> $(CONFIG_HS)
	@echo "cDEFAULT_TMPDIR       = \"$(DEFAULT_TMPDIR)\"" >> $(CONFIG_HS)
	@echo "cRelocatableBuild     :: Bool"                 >> $(CONFIG_HS)
ifeq "$(RelocatableBuild)" "YES"
	@echo "cRelocatableBuild     = True"                  >> $(CONFIG_HS)
else
	@echo "cRelocatableBuild     = False"                 >> $(CONFIG_HS)
endif
	@echo "cLibFFI               :: Bool"                 >> $(CONFIG_HS)
ifeq "$(UseLibFFIForAdjustors)" "YES"
	@echo "cLibFFI               = True"                  >> $(CONFIG_HS)
else
	@echo "cLibFFI               = False"                 >> $(CONFIG_HS)
endif
	@echo done.

# -----------------------------------------------------------------------------
# Create platform includes

# Here we generate a little header file containing CPP symbols that GHC
# uses to determine which platform it is building on/for.  The platforms
# can differ between stage1 and stage2 if we're cross-compiling, so we
# need one of these header files per stage.

PLATFORM_H = ghc_boot_platform.h

stage1/$(PLATFORM_H) : $(FPTOOLS_TOP)/mk/config.mk
	$(MKDIRHIER) stage1
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#ifndef __PLATFORM_H__"  >$@
	@echo "#define __PLATFORM_H__" >>$@
	@echo >> $@
	@echo "#define BuildPlatform_NAME  \"$(BUILDPLATFORM)\"" >> $@
	@echo "#define HostPlatform_NAME   \"$(HOSTPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD  	1" >> $@
	@echo "#define $(HostPlatform_CPP)_HOST		1" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET	1" >> $@
	@echo >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH  	1" >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH	1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH	1" >> $@
	@echo "#define BUILD_ARCH \"$(BuildArch_CPP)\"" >> $@
	@echo "#define HOST_ARCH \"$(HostArch_CPP)\"" >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(BuildOS_CPP)_BUILD_OS 		1" >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS		1" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS	1" >> $@  
	@echo "#define BUILD_OS \"$(BuildOS_CPP)\"" >> $@
	@echo "#define HOST_OS \"$(HostOS_CPP)\"" >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS		 " >> $@  
	@echo "#define $(IRIX_MAJOR)_TARGET_OS		1" >> $@  
	@echo "#endif					 " >> $@  
endif
	@echo >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 	1" >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR	1" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo "#define BUILD_VENDOR \"$(BuildVendor_CPP)\"" >> $@
	@echo "#define HOST_VENDOR \"$(HostVendor_CPP)\"" >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\"" >> $@
	@echo >> $@
	@echo "#endif /* __PLATFORM_H__ */"          >> $@
	@echo "Done."

# For stage2 and above, the BUILD platform is the HOST of stage1, and
# the HOST platform is the TARGET of stage1.  The TARGET remains the same
# (stage1 is the cross-compiler, not stage2).
stage2plus/$(PLATFORM_H) : $(FPTOOLS_TOP)/mk/config.mk
	$(MKDIRHIER) stage2plus
	@echo "Creating $@..."
	@$(RM) $@
	@echo "#ifndef __PLATFORM_H__"  >$@
	@echo "#define __PLATFORM_H__" >>$@
	@echo >> $@
	@echo "#define BuildPlatform_NAME  \"$(HOSTPLATFORM)\"" >> $@
	@echo "#define HostPlatform_NAME   \"$(TARGETPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD  	1" >> $@
	@echo "#define $(TargetPlatform_CPP)_HOST		1" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET	1" >> $@
	@echo >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH  	1" >> $@
	@echo "#define $(TargetArch_CPP)_HOST_ARCH	1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH	1" >> $@
	@echo "#define BUILD_ARCH \"$(HostArch_CPP)\"" >> $@
	@echo "#define HOST_ARCH \"$(TargetArch_CPP)\"" >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS 		1" >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS		1" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS	1" >> $@  
	@echo "#define BUILD_OS \"$(HostOS_CPP)\"" >> $@
	@echo "#define HOST_OS \"$(TargetOS_CPP)\"" >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\"" >> $@
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS		 " >> $@  
	@echo "#define $(IRIX_MAJOR)_TARGET_OS		1" >> $@  
	@echo "#endif					 " >> $@  
endif
	@echo >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR 	1" >> $@
	@echo "#define $(TargetVendor_CPP)_HOST_VENDOR	1" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
	@echo "#define BUILD_VENDOR \"$(HostVendor_CPP)\"" >> $@
	@echo "#define HOST_VENDOR \"$(TargetVendor_CPP)\"" >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\"" >> $@
	@echo >> $@
	@echo "#endif /* __PLATFORM_H__ */"          >> $@
	@echo "Done."

ifeq "$(stage)" "1"
STAGE_PLATFORM_H = stage1/$(PLATFORM_H)
else
STAGE_PLATFORM_H = stage2plus/$(PLATFORM_H)
endif

boot :: $(STAGE_PLATFORM_H)

# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.lhs 
#		from prelude/primops.txt

SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR)
SRC_CPP_OPTS += ${GhcCppOpts}

prelude/primops.txt parser/Parser.y: %: %.pp stage1/$(PLATFORM_H)
	$(CPP) $(RAWCPP_FLAGS) -P $(CPP_OPTS) -x c $< | grep -v '^#pragma GCC' > $@

primop-data-decl.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --data-decl          < $< > $@
primop-tag.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --primop-tag         < $< > $@
primop-list.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --primop-list        < $< > $@
primop-has-side-effects.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --has-side-effects   < $< > $@
primop-out-of-line.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --out-of-line        < $< > $@
primop-commutable.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --commutable         < $< > $@
primop-needs-wrapper.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --needs-wrapper      < $< > $@
primop-can-fail.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --can-fail           < $< > $@
primop-strictness.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --strictness         < $< > $@
primop-primop-info.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --primop-primop-info < $< > $@

# Usages aren't used any more; but the generator 
# can still generate them if we want them back
primop-usage.hs-incl: prelude/primops.txt
	$(GENPRIMOP) --usage              < $< > $@

html:
	$(MAKE) doc stage=2

install-docs:
	@:

#-----------------------------------------------------------------------------
# binary-dist
#

# Ideally we'd get these from the Cabal file's Install-Includes:
BINDIST_EXTRAS += HsVersions.h
ifeq "$(stage)" "1"
BINDIST_EXTRAS += stage1/ghc_boot_platform.h
else
BINDIST_EXTRAS += stage2plus/ghc_boot_platform.h
endif

include $(TOP)/mk/bindist.mk
LIB_DIST_DIR = dist-stage2

