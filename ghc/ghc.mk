# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

ghc_USES_CABAL = YES
ghc_PACKAGE = ghc-bin
ghc_EXECUTABLE = ghc

ifeq "$(GhcWithInterpreter)" "YES"
ghc_stage1_CONFIGURE_OPTS += --flags=internal-interpreter
ghc_stage2_CONFIGURE_OPTS += --flags=internal-interpreter
endif

# This package doesn't pass the Cabal checks because data-dir
# points outside the source directory. This isn't a real problem, so
# we just skip the check.
ghc_NO_CHECK = YES

ghc_stage0_MORE_HC_OPTS = $(GhcStage0HcOpts)
ghc_stage1_MORE_HC_OPTS = $(GhcStage1HcOpts)
ghc_stage2_MORE_HC_OPTS = $(GhcStage2HcOpts)

# We need __GLASGOW_HASKELL__ in hschooks.c, so we have to build C
# sources with GHC:
ghc_stage0_UseGhcForCC = YES
ghc_stage1_UseGhcForCC = YES
ghc_stage2_UseGhcForCC = YES

ifeq "$(GhcDebugged)" "YES"
ghc_stage0_MORE_HC_OPTS += -debug
ghc_stage1_MORE_HC_OPTS += -debug
ghc_stage2_MORE_HC_OPTS += -debug
endif

ifneq "$(GhcDynamic)" ""
$(error GhcDynamic is no longer supported, use DYNAMIC_GHC_PROGRAMS instead)
endif

ifeq "$(GhcThreaded)" "YES"
# Use threaded RTS with GHCi, so threads don't get blocked at the prompt.
ghc_stage1_MORE_HC_OPTS += -threaded
ghc_stage2_MORE_HC_OPTS += -threaded
else
# Opt out from threaded GHC. See ghc-bin.cabal.in
ghc_stage1_CONFIGURE_OPTS += -f-threaded
ghc_stage2_CONFIGURE_OPTS += -f-threaded
endif

# If stage 0 supplies a threaded RTS, we can use it for stage 1.
# See Note [Linking ghc-bin against threaded stage0 RTS] in
# hadrian/src/Settings/Packages.hs for details.
ifeq "$(GhcThreadedRts)" "YES"
ghc_stage0_MORE_HC_OPTS += -threaded
else
ghc_stage0_CONFIGURE_OPTS += -f-threaded
endif

ifeq "$(GhcProfiled)" "YES"
ghc_stage1_PROGRAM_WAY = p
endif

ghc_stage0_PROGNAME = ghc-stage0
ghc_stage1_PROGNAME = ghc-stage1
ghc_stage2_PROGNAME = ghc-stage2

ghc_stage0_SHELL_WRAPPER = YES
ghc_stage1_SHELL_WRAPPER = YES
ghc_stage2_SHELL_WRAPPER = YES
ghc_stage0_SHELL_WRAPPER_NAME = ghc/ghc.wrapper
ghc_stage1_SHELL_WRAPPER_NAME = ghc/ghc.wrapper
ghc_stage2_SHELL_WRAPPER_NAME = ghc/ghc.wrapper
ghc_stage0_INSTALL_INPLACE = YES
ghc_stage1_INSTALL_INPLACE = YES
ghc_stage2_INSTALL_INPLACE = YES

ghc_stage$(INSTALL_GHC_STAGE)_INSTALL = YES
ghc_stage$(INSTALL_GHC_STAGE)_INSTALL_SHELL_WRAPPER_NAME = ghc-$(ProjectVersion)

# We override the program name to be ghc, rather than ghc-stage1.
# This means the right program name is used in error messages etc.
define ghc_stage$(INSTALL_GHC_STAGE)_INSTALL_SHELL_WRAPPER_EXTRA
echo 'executablename="$$exedir/ghc"' >> "$(WRAPPER)"
endef

# if stage is set to something other than "0" or "", disable stage 0
# See Note [Stage0Only vs stage=0] in mk/config.mk.in.
ifneq "$(filter-out 0,$(stage))" ""
ghc_stage0_NOT_NEEDED = YES
endif
# if stage is set to something other than "1" or "", disable stage 1
ifneq "$(filter-out 1,$(stage))" ""
ghc_stage1_NOT_NEEDED = YES
endif
# When cross-compiling, the stage 1 compiler is our release compiler, so omit stage 2
# See Note [Stage0Only vs stage=0] in mk/config.mk.in.
ifeq "$(Stage0Only)" "YES"
ghc_stage1_NOT_NEEDED = YES
endif
# stage 2 has to be requested explicitly with stage=2
ifneq "$(stage)" "2"
ghc_stage2_NOT_NEEDED = YES
endif
$(eval $(call build-prog,ghc,stage0,0))
$(eval $(call build-prog,ghc,stage1,1))
$(eval $(call build-prog,ghc,stage2,2))

ifneq "$(BINDIST)" "YES"

ghc/stage0/build/tmp/$(ghc_stage0_PROG) : $(BOOT_LIBS)
ifeq "$(GhcProfiled)" "YES"
ghc/stage1/build/tmp/$(ghc_stage1_PROG) : $(compiler_stage1_p_LIB)
ghc/stage1/build/tmp/$(ghc_stage1_PROG) : $(foreach lib,$(PACKAGES_STAGE1),$(libraries/$(lib)_dist-install_p_LIB))
endif

all_ghc_stage0 : $(ghc-stage0_INPLACE)
all_ghc_stage1 : $(ghc-stage1_INPLACE)
all_ghc_stage2 : $(ghc-stage2_INPLACE)

$(INPLACE_LIB)/settings : $(includes_SETTINGS)
	"$(CP)" $< $@

$(INPLACE_LIB)/llvm-targets : llvm-targets
	"$(CP)" $< $@

$(INPLACE_LIB)/llvm-passes : llvm-passes
	"$(CP)" $< $@

# The GHC programs need to depend on all the helper programs they might call,
# and the settings files they use

GHC_DEPENDENCIES += $$(unlit_INPLACE)
GHC_DEPENDENCIES += $(INPLACE_LIB)/settings
GHC_DEPENDENCIES += $(INPLACE_LIB)/llvm-targets
GHC_DEPENDENCIES += $(INPLACE_LIB)/llvm-passes

$(HC_STAGE1) : | $(GHC_DEPENDENCIES)
$(HC_STAGE2) : | $(GHC_DEPENDENCIES)
$(HC_STAGE3) : | $(GHC_DEPENDENCIES)

ifeq "$(Windows_Host)" "YES"
$(HC_STAGE1) : | $$(touchy_INPLACE)
$(HC_STAGE2) : | $$(touchy_INPLACE)
$(HC_STAGE3) : | $$(touchy_INPLACE)
endif

# Modules like vector:Data.Vector.Fusion.Stream.Monadic use annotations,
# which means they depend on GHC.Desugar. To ensure that This module is
# available by the time it is needed, we make the stage 2 compiler
# depend on it.
$(HC_STAGE2) : $(foreach w,$(GhcLibWays),libraries/base/dist-install/build/GHC/Desugar.$($w_osuf))

endif

INSTALL_LIBS += $(includes_SETTINGS)
INSTALL_LIBS += llvm-targets
INSTALL_LIBS += llvm-passes

ifeq "$(Windows_Host)" "NO"
install: install_ghc_link
.PHONY: install_ghc_link
install_ghc_link:
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc")
	$(LN_S) $(CrossCompilePrefix)ghc-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc"
else
# On Windows we install the main binary as $(bindir)/ghc.exe
# To get ghc-<version>.exe we have a little C program in driver/ghc
install: install_ghc_post
.PHONY: install_ghc_post
install_ghc_post: install_bins
	$(call removeFiles,"$(DESTDIR)$(bindir)/ghc.exe")
	"$(MV)" -f $(DESTDIR)$(bindir)/ghc-stage$(INSTALL_GHC_STAGE).exe $(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghc.exe
endif
