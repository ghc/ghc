# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

ghc_USES_CABAL = YES
ghc_PACKAGE = ghc-bin

ghc_stage1_CONFIGURE_OPTS += --flags=stage1
ghc_stage2_CONFIGURE_OPTS += --flags=stage2
ghc_stage3_CONFIGURE_OPTS += --flags=stage3

ifeq "$(GhcWithInterpreter)" "YES"
ghc_stage2_CONFIGURE_OPTS += --flags=ghci
ghc_stage3_CONFIGURE_OPTS += --flags=ghci
endif

ifeq "$(compiler_stage1_VERSION_MUNGED)" "YES"
# If we munge the stage1 version, and we're using a devel snapshot for
# stage0, then stage1 may actually have an earlier version than stage0
# (e.g. boot with ghc-7.5.20120316, building ghc-7.5). We therefore
# need to tell Cabal to use version 7.5 of the ghc package when building
# in ghc/stage1
ghc_stage1_CONFIGURE_OPTS += --constraint "ghc == $(compiler_stage1_MUNGED_VERSION)"
endif

# This package doesn't pass the Cabal checks because data-dir
# points outside the source directory. This isn't a real problem, so
# we just skip the check.
ghc_NO_CHECK = YES

ghc_stage1_MORE_HC_OPTS = $(GhcStage1HcOpts)
ghc_stage2_MORE_HC_OPTS = $(GhcStage2HcOpts)
ghc_stage3_MORE_HC_OPTS = $(GhcStage3HcOpts)

# We need __GLASGOW_HASKELL__ in hschooks.c, so we have to build C
# sources with GHC:
ghc_stage1_UseGhcForCC = YES
ghc_stage2_UseGhcForCC = YES
ghc_stage3_UseGhcForCC = YES

ghc_stage1_C_FILES_NODEPS = ghc/hschooks.c

ghc_stage2_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES
ghc_stage3_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES

ifeq "$(GhcDebugged)" "YES"
ghc_stage1_MORE_HC_OPTS += -debug
ghc_stage2_MORE_HC_OPTS += -debug
ghc_stage3_MORE_HC_OPTS += -debug
endif

ifeq "$(GhcDynamic)" "YES"
ghc_stage2_MORE_HC_OPTS += -dynamic
ghc_stage3_MORE_HC_OPTS += -dynamic
endif

ifeq "$(GhcThreaded)" "YES"
# Use threaded RTS with GHCi, so threads don't get blocked at the prompt.
ghc_stage2_MORE_HC_OPTS += -threaded
ghc_stage3_MORE_HC_OPTS += -threaded
endif

ifeq "$(GhcProfiled)" "YES"
ghc_stage2_PROGRAM_WAY = p
endif

ghc_stage1_PROG = ghc-stage1$(exeext)
ghc_stage2_PROG = ghc-stage2$(exeext)
ghc_stage3_PROG = ghc-stage3$(exeext)

ghc_stage1_SHELL_WRAPPER = YES
ghc_stage2_SHELL_WRAPPER = YES
ghc_stage3_SHELL_WRAPPER = YES
ghc_stage1_SHELL_WRAPPER_NAME = ghc/ghc.wrapper
ghc_stage2_SHELL_WRAPPER_NAME = ghc/ghc.wrapper
ghc_stage3_SHELL_WRAPPER_NAME = ghc/ghc.wrapper
ghc_stage1_INSTALL_INPLACE = YES
ghc_stage2_INSTALL_INPLACE = YES
ghc_stage3_INSTALL_INPLACE = YES

ghc_stage$(INSTALL_GHC_STAGE)_INSTALL = YES
ghc_stage$(INSTALL_GHC_STAGE)_INSTALL_SHELL_WRAPPER_NAME = ghc-$(ProjectVersion)

# We override the program name to be ghc, rather than ghc-stage2.
# This means the right program name is used in error messages etc.
define ghc_stage$(INSTALL_GHC_STAGE)_INSTALL_SHELL_WRAPPER_EXTRA
echo 'executablename="$$exedir/ghc"' >> "$(WRAPPER)"
endef

# if stage is set to something other than "1" or "", disable stage 1
ifneq "$(filter-out 1,$(stage))" ""
ghc_stage1_NOT_NEEDED = YES
endif
# if stage is set to something other than "2" or "", disable stage 2
ifneq "$(filter-out 2,$(stage))" ""
ghc_stage2_NOT_NEEDED = YES
endif
# When cross-compiling, the stage 1 compiler is our release compiler, so omit stage 2
ifeq "$(Stage1Only)" "YES"
ghc_stage2_NOT_NEEDED = YES
endif
# stage 3 has to be requested explicitly with stage=3
ifneq "$(stage)" "3"
ghc_stage3_NOT_NEEDED = YES
endif
$(eval $(call build-prog,ghc,stage1,0))
$(eval $(call build-prog,ghc,stage2,1))
$(eval $(call build-prog,ghc,stage3,2))

ifneq "$(BINDIST)" "YES"

ghc/stage1/build/tmp/$(ghc_stage1_PROG) : $(BOOT_LIBS)
ifeq "$(GhcProfiled)" "YES"
ghc/stage2/build/tmp/$(ghc_stage2_PROG) : $(compiler_stage2_p_LIB)
ghc/stage2/build/tmp/$(ghc_stage2_PROG) : $(foreach lib,$(PACKAGES_STAGE1),$(libraries/$(lib)_dist-install_p_LIB))
endif

# Modules here import HsVersions.h, so we need ghc_boot_platform.h
$(ghc_stage1_depfile_haskell) : compiler/stage1/$(PLATFORM_H)
$(ghc_stage2_depfile_haskell) : compiler/stage2/$(PLATFORM_H)
$(ghc_stage3_depfile_haskell) : compiler/stage3/$(PLATFORM_H)

all_ghc_stage1 : $(GHC_STAGE1)
all_ghc_stage2 : $(GHC_STAGE2)
all_ghc_stage3 : $(GHC_STAGE3)

$(INPLACE_LIB)/settings : settings
	"$(CP)" $< $@

$(INPLACE_LIB)/platformConstants: $(includes_GHCCONSTANTS_HASKELL_VALUE)
	"$(CP)" $< $@

# The GHC programs need to depend on all the helper programs they might call,
# and the settings files they use

GHC_DEPENDENCIES += $$(unlit_INPLACE)
GHC_DEPENDENCIES += $(INPLACE_LIB)/settings
GHC_DEPENDENCIES += $(INPLACE_LIB)/platformConstants

$(GHC_STAGE1) : | $(GHC_DEPENDENCIES)
$(GHC_STAGE2) : | $(GHC_DEPENDENCIES)
$(GHC_STAGE3) : | $(GHC_DEPENDENCIES)

ifeq "$(GhcUnregisterised)" "NO"
$(GHC_STAGE1) : | $$(ghc-split_INPLACE)
$(GHC_STAGE2) : | $$(ghc-split_INPLACE)
$(GHC_STAGE3) : | $$(ghc-split_INPLACE)
endif

ifeq "$(Windows_Host)" "YES"
$(GHC_STAGE1) : | $$(touchy_INPLACE)
$(GHC_STAGE2) : | $$(touchy_INPLACE)
$(GHC_STAGE3) : | $$(touchy_INPLACE)
endif

endif

INSTALL_LIBS += settings

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

