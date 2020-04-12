# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

#
# Header files built from the configure script's findings
#
includes_0_H_CONFIG   = includes/dist/build/ghcautoconf.h
includes_1_H_CONFIG   = includes/dist-install/build/ghcautoconf.h
includes_2_H_CONFIG   = $(includes_1_H_CONFIG)

includes_0_H_PLATFORM = includes/dist/build/ghcplatform.h
includes_1_H_PLATFORM = includes/dist-install/build/ghcplatform.h
includes_2_H_PLATFORM = $(includes_1_H_PLATFORM)

includes_0_H_VERSION  = includes/dist/build/ghcversion.h
includes_1_H_VERSION  = includes/dist-install/build/ghcversion.h
includes_2_H_VERSION  = $(includes_1_H_VERSION)

BUILD_0_INCLUDE_DIR = includes/dist/build
BUILD_1_INCLUDE_DIR = includes/dist-install/build
BUILD_2_INCLUDE_DIR = $(BUILD_1_INCLUDE_DIR)

#
# All header files are in includes/{one of these subdirectories}
#
includes_H_SUBDIRS += .
includes_H_SUBDIRS += rts
includes_H_SUBDIRS += rts/prof
includes_H_SUBDIRS += rts/storage
includes_H_SUBDIRS += stg

includes_H_FILES := $(wildcard $(patsubst %,includes/%/*.h,$(includes_H_SUBDIRS)))
# This isn't necessary, but it makes the paths look a little prettier
includes_H_FILES := $(subst /./,/,$(includes_H_FILES))

#
# Options
#

includes_CC_OPTS += $(SRC_CC_OPTS)
includes_CC_OPTS += $(SRC_CC_WARNING_OPTS)
includes_CC_OPTS += $(CONF_CC_OPTS_STAGE1)

ifeq "$(GhcUnregisterised)" "YES"
includes_CC_OPTS += -DUSE_MINIINTERPRETER
endif

includes_CC_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))
includes_CC_OPTS += -I$(BUILD_1_INCLUDE_DIR)
includes_CC_OPTS += -Irts

ifneq "$(GhcWithSMP)" "YES"
includes_CC_OPTS += -DNOSMP
endif

ifeq "$(DYNAMIC_BY_DEFAULT)" "YES"
includes_CC_OPTS += -DDYNAMIC_BY_DEFAULT
endif

define includesHeaderVersion
# $1 = stage
$$(includes_$1_H_VERSION) : mk/project.mk | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	@echo "Creating $$@..."
	@echo "#if !defined(__GHCVERSION_H__)"                                   > $$@
	@echo "#define __GHCVERSION_H__"                                        >> $$@
	@echo                                                                   >> $$@
	@echo "#define __GLASGOW_HASKELL__ $$(ProjectVersionInt)"               >> $$@
	@echo                                                                   >> $$@
	@if [ -n "$$(ProjectPatchLevel1)" ]; then \
	  echo "#define __GLASGOW_HASKELL_PATCHLEVEL1__ $$(ProjectPatchLevel1)" >> $$@; \
	fi
	@if [ -n "$$(ProjectPatchLevel2)" ]; then \
	  echo "#define __GLASGOW_HASKELL_PATCHLEVEL2__ $$(ProjectPatchLevel2)" >> $$@; \
	fi
	@echo                                                                   >> $$@
	@echo '#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (\'           >> $$@
	@echo '   ((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \'                  >> $$@
	@echo '   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \'                  >> $$@
	@echo '          && (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \'      >> $$@
	@echo '   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \'                  >> $$@
	@echo '          && (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \'         >> $$@
	@echo '          && (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )'         >> $$@
	@echo                                                                   >> $$@
	@echo "#endif /* __GHCVERSION_H__ */"                                   >> $$@
	@echo "Done."

endef

$(eval $(call includesHeaderVersion,0))
$(eval $(call includesHeaderVersion,1))

ifneq "$(BINDIST)" "YES"

define includesHeaderConfig
# $1 = stage
$$(includes_$1_H_CONFIG) : mk/config.h mk/config.mk includes/ghc.mk | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	@echo "Creating $$@..."
	@echo "#if !defined(__GHCAUTOCONF_H__)"  > $$@
	@echo "#define __GHCAUTOCONF_H__" >> $$@
#
#	Copy the contents of mk/config.h, turning '#define PACKAGE_FOO
#	"blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
#
	@sed 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$$$$,\1/* #undef \2 */,' mk/config.h >> $$@
#
	@echo "#endif /* __GHCAUTOCONF_H__ */"          >> $$@
	@echo "Done."

endef

$(eval $(call includesHeaderConfig,0))
$(eval $(call includesHeaderConfig,1))

BuildPlatform_0_CPP = $(BuildPlatform_CPP)
BuildPlatform_1_CPP = $(HostPlatform_CPP)
BuildPlatform_2_CPP = $(TargetPlatform_CPP)

HostPlatform_0_CPP = $(HostPlatform_CPP)
HostPlatform_1_CPP = $(TargetPlatform_CPP)
HostPlatform_2_CPP = $(TargetPlatform_CPP)

BuildArch_0_CPP = $(BuildArch_CPP)
BuildArch_1_CPP = $(HostArch_CPP)
BuildArch_2_CPP = $(TargetArch_CPP)

HostArch_0_CPP = $(HostArch_CPP)
HostArch_1_CPP = $(TargetArch_CPP)
HostArch_2_CPP = $(TargetArch_CPP)

BuildOS_0_CPP = $(BuildOS_CPP)
BuildOS_1_CPP = $(HostOS_CPP)
BuildOS_2_CPP = $(TargetOS_CPP)

HostOS_0_CPP = $(HostOS_CPP)
HostOS_1_CPP = $(TargetOS_CPP)
HostOS_2_CPP = $(TargetOS_CPP)

BuildVendor_0_CPP = $(BuildVendor_CPP)
BuildVendor_1_CPP = $(HostVendor_CPP)
BuildVendor_2_CPP = $(TargetVendor_CPP)

HostVendor_0_CPP = $(HostVendor_CPP)
HostVendor_1_CPP = $(TargetVendor_CPP)
HostVendor_2_CPP = $(TargetVendor_CPP)

define includesHeaderPlatform
# $1 = stage
$$(includes_$1_H_PLATFORM) : includes/ghc.mk includes/Makefile | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	@echo "Creating $$@..."
	@echo "#if !defined(__GHCPLATFORM_H__)"                      > $$@
	@echo "#define __GHCPLATFORM_H__"                           >> $$@
	@echo                                                       >> $$@
	@echo "#define GHC_STAGE $1"                                >> $$@
	@echo                                                       >> $$@
	@echo "#define BuildPlatform_TYPE  $(BuildPlatform_$1_CPP)" >> $$@
	@echo "#define HostPlatform_TYPE   $(HostPlatform_$1_CPP)"  >> $$@
	@echo                                                       >> $$@
	@echo "#define $(BuildPlatform_$1_CPP)_BUILD  1"            >> $$@
	@echo "#define $(HostPlatform_$1_CPP)_HOST  1"              >> $$@
	@echo                                                       >> $$@
	@echo "#define $(BuildArch_$1_CPP)_BUILD_ARCH  1"           >> $$@
	@echo "#define $(HostArch_$1_CPP)_HOST_ARCH  1"             >> $$@
	@echo "#define BUILD_ARCH  \"$(BuildArch_$1_CPP)\""         >> $$@
	@echo "#define HOST_ARCH  \"$(HostArch_$1_CPP)\""           >> $$@
	@echo                                                       >> $$@
	@echo "#define $(BuildOS_$1_CPP)_BUILD_OS  1"               >> $$@
	@echo "#define $(HostOS_$1_CPP)_HOST_OS  1"                 >> $$@
	@echo "#define BUILD_OS  \"$(BuildOS_$1_CPP)\""             >> $$@
	@echo "#define HOST_OS  \"$(HostOS_$1_CPP)\""               >> $$@
	@echo                                                       >> $$@
	@echo "#define $(BuildVendor_$1_CPP)_BUILD_VENDOR  1"       >> $$@
	@echo "#define $(HostVendor_$1_CPP)_HOST_VENDOR  1"         >> $$@
	@echo "#define BUILD_VENDOR  \"$(BuildVendor_$1_CPP)\""     >> $$@
	@echo "#define HOST_VENDOR  \"$(HostVendor_$1_CPP)\""       >> $$@
	@echo                                                       >> $$@
ifeq "$$(GhcUnregisterised)" "YES"
	@echo "#define UnregisterisedCompiler 1"                    >> $$@
endif
	@echo                                                       >> $$@
	@echo "#endif /* __GHCPLATFORM_H__ */"                      >> $$@
	@echo "Done."
endef

endif

$(eval $(call includesHeaderPlatform,0))
$(eval $(call includesHeaderPlatform,1))

# -----------------------------------------------------------------------------
# Settings

# These settings are read by GHC at runtime, so as to not cause spurious
# rebuilds.

includes_SETTINGS = includes/dist/build/settings

$(includes_SETTINGS) : includes/Makefile | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo '[("GCC extra via C opts", "$(GccExtraViaCOpts)")' >> $@
	@echo ',("C compiler command", "$(SettingsCCompilerCommand)")' >> $@
	@echo ',("C compiler flags", "$(SettingsCCompilerFlags)")' >> $@
	@echo ',("C++ compiler flags", "$(SettingsCxxCompilerFlags)")' >> $@
	@echo ',("C compiler link flags", "$(SettingsCCompilerLinkFlags)")' >> $@
	@echo ',("C compiler supports -no-pie", "$(SettingsCCompilerSupportsNoPie)")' >> $@
	@echo ',("Haskell CPP command", "$(SettingsHaskellCPPCommand)")' >> $@
	@echo ',("Haskell CPP flags", "$(SettingsHaskellCPPFlags)")' >> $@
	@echo ',("ld command", "$(SettingsLdCommand)")' >> $@
	@echo ',("ld flags", "$(SettingsLdFlags)")' >> $@
	@echo ',("ld supports compact unwind", "$(LdHasNoCompactUnwind)")' >> $@
	@echo ',("ld supports build-id", "$(LdHasBuildId)")' >> $@
	@echo ',("ld supports filelist", "$(LdHasFilelist)")' >> $@
	@echo ',("ld is GNU ld", "$(LdIsGNULd)")' >> $@
	@echo ',("ar command", "$(SettingsArCommand)")' >> $@
	@echo ',("ar flags", "$(ArArgs)")' >> $@
	@echo ',("ar supports at file", "$(ArSupportsAtFile)")' >> $@
	@echo ',("ranlib command", "$(SettingsRanlibCommand)")' >> $@
	@echo ',("touch command", "$(SettingsTouchCommand)")' >> $@
	@echo ',("dllwrap command", "$(SettingsDllWrapCommand)")' >> $@
	@echo ',("windres command", "$(SettingsWindresCommand)")' >> $@
	@echo ',("libtool command", "$(SettingsLibtoolCommand)")' >> $@
	@echo ',("unlit command", "$$topdir/bin/$(utils/unlit_dist_PROG)")' >> $@
	@echo ',("cross compiling", "$(CrossCompiling)")' >> $@
	@echo ',("target platform string", "$(TARGETPLATFORM)")' >> $@
	@echo ',("target os", "$(HaskellTargetOs)")' >> $@
	@echo ',("target arch", "$(HaskellTargetArch)")' >> $@
	@echo ',("target word size", "$(TargetWordSize)")' >> $@
	@echo ',("target word big endian", "$(TargetWordBigEndian)")' >> $@
	@echo ',("target has GNU nonexec stack", "$(TargetHasGnuNonexecStack)")' >> $@
	@echo ',("target has .ident directive", "$(TargetHasIdentDirective)")' >> $@
	@echo ',("target has subsections via symbols", "$(TargetHasSubsectionsViaSymbols)")' >> $@
	@echo ',("target has RTS linker", "$(TargetHasRTSLinker)")' >> $@
	@echo ',("Unregisterised", "$(GhcUnregisterised)")' >> $@
	@echo ',("LLVM target", "$(LLVMTarget_CPP)")' >> $@
	@echo ',("LLVM llc command", "$(SettingsLlcCommand)")' >> $@
	@echo ',("LLVM opt command", "$(SettingsOptCommand)")' >> $@
	@echo ',("LLVM clang command", "$(SettingsClangCommand)")' >> $@
	@echo
	@echo ',("integer library", "$(INTEGER_LIBRARY)")' >> $@
	@echo ',("Use interpreter", "$(GhcWithInterpreter)")' >> $@
	@echo ',("Use native code generator", "$(GhcWithNativeCodeGen)")' >> $@
	@echo ',("Support SMP", "$(GhcWithSMP)")' >> $@
	@echo ',("RTS ways", "$(GhcRTSWays)")' >> $@
	@echo ',("Tables next to code", "$(TablesNextToCode)")' >> $@
	@echo ',("Leading underscore", "$(LeadingUnderscore)")' >> $@
	@echo ',("Use LibFFI", "$(UseLibFFIForAdjustors)")' >> $@
# Note that GhcThreaded just reflects the Makefile variable setting. In
# particular, the stage1 compiler is never actually compiled with -threaded, but
# it will nevertheless have cGhcThreaded = True. The "+RTS --info" output will
# show what RTS GHC is really using.
	@echo ",(\"Use Threads\", \"$(GhcThreaded)\")" >> $@
	@echo ",(\"Use Debugging\", \"$(GhcDebugged)\")" >> $@
	@echo ",(\"RTS expects libdw\", \"$(GhcRtsWithLibdw)\")" >> $@
	@echo "]" >> $@


# ---------------------------------------------------------------------------
# Make DerivedConstants.h for the compiler

includes_DERIVEDCONSTANTS = includes/dist-derivedconstants/header/DerivedConstants.h
includes_GHCCONSTANTS_HASKELL_TYPE = includes/dist-derivedconstants/header/GHCConstantsHaskellType.hs
includes_GHCCONSTANTS_HASKELL_VALUE = includes/dist-derivedconstants/header/platformConstants
includes_GHCCONSTANTS_HASKELL_WRAPPERS = includes/dist-derivedconstants/header/GHCConstantsHaskellWrappers.hs
includes_GHCCONSTANTS_HASKELL_EXPORTS = includes/dist-derivedconstants/header/GHCConstantsHaskellExports.hs

INSTALL_LIBS += $(includes_GHCCONSTANTS_HASKELL_VALUE)

DERIVE_CONSTANTS_FLAGS += --gcc-program "$(CC)"
DERIVE_CONSTANTS_FLAGS += $(addprefix --gcc-flag$(space),$(includes_CC_OPTS) -fcommon)
DERIVE_CONSTANTS_FLAGS += --nm-program "$(NM)"
ifneq "$(OBJDUMP)" ""
DERIVE_CONSTANTS_FLAGS += --objdump-program "$(OBJDUMP)"
endif
DERIVE_CONSTANTS_FLAGS += --target-os "$(TargetOS_CPP)"

ifneq "$(BINDIST)" "YES"
$(includes_DERIVEDCONSTANTS):           $$(includes_H_FILES) $$(rts_H_FILES)
$(includes_GHCCONSTANTS_HASKELL_VALUE): $$(includes_H_FILES) $$(rts_H_FILES)

$(includes_DERIVEDCONSTANTS): $(deriveConstants_INPLACE) $(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) | $$(dir $$@)/.
	$< --gen-header -o $@ --tmpdir $(dir $@) $(DERIVE_CONSTANTS_FLAGS)

$(includes_GHCCONSTANTS_HASKELL_TYPE): $(deriveConstants_INPLACE) | $$(dir $$@)/.
	$< --gen-haskell-type -o $@ --tmpdir $(dir $@) $(DERIVE_CONSTANTS_FLAGS)

$(includes_GHCCONSTANTS_HASKELL_VALUE): $(deriveConstants_INPLACE) | $$(dir $$@)/.
	$< --gen-haskell-value -o $@ --tmpdir $(dir $@) $(DERIVE_CONSTANTS_FLAGS)

$(includes_GHCCONSTANTS_HASKELL_WRAPPERS): $(deriveConstants_INPLACE) | $$(dir $$@)/.
	$< --gen-haskell-wrappers -o $@ --tmpdir $(dir $@) $(DERIVE_CONSTANTS_FLAGS)

$(includes_GHCCONSTANTS_HASKELL_EXPORTS): $(deriveConstants_INPLACE) | $$(dir $$@)/.
	$< --gen-haskell-exports -o $@ --tmpdir $(dir $@) $(DERIVE_CONSTANTS_FLAGS)
endif

# ---------------------------------------------------------------------------
# Install all header files

$(eval $(call clean-target,includes,,\
  $(includes_0_H_CONFIG) $(includes_0_H_PLATFORM) $(includes_0_H_VERSION) \
  $(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) $(includes_1_H_VERSION)))

$(eval $(call all-target,includes,\
  $(includes_0_H_CONFIG) $(includes_0_H_PLATFORM) $(includes_0_H_VERSION) \
  $(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) $(includes_1_H_VERSION) \
  $(includes_GHCCONSTANTS_HASKELL_TYPE) \
  $(includes_GHCCONSTANTS_HASKELL_VALUE) \
  $(includes_GHCCONSTANTS_HASKELL_WRAPPERS) \
  $(includes_GHCCONSTANTS_HASKELL_EXPORTS) \
  $(includes_DERIVEDCONSTANTS)))

install: install_includes

.PHONY: install_includes
install_includes : $(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) $(includes_1_H_VERSION)
	$(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)"
	$(foreach d,$(includes_H_SUBDIRS), \
	    $(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)/$d" && \
	    $(INSTALL_HEADER) $(INSTALL_OPTS) includes/$d/*.h "$(DESTDIR)$(ghcheaderdir)/$d/" && \
	) true
	$(INSTALL_HEADER) $(INSTALL_OPTS) \
		$(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) $(includes_1_H_VERSION) \
		$(includes_DERIVEDCONSTANTS) \
		"$(DESTDIR)$(ghcheaderdir)/"

