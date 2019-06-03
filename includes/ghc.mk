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
# XXX: these should go in includes/dist/build?
includes_H_CONFIG   = includes/ghcautoconf.h
includes_H_PLATFORM = includes/ghcplatform.h
includes_H_VERSION  = includes/ghcversion.h

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
includes_CC_OPTS += -Irts

ifneq "$(GhcWithSMP)" "YES"
includes_CC_OPTS += -DNOSMP
endif

ifeq "$(DYNAMIC_BY_DEFAULT)" "YES"
includes_CC_OPTS += -DDYNAMIC_BY_DEFAULT
endif


$(includes_H_VERSION) : mk/project.mk | $$(dir $$@)/.
	@echo "Creating $@..."
	@echo "#ifndef __GHCVERSION_H__"  > $@
	@echo "#define __GHCVERSION_H__" >> $@
	@echo >> $@
	@echo "#define __GLASGOW_HASKELL__ $(ProjectVersionInt)" >> $@
	@echo >> $@
	@if [ -n "$(ProjectPatchLevel1)" ]; then \
	  echo "#define __GLASGOW_HASKELL_PATCHLEVEL1__ $(ProjectPatchLevel1)" >> $@; \
	fi
	@if [ -n "$(ProjectPatchLevel2)" ]; then \
	  echo "#define __GLASGOW_HASKELL_PATCHLEVEL2__ $(ProjectPatchLevel2)" >> $@; \
	fi
	@echo >> $@
	@echo '#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (\'      >> $@
	@echo '   ((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \'             >> $@
	@echo '   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \'             >> $@
	@echo '          && (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \' >> $@
	@echo '   ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \'             >> $@
	@echo '          && (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \'    >> $@
	@echo '          && (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )'    >> $@
	@echo >> $@
	@echo "#endif /* __GHCVERSION_H__ */"          >> $@
	@echo "Done."

ifneq "$(BINDIST)" "YES"

ifeq "$(PORTING_HOST)" "YES"

$(includes_H_CONFIG) :
	@echo "*** Cross-compiling: please copy $(includes_H_CONFIG) from the target system"
	@exit 1

else

$(includes_H_CONFIG) : mk/config.h mk/config.mk includes/ghc.mk | $$(dir $$@)/.
	@echo "Creating $@..."
	@echo "#ifndef __GHCAUTOCONF_H__"  >$@
	@echo "#define __GHCAUTOCONF_H__" >>$@
#
#	Copy the contents of mk/config.h, turning '#define PACKAGE_FOO
#	"blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
#
	@sed 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$$,\1/* #undef \2 */,' mk/config.h >> $@
#
#	Tack on some extra config information from the build system
#
ifeq "$(GhcEnableTablesNextToCode) $(GhcUnregisterised)" "YES NO"
	@echo >> $@
	@echo "#define TABLES_NEXT_TO_CODE 1" >> $@
endif
#
ifeq "$(CC_LLVM_BACKEND)" "1"
	@echo >> $@
	@echo "#define llvm_CC_FLAVOR 1" >> $@
endif
#
ifeq "$(CC_CLANG_BACKEND)" "1"
	@echo >> $@
	@echo "#define clang_CC_FLAVOR 1" >> $@
endif
#
	@echo "#endif /* __GHCAUTOCONF_H__ */"          >> $@
	@echo "Done."

endif

$(includes_H_PLATFORM) : includes/Makefile | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __GHCPLATFORM_H__"  >$@
	@echo "#define __GHCPLATFORM_H__" >>$@
	@echo >> $@
	@echo "#define BuildPlatform_TYPE  $(HostPlatform_CPP)" >> $@
	@echo "#define HostPlatform_TYPE   $(TargetPlatform_CPP)" >> $@
	@echo >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD  1" >> $@
	@echo "#define $(TargetPlatform_CPP)_HOST  1" >> $@
	@echo >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH  1" >> $@
	@echo "#define $(TargetArch_CPP)_HOST_ARCH  1" >> $@
	@echo "#define BUILD_ARCH  \"$(HostArch_CPP)\"" >> $@
	@echo "#define HOST_ARCH  \"$(TargetArch_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS  1" >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS  1" >> $@
	@echo "#define BUILD_OS  \"$(HostOS_CPP)\"" >> $@
	@echo "#define HOST_OS  \"$(TargetOS_CPP)\"" >> $@
	@echo >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR  1" >> $@
	@echo "#define $(TargetVendor_CPP)_HOST_VENDOR  1" >> $@
	@echo "#define BUILD_VENDOR  \"$(HostVendor_CPP)\"" >> $@
	@echo "#define HOST_VENDOR  \"$(TargetVendor_CPP)\"" >> $@
	@echo >> $@
	@echo "/* These TARGET macros are for backwards compatibility... DO NOT USE! */" >> $@
	@echo "#define TargetPlatform_TYPE $(TargetPlatform_CPP)" >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET  1" >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH  1" >> $@
	@echo "#define TARGET_ARCH  \"$(TargetArch_CPP)\"" >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS  1" >> $@  
	@echo "#define TARGET_OS  \"$(TargetOS_CPP)\"" >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1" >> $@
ifeq "$(GhcUnregisterised)" "YES"
	@echo "#define UnregisterisedCompiler 1" >> $@
endif
	@echo >> $@
	@echo "#endif /* __GHCPLATFORM_H__ */"          >> $@
	@echo "Done."

endif

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
	@echo ',("target has GNU nonexec stack", "$(HaskellHaveGnuNonexecStack)")' >> $@
	@echo ',("target has .ident directive", "$(HaskellHaveIdentDirective)")' >> $@
	@echo ',("target has subsections via symbols", "$(HaskellHaveSubsectionsViaSymbols)")' >> $@
	@echo ',("target has RTS linker", "$(HaskellHaveRTSLinker)")' >> $@
	@echo ',("Unregisterised", "$(Unregisterised)")' >> $@
	@echo ',("LLVM llc command", "$(SettingsLlcCommand)")' >> $@
	@echo ',("LLVM opt command", "$(SettingsOptCommand)")' >> $@
	@echo ',("LLVM clang command", "$(SettingsClangCommand)")' >> $@
	@echo
	@echo ',("integer library", "$(INTEGER_LIBRARY)")' >> $@
	@echo ',("Use interpreter", "$(GhcWithInterpreter)")' >> $@
	@echo ',("Use native code generator", "$(GhcWithNativeCodeGen)")' >> $@
	@echo ',("Support SMP", "$(GhcWithSMP)")' >> $@
	@echo ',("RTS ways", "$(GhcRTSWays)")' >> $@
	@echo ',("Tables next to code", "$(GhcEnableTablesNextToCode)")' >> $@
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
$(includes_DERIVEDCONSTANTS):           $$(includes_H_CONFIG) $$(includes_H_PLATFORM) $$(includes_H_VERSION) $$(includes_H_FILES) $$(rts_H_FILES)
$(includes_GHCCONSTANTS_HASKELL_VALUE): $$(includes_H_CONFIG) $$(includes_H_PLATFORM) $$(includes_H_VERSION) $$(includes_H_FILES) $$(rts_H_FILES)

$(includes_DERIVEDCONSTANTS): $(deriveConstants_INPLACE) | $$(dir $$@)/.
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
  $(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_H_VERSION)))

$(eval $(call all-target,includes,\
  $(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_H_VERSION) \
  $(includes_GHCCONSTANTS_HASKELL_TYPE) \
  $(includes_GHCCONSTANTS_HASKELL_VALUE) \
  $(includes_GHCCONSTANTS_HASKELL_WRAPPERS) \
  $(includes_GHCCONSTANTS_HASKELL_EXPORTS) \
  $(includes_DERIVEDCONSTANTS)))

install: install_includes

.PHONY: install_includes
install_includes :
	$(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)"
	$(foreach d,$(includes_H_SUBDIRS), \
	    $(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)/$d" && \
	    $(INSTALL_HEADER) $(INSTALL_OPTS) includes/$d/*.h "$(DESTDIR)$(ghcheaderdir)/$d/" && \
	) true
	$(INSTALL_HEADER) $(INSTALL_OPTS) $(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_H_VERSION) $(includes_DERIVEDCONSTANTS) "$(DESTDIR)$(ghcheaderdir)/"

