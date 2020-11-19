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
includes_1_H_CONFIG   = rts/dist-install/build/include/ghcautoconf.h
includes_2_H_CONFIG   = $(includes_1_H_CONFIG)

includes_1_H_PLATFORM = rts/dist-install/build/include/ghcplatform.h
includes_2_H_PLATFORM = $(includes_1_H_PLATFORM)

includes_INCLUDE_DIRS = .
# "includes" isn't really a separate component, but just part of the RTS that
# is in a separate ghc.mk for historical reasons. The ../dist-install puts the
# build products with the rest of the RTS's.
includes_dist-install_INCLUDE_DIRS = ../dist-install/build/include

includes_dist-install_DIST_INCLUDE_DIRS = \
	$(includes_INCLUDE_DIRS) \
	$(includes_dist-install_INCLUDE_DIRS)

#
# All header files are in rts/include/{one of these subdirectories}
#
includes_H_SUBDIRS += .
includes_H_SUBDIRS += rts
includes_H_SUBDIRS += rts/prof
includes_H_SUBDIRS += rts/storage
includes_H_SUBDIRS += stg

includes_H_FILES := $(wildcard $(patsubst %,rts/include/%/*.h,$(includes_H_SUBDIRS)))
# This isn't necessary, but it makes the paths look a little prettier
includes_H_FILES := $(subst /./,/,$(includes_H_FILES))

includes_H_FILES_GENERATED = \
    ghcautoconf.h \
    ghcplatform.h

# Unlike above, include generated files. We still need the previous list
# without the generated files separtely and not just as part of this due to
# lingering issues like the derived constants generation snooping the RTS
# headers.
includes_dist_H_FILES = \
	$(includes_H_FILES)
includes_dist-install_H_FILES = \
	$(includes_H_FILES) \
	$(includes_dist-install_H_FILES_GENERATED)
includes_dist-install_H_FILES_GENERATED = \
	$(patsubst %,rts/dist-install/build/include/%,$(includes_H_FILES_GENERATED))

#
# Options
#

includes_CC_OPTS += $(SRC_CC_OPTS)
includes_CC_OPTS += $(SRC_CC_WARNING_OPTS)
includes_CC_OPTS += $(CONF_CC_OPTS_STAGE1)

ifeq "$(GhcUnregisterised)" "YES"
includes_CC_OPTS += -DUSE_MINIINTERPRETER
endif

includes_CC_OPTS += $(addprefix -Irts/,$(rts_dist-install_DIST_INCLUDE_DIRS))

ifneq "$(GhcWithSMP)" "YES"
includes_CC_OPTS += -DNOSMP
endif

ifneq "$(BINDIST)" "YES"

define includesHeaderConfig
# $1 = stage
$$(includes_$1_H_CONFIG) : mk/config.h mk/config.mk rts/include/ghc.mk | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	@echo "Creating $$@..."
	@echo "#if !defined(__GHCAUTOCONF_H__)"  > $$@
	@echo "#define __GHCAUTOCONF_H__" >> $$@
#
#	Copy the contents of mk/config.h, turning '#define PACKAGE_FOO
#	"blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
#
	@sed \
		-e 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$$$$,\1/* #undef \2 */,' \
		-e '/__GLASGOW_HASKELL/d' \
		-e '/REMOVE ME/d' \
		mk/config.h \
		>> $$@
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
$$(includes_$1_H_PLATFORM) : rts/include/ghc.mk rts/include/Makefile | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	@echo "Creating $$@..."
	@echo "#if !defined(__GHCPLATFORM_H__)"                      > $$@
	@echo "#define __GHCPLATFORM_H__"                           >> $$@
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
# See Note [tooldir: How GHC finds mingw on Windows]

includes_SETTINGS = rts/dist-install/build/include/settings

# N.B. this is duplicated in hadrian/bindist/Makefile.
$(includes_SETTINGS) : rts/include/Makefile | $$(dir $$@)/.
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
	@echo ',("Merge objects command", "$(SettingsMergeObjectsCommand)")' >> $@
	@echo ',("Merge objects flags", "$(SettingsMergeObjectsFlags)")' >> $@
	@echo ',("ar command", "$(SettingsArCommand)")' >> $@
	@echo ',("ar flags", "$(ArArgs)")' >> $@
	@echo ',("ar supports at file", "$(ArSupportsAtFile)")' >> $@
	@echo ',("ranlib command", "$(SettingsRanlibCommand)")' >> $@
	@echo ',("otool command", "$(SettingsOtoolCommand)")' >> $@
	@echo ',("install_name_tool command", "$(SettingsInstallNameToolCommand)")' >> $@
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
	@echo ',("target has libm", "$(TargetHasLibm)")' >> $@
	@echo ',("Unregisterised", "$(GhcUnregisterised)")' >> $@
	@echo ',("LLVM target", "$(LLVMTarget_CPP)")' >> $@
	@echo ',("LLVM llc command", "$(SettingsLlcCommand)")' >> $@
	@echo ',("LLVM opt command", "$(SettingsOptCommand)")' >> $@
	@echo ',("LLVM clang command", "$(SettingsClangCommand)")' >> $@
	@echo ',("Use inplace MinGW toolchain", "$(SettingsUseDistroMINGW)")' >> $@
	@echo
	@echo ',("Use interpreter", "$(GhcWithInterpreter)")' >> $@
	@echo ',("Support SMP", "$(GhcWithSMP)")' >> $@
	@echo ',("RTS ways", "$(GhcRTSWays)")' >> $@
	@echo ',("Tables next to code", "$(TablesNextToCode)")' >> $@
	@echo ',("Leading underscore", "$(LeadingUnderscore)")' >> $@
	@echo ',("Use LibFFI", "$(UseLibffiForAdjustors)")' >> $@
	@echo ',("RTS expects libdw", "$(GhcRtsWithLibdw)")' >> $@
	@echo "]" >> $@


# ---------------------------------------------------------------------------
# Make DerivedConstants.h for the compiler

includes_DERIVEDCONSTANTS = rts/dist-install/build/include/DerivedConstants.h
includes_EVENTLOG_CONSTANTS = rts/dist-install/build/include/rts/EventLogConstants.h
includes_EVENT_TYPES = rts/dist-install/build/include/rts/EventTypes.h

DERIVE_CONSTANTS_FLAGS_FOR_HEADER += --gcc-program "$(CC)"
DERIVE_CONSTANTS_FLAGS_FOR_HEADER += $(addprefix --gcc-flag$(space),$(includes_CC_OPTS) -fcommon)
DERIVE_CONSTANTS_FLAGS_FOR_HEADER += --nm-program "$(NM)"
ifneq "$(OBJDUMP)" ""
DERIVE_CONSTANTS_FLAGS_FOR_HEADER += --objdump-program "$(OBJDUMP)"
endif
DERIVE_CONSTANTS_FLAGS_FOR_HEADER += --target-os "$(TargetOS_CPP)"

ifneq "$(BINDIST)" "YES"
$(includes_DERIVEDCONSTANTS):           $$(includes_H_FILES) $$(rts_H_FILES)

$(includes_DERIVEDCONSTANTS): $(deriveConstants_INPLACE) $(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) | $$(dir $$@)/.
	$< --gen-header -o $@ --tmpdir $(dir $@) $(DERIVE_CONSTANTS_FLAGS_FOR_HEADER)

$(includes_EVENTLOG_CONSTANTS): rts/gen_event_types.py
	mkdir -p $(dir $@)
	${PYTHON} $< --event-types-defines=$@

$(includes_EVENT_TYPES): rts/gen_event_types.py
	mkdir -p $(dir $@)
	${PYTHON} $< --event-types-array=$@

includes/EventLog.h : $(includes_EVENTLOG_CONSTANTS) $(includes_EVENT_TYPES)
endif

includes_dist-install_H_FILES_GENERATED += $(includes_DERIVEDCONSTANTS)
includes_dist-install_H_FILES_GENERATED += $(includes_EVENTLOG_CONSTANTS)
includes_dist-install_H_FILES_GENERATED += $(includes_EVENT_TYPES)

# ---------------------------------------------------------------------------
# Install all header files

$(eval $(call clean-target,includes,,$(includes_dist-install_H_FILES_GENERATED)))

$(eval $(call all-target,includes,$(includes_dist-install_H_FILES_GENERATED)))

install: install_includes

.PHONY: install_includes
install_includes : $(includes_dist-install_H_FILES_GENERATED)
	$(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)"
	$(foreach d,$(includes_H_SUBDIRS), \
	    $(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)/$d" && \
	    $(INSTALL_HEADER) $(INSTALL_OPTS) rts/include/$d/*.h "$(DESTDIR)$(ghcheaderdir)/$d/" && \
	) true
	$(INSTALL_HEADER) $(INSTALL_OPTS) \
		$(includes_dist-install_H_FILES_GENERATED) \
		"$(DESTDIR)$(ghcheaderdir)/"

