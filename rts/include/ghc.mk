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

BUILD_1_INCLUDE_DIRS = rts/include rts/dist-install/build/include
BUILD_2_INCLUDE_DIRS = $(BUILD_1_INCLUDE_DIRS)

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

includes_CC_OPTS += $(addprefix -I,$(BUILD_1_INCLUDE_DIRS))
includes_CC_OPTS += -Irts

ifneq "$(GhcWithSMP)" "YES"
includes_CC_OPTS += -DNOSMP
endif

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
	@echo ',("Use inplace MinGW toolchain", $(SettingsUseDistroMINGW)")' >> $@
	@echo
	@echo ',("Use interpreter", "$(GhcWithInterpreter)")' >> $@
	@echo ',("Support SMP", "$(GhcWithSMP)")' >> $@
	@echo ',("RTS ways", "$(GhcRTSWays)")' >> $@
	@echo ',("Tables next to code", "$(TablesNextToCode)")' >> $@
	@echo ',("Leading underscore", "$(LeadingUnderscore)")' >> $@
	@echo ',("Use LibFFI", "$(UseLibffiForAdjustors)")' >> $@
	@echo ",(\"RTS expects libdw\", \"$(GhcRtsWithLibdw)\")" >> $@
	@echo "]" >> $@


# ---------------------------------------------------------------------------
# Make DerivedConstants.h for the compiler

includes_DERIVEDCONSTANTS = rts/dist-install/build/include/DerivedConstants.h

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
endif

includes_dist-install_H_FILES_GENERATED += $(includes_DERIVEDCONSTANTS)

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

