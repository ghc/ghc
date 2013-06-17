# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

#
# Header files built from the configure script's findings
#
# XXX: these should go in includes/dist/build?
includes_H_CONFIG   = includes/ghcautoconf.h
includes_H_PLATFORM = includes/ghcplatform.h

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
ifeq "$(HostOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_HOST_OS" >> $@  
	@echo "#define $(IRIX_MAJOR)_HOST_OS  1" >> $@  
	@echo "#endif" >> $@  
endif
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

# ---------------------------------------------------------------------------
# Make DerivedConstants.h for the compiler

includes_DERIVEDCONSTANTS = includes/dist-derivedconstants/header/DerivedConstants.h
includes_GHCCONSTANTS_HASKELL_TYPE = includes/dist-derivedconstants/header/GHCConstantsHaskellType.hs
includes_GHCCONSTANTS_HASKELL_VALUE = includes/dist-derivedconstants/header/platformConstants
includes_GHCCONSTANTS_HASKELL_WRAPPERS = includes/dist-derivedconstants/header/GHCConstantsHaskellWrappers.hs
includes_GHCCONSTANTS_HASKELL_EXPORTS = includes/dist-derivedconstants/header/GHCConstantsHaskellExports.hs

INSTALL_LIBS += $(includes_GHCCONSTANTS_HASKELL_VALUE)

DERIVE_CONSTANTS_FLAGS += --gcc-program "$(WhatGccIsCalled)"
DERIVE_CONSTANTS_FLAGS += $(addprefix --gcc-flag$(space),$(includes_CC_OPTS) -fcommon)
DERIVE_CONSTANTS_FLAGS += --nm-program "$(NM)"

ifneq "$(BINDIST)" "YES"
$(includes_DERIVEDCONSTANTS):           $$(includes_H_CONFIG) $$(includes_H_PLATFORM) $$(includes_H_FILES) $$(rts_H_FILES)
$(includes_GHCCONSTANTS_HASKELL_VALUE): $$(includes_H_CONFIG) $$(includes_H_PLATFORM) $$(includes_H_FILES) $$(rts_H_FILES)

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
  $(includes_H_CONFIG) $(includes_H_PLATFORM)))

$(eval $(call all-target,includes,\
  $(includes_H_CONFIG) $(includes_H_PLATFORM) \
  $(includes_GHCCONSTANTS_HASKELL_TYPE) \
  $(includes_GHCCONSTANTS_HASKELL_VALUE) \
  $(includes_GHCCONSTANTS_HASKELL_WRAPPERS) \
  $(includes_GHCCONSTANTS_HASKELL_EXPORTS) \
  $(includes_DERIVEDCONSTANTS)))

install: install_includes

.PHONY: install_includes
install_includes :
	$(call INSTALL_DIR,"$(DESTDIR)$(ghcheaderdir)")
	$(foreach d,$(includes_H_SUBDIRS), \
	    $(call INSTALL_DIR,"$(DESTDIR)$(ghcheaderdir)/$d") && \
	    $(call INSTALL_HEADER,$(INSTALL_OPTS),includes/$d/*.h,"$(DESTDIR)$(ghcheaderdir)/$d/") && \
	) true
	$(call INSTALL_HEADER,$(INSTALL_OPTS),$(includes_H_CONFIG) $(includes_H_PLATFORM) $(includes_DERIVEDCONSTANTS),"$(DESTDIR)$(ghcheaderdir)/")

