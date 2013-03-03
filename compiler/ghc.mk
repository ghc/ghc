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

# -----------------------------------------------------------------------------
# Create compiler configuration
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

# This is just to avoid generating a warning when generating deps
# involving RtsFlags.h
compiler_stage1_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES
compiler_stage2_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES
compiler_stage3_MKDEPENDC_OPTS = -DMAKING_GHC_BUILD_SYSTEM_DEPENDENCIES

compiler_stage1_C_FILES_NODEPS = compiler/parser/cutils.c

# This package doesn't pass the Cabal checks because include-dirs
# points outside the source directory. This isn't a real problem, so
# we just skip the check.
compiler_NO_CHECK = YES

ifneq "$(BINDIST)" "YES"
compiler/stage1/package-data.mk : compiler/stage1/build/Config.hs
compiler/stage2/package-data.mk : compiler/stage2/build/Config.hs
compiler/stage3/package-data.mk : compiler/stage3/build/Config.hs

compiler/stage1/build/PlatformConstants.o: $(includes_GHCCONSTANTS_HASKELL_TYPE)
compiler/stage2/build/PlatformConstants.o: $(includes_GHCCONSTANTS_HASKELL_TYPE)
compiler/stage3/build/PlatformConstants.o: $(includes_GHCCONSTANTS_HASKELL_TYPE)
compiler/stage1/build/DynFlags.o: $(includes_GHCCONSTANTS_HASKELL_EXPORTS)
compiler/stage2/build/DynFlags.o: $(includes_GHCCONSTANTS_HASKELL_EXPORTS)
compiler/stage3/build/DynFlags.o: $(includes_GHCCONSTANTS_HASKELL_EXPORTS)
compiler/stage1/build/DynFlags.o: $(includes_GHCCONSTANTS_HASKELL_WRAPPERS)
compiler/stage2/build/DynFlags.o: $(includes_GHCCONSTANTS_HASKELL_WRAPPERS)
compiler/stage3/build/DynFlags.o: $(includes_GHCCONSTANTS_HASKELL_WRAPPERS)
endif

compiler/stage%/build/Config.hs : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo 'Creating $@ ... '
	@echo '{-# LANGUAGE CPP #-}'                                        >> $@
	@echo 'module Config where'                                         >> $@
	@echo                                                               >> $@
	@echo '#include "ghc_boot_platform.h"'                              >> $@
	@echo                                                               >> $@
	@echo 'data IntegerLibrary = IntegerGMP | IntegerSimple'            >> $@
	@echo '    deriving Eq'                                             >> $@
	@echo                                                               >> $@
	@echo 'cBuildPlatformString :: String'                              >> $@
	@echo 'cBuildPlatformString = BuildPlatform_NAME'                   >> $@
	@echo 'cHostPlatformString :: String'                               >> $@
	@echo 'cHostPlatformString = HostPlatform_NAME'                     >> $@
	@echo 'cTargetPlatformString :: String'                             >> $@
	@echo 'cTargetPlatformString = TargetPlatform_NAME'                 >> $@
	@echo                                                               >> $@
	@echo 'cProjectName          :: String'                             >> $@
	@echo 'cProjectName          = "$(ProjectName)"'                    >> $@
	@echo 'cProjectVersion       :: String'                             >> $@
	@echo 'cProjectVersion       = "$(ProjectVersion)"'                 >> $@
	@echo 'cProjectVersionInt    :: String'                             >> $@
	@echo 'cProjectVersionInt    = "$(ProjectVersionInt)"'              >> $@
	@echo 'cProjectPatchLevel    :: String'                             >> $@
	@echo 'cProjectPatchLevel    = "$(ProjectPatchLevel)"'              >> $@
	@echo 'cBooterVersion        :: String'                             >> $@
	@echo 'cBooterVersion        = "$(GhcVersion)"'                     >> $@
	@echo 'cStage                :: String'                             >> $@
	@echo 'cStage                = show (STAGE :: Int)'                 >> $@
	@echo 'cIntegerLibrary       :: String'                             >> $@
	@echo 'cIntegerLibrary       = "$(INTEGER_LIBRARY)"'                >> $@
	@echo 'cIntegerLibraryType   :: IntegerLibrary'                     >> $@
ifeq "$(INTEGER_LIBRARY)" "integer-gmp"
	@echo 'cIntegerLibraryType   = IntegerGMP'                          >> $@
else ifeq "$(INTEGER_LIBRARY)" "integer-simple"
	@echo 'cIntegerLibraryType   = IntegerSimple'                       >> $@
else ifneq "$(CLEANING)" "YES"
$(error Unknown integer library)
endif
	@echo 'cSupportsSplitObjs    :: String'                             >> $@
	@echo 'cSupportsSplitObjs    = "$(SupportsSplitObjs)"'              >> $@
	@echo 'cGhcWithInterpreter   :: String'                             >> $@
	@echo 'cGhcWithInterpreter   = "$(GhcWithInterpreter)"'             >> $@
	@echo 'cGhcWithNativeCodeGen :: String'                             >> $@
	@echo 'cGhcWithNativeCodeGen = "$(GhcWithNativeCodeGen)"'           >> $@
	@echo 'cGhcWithSMP           :: String'                             >> $@
	@echo 'cGhcWithSMP           = "$(GhcWithSMP)"'                     >> $@
	@echo 'cGhcRTSWays           :: String'                             >> $@
	@echo 'cGhcRTSWays           = "$(GhcRTSWays)"'                     >> $@
	@echo 'cGhcEnableTablesNextToCode :: String'                        >> $@
	@echo 'cGhcEnableTablesNextToCode = "$(GhcEnableTablesNextToCode)"' >> $@
	@echo 'cLeadingUnderscore    :: String'                             >> $@
	@echo 'cLeadingUnderscore    = "$(LeadingUnderscore)"'              >> $@
	@echo 'cRAWCPP_FLAGS         :: String'                             >> $@
	@echo 'cRAWCPP_FLAGS         = "$(RAWCPP_FLAGS)"'                   >> $@
	@echo 'cGHC_UNLIT_PGM        :: String'                             >> $@
	@echo 'cGHC_UNLIT_PGM        = "$(utils/unlit_dist_PROG)"'          >> $@
	@echo 'cGHC_SPLIT_PGM        :: String'                             >> $@
	@echo 'cGHC_SPLIT_PGM        = "$(driver/split_dist_PROG)"'         >> $@
	@echo 'cLibFFI               :: Bool'                               >> $@
ifeq "$(UseLibFFIForAdjustors)" "YES"
	@echo 'cLibFFI               = True'                                >> $@
else
	@echo 'cLibFFI               = False'                               >> $@
endif
	@echo done.

# -----------------------------------------------------------------------------
# Create platform includes

# Here we generate a little header file containing CPP symbols that GHC
# uses to determine which platform it is building on/for.  The platforms
# can differ between stage1 and stage2 if we're cross-compiling, so we
# need one of these header files per stage.

PLATFORM_H = ghc_boot_platform.h

compiler/stage1/$(PLATFORM_H) : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __PLATFORM_H__"                           >> $@
	@echo "#define __PLATFORM_H__"                           >> $@
	@echo                                                    >> $@
	@echo "#define BuildPlatform_NAME  \"$(BUILDPLATFORM)\""  >> $@
	@echo "#define HostPlatform_NAME   \"$(HOSTPLATFORM)\""   >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD 1"              >> $@
	@echo "#define $(HostPlatform_CPP)_HOST 1"                >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET 1"            >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH 1"             >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH 1"               >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH 1"           >> $@
	@echo "#define BUILD_ARCH \"$(BuildArch_CPP)\""           >> $@
	@echo "#define HOST_ARCH \"$(HostArch_CPP)\""             >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildOS_CPP)_BUILD_OS 1"                 >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS 1"                   >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(BuildOS_CPP)\""               >> $@
	@echo "#define HOST_OS \"$(HostOS_CPP)\""                 >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
ifeq "$(TargetOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS"                   >> $@
	@echo "#define $(IRIX_MAJOR)_TARGET_OS 1"                 >> $@
	@echo "#endif"                                            >> $@
endif
	@echo                                                     >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 1"         >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR 1"           >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1"      >> $@
	@echo "#define BUILD_VENDOR \"$(BuildVendor_CPP)\""       >> $@
	@echo "#define HOST_VENDOR \"$(HostVendor_CPP)\""         >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\""     >> $@
	@echo                                                     >> $@
	@echo "#endif /* __PLATFORM_H__ */"                       >> $@
	@echo "Done."

# For stage2 and above, the BUILD platform is the HOST of stage1, and
# the HOST platform is the TARGET of stage1.  The TARGET remains the same
# (stage1 is the cross-compiler, not stage2).
compiler/stage2/$(PLATFORM_H) : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __PLATFORM_H__"                            >> $@
	@echo "#define __PLATFORM_H__"                            >> $@
	@echo                                                     >> $@
	@echo "#define BuildPlatform_NAME  \"$(HOSTPLATFORM)\""   >> $@
	@echo "#define HostPlatform_NAME   \"$(TARGETPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo                                                     >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD 1"               >> $@
	@echo "#define $(TargetPlatform_CPP)_HOST 1"              >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET 1"            >> $@
	@echo                                                     >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH 1"              >> $@
	@echo "#define $(TargetArch_CPP)_HOST_ARCH 1"             >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH 1"           >> $@
	@echo "#define BUILD_ARCH \"$(HostArch_CPP)\""            >> $@
	@echo "#define HOST_ARCH \"$(TargetArch_CPP)\""           >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS 1"                  >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS 1"                 >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(HostOS_CPP)\""                >> $@
	@echo "#define HOST_OS \"$(TargetOS_CPP)\""               >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
ifeq "$(TargetOS_CPP)" "irix"
	@echo "#ifndef $(IRIX_MAJOR)_TARGET_OS"                   >> $@
	@echo "#define $(IRIX_MAJOR)_TARGET_OS 1"                 >> $@
	@echo "#endif"                                            >> $@
endif
	@echo                                                     >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR 1"          >> $@
	@echo "#define $(TargetVendor_CPP)_HOST_VENDOR 1"         >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1"      >> $@
	@echo "#define BUILD_VENDOR \"$(HostVendor_CPP)\""        >> $@
	@echo "#define HOST_VENDOR \"$(TargetVendor_CPP)\""       >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\""     >> $@
	@echo                                                     >> $@
	@echo "#endif /* __PLATFORM_H__ */"                       >> $@
	@echo "Done."

compiler/stage3/$(PLATFORM_H) : compiler/stage2/$(PLATFORM_H)
	"$(CP)" $< $@

# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.lhs 
#		from prelude/primops.txt

PRIMOP_BITS_NAMES = primop-data-decl.hs-incl        \
                    primop-tag.hs-incl              \
                    primop-list.hs-incl             \
                    primop-has-side-effects.hs-incl \
                    primop-out-of-line.hs-incl      \
                    primop-commutable.hs-incl       \
                    primop-code-size.hs-incl        \
                    primop-can-fail.hs-incl         \
                    primop-strictness.hs-incl       \
                    primop-fixity.hs-incl       \
                    primop-primop-info.hs-incl

PRIMOP_BITS_STAGE1 = $(addprefix compiler/stage1/build/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE2 = $(addprefix compiler/stage2/build/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE3 = $(addprefix compiler/stage3/build/,$(PRIMOP_BITS_NAMES))

compiler_CPP_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))
compiler_CPP_OPTS += ${GhcCppOpts}

define preprocessCompilerFiles
# $0 = stage
compiler/stage$1/build/Parser.y: compiler/parser/Parser.y.pp
	$$(CPP) $$(RAWCPP_FLAGS) -P $$(compiler_CPP_OPTS) -x c $$< | grep -v '^#pragma GCC' > $$@

compiler/stage$1/build/primops.txt: compiler/prelude/primops.txt.pp compiler/stage$1/$$(PLATFORM_H)
	$$(CPP) $$(RAWCPP_FLAGS) -P $$(compiler_CPP_OPTS) -Icompiler/stage$1 -x c $$< | grep -v '^#pragma GCC' > $$@

compiler/stage$1/build/primop-data-decl.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --data-decl          < $$< > $$@
compiler/stage$1/build/primop-tag.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-tag         < $$< > $$@
compiler/stage$1/build/primop-list.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-list        < $$< > $$@
compiler/stage$1/build/primop-has-side-effects.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --has-side-effects   < $$< > $$@
compiler/stage$1/build/primop-out-of-line.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --out-of-line        < $$< > $$@
compiler/stage$1/build/primop-commutable.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --commutable         < $$< > $$@
compiler/stage$1/build/primop-code-size.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --code-size          < $$< > $$@
compiler/stage$1/build/primop-can-fail.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --can-fail           < $$< > $$@
compiler/stage$1/build/primop-strictness.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --strictness         < $$< > $$@
compiler/stage$1/build/primop-fixity.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --fixity             < $$< > $$@
compiler/stage$1/build/primop-primop-info.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-primop-info < $$< > $$@

# Usages aren't used any more; but the generator 
# can still generate them if we want them back
compiler/stage$1/build/primop-usage.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --usage              < $$< > $$@

endef

$(eval $(call preprocessCompilerFiles,1))
$(eval $(call preprocessCompilerFiles,2))
$(eval $(call preprocessCompilerFiles,3))

# -----------------------------------------------------------------------------
# Configuration

compiler_stage1_CONFIGURE_OPTS += --flags=stage1
compiler_stage2_CONFIGURE_OPTS += --flags=stage2
compiler_stage3_CONFIGURE_OPTS += --flags=stage3

ifeq "$(GhcWithNativeCodeGen)" "YES"
compiler_stage1_CONFIGURE_OPTS += --flags=ncg
compiler_stage2_CONFIGURE_OPTS += --flags=ncg
endif

ifeq "$(GhcWithInterpreter)" "YES"
compiler_stage2_CONFIGURE_OPTS += --flags=ghci

ifeq "$(BuildSharedLibs)" "YES"
# There are too many symbols to make a Windows DLL for the ghc package,
# so we don't build it the dyn way; see trac #5987
ifneq "$(TargetOS_CPP)" "mingw32"
compiler_stage2_CONFIGURE_OPTS += --enable-shared
endif
endif

ifeq "$(GhcEnableTablesNextToCode) $(GhcUnregisterised)" "YES NO"
# Should GHCI be building info tables in the TABLES_NEXT_TO_CODE style
# or not?
# XXX This should logically be a CPP option, but there doesn't seem to
# be a flag for that
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-DGHCI_TABLES_NEXT_TO_CODE
endif

# Should the debugger commands be enabled?
ifeq "$(GhciWithDebugger)" "YES"
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-DDEBUGGER
endif

endif

ifeq "$(TargetOS_CPP)" "openbsd"
compiler_CONFIGURE_OPTS += --ld-options=-E
endif

ifeq "$(GhcUnregisterised)" "NO"
else
compiler_CONFIGURE_OPTS += --ghc-option=-DNO_REGS
endif

ifeq "$(GhcProfiled)" "YES"

# If we're profiling GHC then we want SCCs.  However, adding -auto-all
# everywhere tends to give a hard-to-read profile, and adds lots of
# overhead.  A better approach is to proceed top-down; identify the
# parts of the compiler of interest, and then add further cost centres
# as necessary.  Turn on -auto-all for individual modules like this:

# compiler/main/DriverPipeline_HC_OPTS += -auto-all
compiler/main/GhcMake_HC_OPTS        += -auto-all
compiler/main/GHC_HC_OPTS            += -auto-all

# or alternatively add {-# OPTIONS_GHC -auto-all #-} to the top of
# modules you're interested in.

# We seem to still build the vanilla libraries even if we say
# --disable-library-vanilla, but installation then fails, as Cabal
# doesn't copy the vanilla .hi files, but ghc-pkg complains about
# their absence when we register the package. So for now, we just
# leave the vanilla libraries enabled.
# compiler_stage2_CONFIGURE_OPTS += --disable-library-vanilla
compiler_stage2_CONFIGURE_OPTS += --ghc-pkg-option=--force
endif

compiler_stage3_CONFIGURE_OPTS := $(compiler_stage2_CONFIGURE_OPTS)

compiler_stage1_CONFIGURE_OPTS += --ghc-option=-DSTAGE=1
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-DSTAGE=2
compiler_stage3_CONFIGURE_OPTS += --ghc-option=-DSTAGE=3
compiler_stage2_HADDOCK_OPTS += --optghc=-DSTAGE=2

compiler/stage1/package-data.mk : compiler/ghc.mk
compiler/stage2/package-data.mk : compiler/ghc.mk
compiler/stage3/package-data.mk : compiler/ghc.mk

# -----------------------------------------------------------------------------
# And build the package

compiler_PACKAGE = ghc

# Note [fiddle-stage1-version]
# The version of the GHC package changes every day, since the
# patchlevel is the current date.  We don't want to force
# recompilation of the entire compiler when this happens, so for stage
# 1 we omit the patchlevel from the version number.  For stage 2 we
# have to include the patchlevel since this is the package we install,
# however.
#
# Note: we also have to tweak the version number of the package itself
# when it gets registered; see Note [munge-stage1-package-config]
# below.
# The ProjectPatchLevel > 20000000 iff it's a date. If it's e.g. 6.12.1
# then we don't want to remove it
ifneq "$(CLEANING)" "YES"
ifeq "$(shell [ $(ProjectPatchLevel) -gt 20000000 ] && echo YES)" "YES"
compiler_stage1_VERSION_MUNGED = YES
endif
endif

ifeq "$(compiler_stage1_VERSION_MUNGED)" "YES"
compiler_stage1_MUNGED_VERSION = $(subst .$(ProjectPatchLevel),,$(ProjectVersion))
define compiler_PACKAGE_MAGIC
compiler_stage1_VERSION = $(compiler_stage1_MUNGED_VERSION)
endef

# Don't register the non-munged package
compiler_stage1_REGISTER_PACKAGE = NO

endif

# Don't do splitting for the GHC package, it takes too long and
# there's not much benefit.
compiler_stage1_SplitObjs = NO
compiler_stage2_SplitObjs = NO
compiler_stage3_SplitObjs = NO

ifeq "$(TargetOS_CPP)" "mingw32"
# There are too many symbols to make a Windows DLL for the ghc package,
# so we don't build it the dyn way; see trac #5987
compiler_stage1_EXCLUDED_WAYS := dyn
compiler_stage2_EXCLUDED_WAYS := dyn
compiler_stage3_EXCLUDED_WAYS := dyn
endif

# if stage is set to something other than "1" or "", disable stage 1
ifneq "$(filter-out 1,$(stage))" ""
compiler_stage1_NOT_NEEDED = YES
endif
# if stage is set to something other than "2" or "", disable stage 2
ifneq "$(filter-out 2,$(stage))" ""
compiler_stage2_NOT_NEEDED = YES
endif
# stage 3 has to be requested explicitly with stage=3
ifneq "$(stage)" "3"
compiler_stage3_NOT_NEEDED = YES
endif
$(eval $(call build-package,compiler,stage1,0))
$(eval $(call build-package,compiler,stage2,1))
$(eval $(call build-package,compiler,stage3,2))

# We only want to turn keepCAFs on if we will be loading dynamic
# Haskell libraries with GHCi. We therefore filter the object file
# out for non-dynamic ways.
define keepCAFsForGHCiDynOnly
# $1 = stage
# $2 = way
ifeq "$$(findstring dyn, $1)" ""
compiler_stage$1_$2_C_OBJS := $$(filter-out %/keepCAFsForGHCi.o,$$(compiler_stage$1_$2_C_OBJS))
endif
endef
$(foreach w,$(compiler_stage1_WAYS),$(eval $(call keepCAFsForGHCiDynOnly,1,$w)))
$(foreach w,$(compiler_stage2_WAYS),$(eval $(call keepCAFsForGHCiDynOnly,2,$w)))
$(foreach w,$(compiler_stage3_WAYS),$(eval $(call keepCAFsForGHCiDynOnly,3,$w)))

# after build-package, because that adds --enable-library-for-ghci
# to compiler_stage*_CONFIGURE_OPTS:
# We don't build the GHCi library for the ghc package. We can load it
# the .a file instead, and as object splitting isn't on for the ghc
# package this isn't much slower.However, not building the package saves
# a significant chunk of disk space.
compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci
compiler_stage2_CONFIGURE_OPTS += --disable-library-for-ghci
compiler_stage3_CONFIGURE_OPTS += --disable-library-for-ghci

# after build-package, because that sets compiler_stage1_HC_OPTS:
compiler_stage1_HC_OPTS += $(GhcStage1HcOpts)
compiler_stage2_HC_OPTS += $(GhcStage2HcOpts)
compiler_stage3_HC_OPTS += $(GhcStage3HcOpts)

ifneq "$(BINDIST)" "YES"

compiler_stage2_TAGS_HC_OPTS = -package ghc
$(eval $(call tags-package,compiler,stage2))

$(compiler_stage1_depfile_haskell) : compiler/stage1/$(PLATFORM_H)
$(compiler_stage2_depfile_haskell) : compiler/stage2/$(PLATFORM_H)
$(compiler_stage3_depfile_haskell) : compiler/stage3/$(PLATFORM_H)

COMPILER_INCLUDES_DEPS += $(includes_H_CONFIG)
COMPILER_INCLUDES_DEPS += $(includes_H_PLATFORM)
COMPILER_INCLUDES_DEPS += $(includes_GHCCONSTANTS)
COMPILER_INCLUDES_DEPS += $(includes_GHCCONSTANTS_HASKELL_TYPE)
COMPILER_INCLUDES_DEPS += $(includes_GHCCONSTANTS_HASKELL_WRAPPERS)
COMPILER_INCLUDES_DEPS += $(includes_GHCCONSTANTS_HASKELL_EXPORTS)
COMPILER_INCLUDES_DEPS += $(includes_DERIVEDCONSTANTS)

$(compiler_stage1_depfile_haskell) : $(COMPILER_INCLUDES_DEPS) $(PRIMOP_BITS_STAGE1)
$(compiler_stage2_depfile_haskell) : $(COMPILER_INCLUDES_DEPS) $(PRIMOP_BITS_STAGE2)
$(compiler_stage3_depfile_haskell) : $(COMPILER_INCLUDES_DEPS) $(PRIMOP_BITS_STAGE3)

$(foreach way,$(compiler_stage1_WAYS),\
      compiler/stage1/build/PrimOp.$($(way)_osuf)) : $(PRIMOP_BITS_STAGE1)
$(foreach way,$(compiler_stage2_WAYS),\
      compiler/stage2/build/PrimOp.$($(way)_osuf)) : $(PRIMOP_BITS_STAGE2)
$(foreach way,$(compiler_stage3_WAYS),\
      compiler/stage3/build/PrimOp.$($(way)_osuf)) : $(PRIMOP_BITS_STAGE3)


# GHC itself doesn't know about the above dependencies, so we have to
# switch off the recompilation checker for that module:
compiler/prelude/PrimOp_HC_OPTS  += -fforce-recomp

# LibFFI.hs #includes ffi.h
ifneq "$(UseSystemLibFFI)" "YES"
compiler/stage2/build/LibFFI.hs : $(libffi_HEADERS)
endif

# On Windows it seems we also need to link directly to libffi
ifeq "$(HostOS_CPP)" "mingw32"
define windowsDynLinkToFfi
# $1 = way
ifneq "$$(findstring dyn, $1)" ""
compiler_stage2_$1_ALL_HC_OPTS += -l$$(LIBFFI_WINDOWS_LIB)
endif
endef
$(foreach way,$(GhcLibWays),$(eval $(call windowsDynLinkToFfi,$(way))))
endif

# Note [munge-stage1-package-config]
# Strip the date/patchlevel from the version of stage1.  See Note
# [fiddle-stage1-version] above.
ifeq "$(compiler_stage1_VERSION_MUNGED)" "YES"
compiler/stage1/inplace-pkg-config-munged: compiler/stage1/inplace-pkg-config
	sed -e 's/^\(version: .*\)\.$(ProjectPatchLevel)$$/\1/' \
	    -e 's/^\(id: .*\)\.$(ProjectPatchLevel)$$/\1/' \
	    -e 's/^\(hs-libraries: HSghc-.*\)\.$(ProjectPatchLevel)$$/\1/' \
	  < $< > $@
	"$(compiler_stage1_GHC_PKG)" update --force $(compiler_stage1_GHC_PKG_OPTS) $@

# We need to make sure the munged config is in the database before we
# try to configure ghc-bin
ghc/stage1/package-data.mk : compiler/stage1/inplace-pkg-config-munged
endif

endif

