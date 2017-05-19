# -----------------------------------------------------------------------------
#
# (c) 2009-2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
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
	@echo 'data IntegerLibrary = IntegerGMP'                            >> $@
	@echo '                    | IntegerSimple'                         >> $@
	@echo '                    deriving Eq'                             >> $@
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
	@echo 'cProjectGitCommitId   :: String'				    >> $@
	@echo 'cProjectGitCommitId   = "$(ProjectGitCommitId)"'		    >> $@
	@echo 'cProjectVersion       :: String'                             >> $@
	@echo 'cProjectVersion       = "$(ProjectVersion)"'                 >> $@
	@echo 'cProjectVersionInt    :: String'                             >> $@
	@echo 'cProjectVersionInt    = "$(ProjectVersionInt)"'              >> $@
	@echo 'cProjectPatchLevel    :: String'                             >> $@
	@echo 'cProjectPatchLevel    = "$(ProjectPatchLevel)"'              >> $@
	@echo 'cProjectPatchLevel1   :: String'                             >> $@
	@echo 'cProjectPatchLevel1   = "$(ProjectPatchLevel1)"'             >> $@
	@echo 'cProjectPatchLevel2   :: String'                             >> $@
	@echo 'cProjectPatchLevel2   = "$(ProjectPatchLevel2)"'             >> $@
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
	@echo 'cGhcRtsWithLibdw      :: Bool'                               >> $@
ifeq "$(GhcRtsWithLibdw)" "YES"
	@echo 'cGhcRtsWithLibdw      = True'                                >> $@
else
	@echo 'cGhcRtsWithLibdw      = False'                               >> $@
endif
	@echo 'cGhcEnableTablesNextToCode :: String'                        >> $@
	@echo 'cGhcEnableTablesNextToCode = "$(GhcEnableTablesNextToCode)"' >> $@
	@echo 'cLeadingUnderscore    :: String'                             >> $@
	@echo 'cLeadingUnderscore    = "$(LeadingUnderscore)"'              >> $@
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
# Note that GhcThreaded just reflects the Makefile variable setting.
# In particular, the stage1 compiler is never actually compiled with
# -threaded, but it will nevertheless have cGhcThreaded = True.
# The "+RTS --info" output will show what RTS GHC is really using.
	@echo 'cGhcThreaded :: Bool'                                        >> $@
ifeq "$(GhcThreaded)" "YES"
	@echo 'cGhcThreaded = True'                                         >> $@
else
	@echo 'cGhcThreaded = False'                                        >> $@
endif
	@echo 'cGhcDebugged :: Bool'                                        >> $@
ifeq "$(GhcDebugged)" "YES"
	@echo 'cGhcDebugged = True'                                         >> $@
else
	@echo 'cGhcDebugged = False'                                        >> $@
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
	@echo "#define LLVM_TARGET \"$(LLVMTarget_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildOS_CPP)_BUILD_OS 1"                 >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS 1"                   >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(BuildOS_CPP)\""               >> $@
	@echo "#define HOST_OS \"$(HostOS_CPP)\""                 >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
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
	@echo "#define LLVM_TARGET \"$(LLVMTarget_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS 1"                  >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS 1"                 >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(HostOS_CPP)\""                >> $@
	@echo "#define HOST_OS \"$(TargetOS_CPP)\""               >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
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
#		Generate supporting stuff for prelude/PrimOp.hs
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
                    primop-fixity.hs-incl           \
                    primop-primop-info.hs-incl      \
                    primop-vector-uniques.hs-incl   \
                    primop-vector-tys.hs-incl       \
                    primop-vector-tys-exports.hs-incl \
                    primop-vector-tycons.hs-incl

PRIMOP_BITS_STAGE1 = $(addprefix compiler/stage1/build/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE2 = $(addprefix compiler/stage2/build/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE3 = $(addprefix compiler/stage3/build/,$(PRIMOP_BITS_NAMES))

compiler_CPP_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))
compiler_CPP_OPTS += ${GhcCppOpts}

# We add these paths to the Haskell compiler's #include search path list since
# we must avoid #including files by paths relative to the source file as Hadrian
# moves the build artifacts out of the source tree. See #8040.
compiler_HC_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))

define preprocessCompilerFiles
# $0 = stage
compiler/stage$1/build/primops.txt: compiler/prelude/primops.txt.pp compiler/stage$1/$$(PLATFORM_H)
	$$(HS_CPP) -P $$(compiler_CPP_OPTS) -Icompiler/stage$1 -x c $$< | grep -v '^#pragma GCC' > $$@

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
compiler/stage$1/build/primop-vector-uniques.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-uniques     < $$< > $$@
compiler/stage$1/build/primop-vector-tys.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-tys         < $$< > $$@
compiler/stage$1/build/primop-vector-tys-exports.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-tys-exports < $$< > $$@
compiler/stage$1/build/primop-vector-tycons.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-tycons      < $$< > $$@

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

ifeq "$(GhcThreaded)" "YES"
# We pass THREADED_RTS to the stage2 C files so that cbits/genSym.c will bring
# the threaded version of atomic_inc() into scope.
compiler_stage2_CONFIGURE_OPTS += --ghc-option=-optc-DTHREADED_RTS
endif

ifeq "$(GhcWithNativeCodeGen)" "YES"
compiler_stage1_CONFIGURE_OPTS += --flags=ncg
compiler_stage2_CONFIGURE_OPTS += --flags=ncg
endif

ifeq "$(GhcWithInterpreter)" "YES"
compiler_stage2_CONFIGURE_OPTS += --flags=ghci

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

ifneq "$(GhcWithSMP)" "YES"
compiler_CONFIGURE_OPTS += --ghc-option=-DNOSMP
compiler_CONFIGURE_OPTS += --ghc-option=-optc-DNOSMP
endif

ifeq "$(WITH_TERMINFO)" "NO"
compiler_stage2_CONFIGURE_OPTS += --flags=-terminfo
endif

# Careful optimisation of the parser: we don't want to throw everything
# at it, because that takes too long and doesn't buy much, but we do want
# to inline certain key external functions, so we instruct GHC not to
# throw away inlinings as it would normally do in -O0 mode.
# Since GHC version 7.8, we need -fcmm-sink to be
# passed to the compiler. This is required on x86 to avoid the
# register allocator running out of stack slots when compiling this
# module with -fPIC -dynamic.
# See #8182 for all the details
compiler/stage1/build/Parser_HC_OPTS += -O0 -fno-ignore-interface-pragmas -fcmm-sink
compiler/stage2/build/Parser_HC_OPTS += -O0 -fno-ignore-interface-pragmas -fcmm-sink
compiler/stage3/build/Parser_HC_OPTS += -O0 -fno-ignore-interface-pragmas -fcmm-sink

ifeq "$(GhcProfiled)" "YES"
# If we're profiling GHC then we want SCCs.  However, adding -auto-all
# everywhere tends to give a hard-to-read profile, and adds lots of
# overhead.  A better approach is to proceed top-down; identify the
# parts of the compiler of interest, and then add further cost centres
# as necessary.  Turn on -fprof-auto for individual modules like this:

# compiler/main/DriverPipeline_HC_OPTS += -fprof-auto
compiler/main/GhcMake_HC_OPTS        += -fprof-auto
compiler/main/GHC_HC_OPTS            += -fprof-auto

# or alternatively add {-# OPTIONS_GHC -fprof-auto #-} to the top of
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

# Don't do splitting for the GHC package, it takes too long and
# there's not much benefit.
compiler_stage1_SplitObjs = NO
compiler_stage2_SplitObjs = NO
compiler_stage3_SplitObjs = NO
compiler_stage1_SplitSections = NO
compiler_stage2_SplitSections = NO
compiler_stage3_SplitSections = NO

# There are too many symbols in the ghc package for a Windows DLL
# (due to a limitation of bfd ld, see Trac #5987). We therefore need to split
# some of the modules off into a separate DLL. This clump are the modules
# reachable from DynFlags:
compiler_stage2_dll0_START_MODULE = DynFlags
compiler_stage2_dll0_MODULES = \
	Annotations \
	ApiAnnotation \
	Avail \
	Bag \
	BasicTypes \
	Binary \
	BinFingerprint \
	BooleanFormula \
	BufWrite \
	ByteCodeTypes \
	Class \
	CmdLineParser \
	CmmType \
	CoAxiom \
	ConLike \
	Coercion \
	Config \
	Constants \
	CoreArity \
	CoreFVs \
	CoreSubst \
	CoreOpt \
	CoreSyn \
	CoreTidy \
	CoreUnfold \
	CoreUtils \
	CoreSeq \
	CoreStats \
	CostCentre \
	DataCon \
	Demand \
	Digraph \
	DriverPhases \
	DynFlags \
	Encoding \
	EnumSet \
	ErrUtils \
	Exception \
	FamInstEnv \
	FastFunctions \
	FastMutInt \
	FastString \
	FastStringEnv \
	FieldLabel \
	Fingerprint \
	FiniteMap \
	ForeignCall \
	FV \
	Hooks \
	HsBinds \
	HsDecls \
	HsDoc \
	HsExpr \
	HsImpExp \
	HsLit \
	PlaceHolder \
	HsExtension \
	PmExpr \
	HsPat \
	HsSyn \
	HsTypes \
	HsUtils \
	HscTypes \
	IOEnv \
	NameCache \
	Id \
	IdInfo \
	IfaceSyn \
	IfaceType \
	InteractiveEvalTypes \
	Json \
	ToIface \
	InstEnv \
	Kind \
	KnownUniques \
	Lexeme \
	ListSetOps \
	Literal \
	Maybes \
	MkCore \
	MkId \
	Module \
	MonadUtils \
	Name \
	NameEnv \
	NameSet \
	OccName \
	OccurAnal \
	OptCoercion \
	OrdList \
	Outputable \
	PackageConfig \
	Packages \
	Pair \
	Panic \
	PatSyn \
	PipelineMonad \
	Platform \
	PlatformConstants \
	PprColour \
	PprCore \
	PrelNames \
	PrelRules \
	Pretty \
	PrimOp \
	RepType \
	RdrName \
	Rules \
	SrcLoc \
	StringBuffer \
	SysTools.Terminal \
	TcEvidence \
	TcRnTypes \
	TcType \
	TrieMap \
	TyCon \
	Type \
	TyCoRep \
	TysPrim \
	TysWiredIn \
	Unify \
	UniqDFM \
	UniqDSet \
	UniqFM \
	UniqSet \
	UniqSupply \
	Unique \
	Util \
	Var \
	VarEnv \
	VarSet

ifeq "$(GhcWithInterpreter)" "YES"
# These files are reacheable from DynFlags
# only by GHCi-enabled code (see #9552)
compiler_stage2_dll0_MODULES += # none
endif

compiler_stage2_dll0_HS_OBJS = \
    $(patsubst %,compiler/stage2/build/%.$(dyn_osuf),$(subst .,/,$(compiler_stage2_dll0_MODULES)))

# if stage is set to something other than "1" or "", disable stage 1
# See Note [Stage1Only vs stage=1] in mk/config.mk.in.
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
ifeq "$$(findstring dyn, $2)" ""
compiler_stage$1_$2_C_OBJS := $$(filter-out %/keepCAFsForGHCi.$$($2_osuf),$$(compiler_stage$1_$2_C_OBJS))
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
ifeq "$(V)" "0"
compiler_stage1_HC_OPTS += $(filter-out -Rghc-timing,$(GhcHcOpts)) $(GhcStage1HcOpts)
compiler_stage2_HC_OPTS += $(filter-out -Rghc-timing,$(GhcHcOpts)) $(GhcStage2HcOpts)
compiler_stage3_HC_OPTS += $(filter-out -Rghc-timing,$(GhcHcOpts)) $(GhcStage3HcOpts)
else
compiler_stage1_HC_OPTS += $(GhcHcOpts) $(GhcStage1HcOpts)
compiler_stage2_HC_OPTS += $(GhcHcOpts) $(GhcStage2HcOpts)
compiler_stage3_HC_OPTS += $(GhcHcOpts) $(GhcStage3HcOpts)
endif

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

ifeq "$(DYNAMIC_GHC_PROGRAMS)" "YES"
compiler/utils/Util_HC_OPTS += -DDYNAMIC_GHC_PROGRAMS
endif

endif
