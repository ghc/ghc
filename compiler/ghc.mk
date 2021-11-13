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

# -----------------------------------------------------------------------------
# Create compiler configuration
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

compiler_stage1_C_FILES_NODEPS = compiler/cbits/cutils.c

# TODO(@Ericson2314) Get rid of compiler-specific stage indices. I think the
# argument was stage n ghc is used to build stage n everything else, but I
# don't buy that argument.

ifneq "$(BINDIST)" "YES"

$(foreach n,1 2 3, \
    $(eval compiler/stage$n/package-data.mk : compiler/stage$n/build/GHC/Settings/Config.hs) \
    $(eval compiler/stage$n/build/GHC/Platform/Constants.o: compiler/stage$n/build/GHC/Platform/Constants.hs) \
  )
endif

BUILDPLATFORM_1 = $(BUILDPLATFORM)
BUILDPLATFORM_2 = $(HOSTPLATFORM)
BUILDPLATFORM_3 = $(TARGETPLATFORM)

HOSTPLATFORM_1 = $(HOSTPLATFORM)
HOSTPLATFORM_2 = $(TARGETPLATFORM)
HOSTPLATFORM_3 = $(TARGETPLATFORM)

define compilerConfig
# $1 = compile stage (1-indexed)
compiler/stage$1/build/GHC/Settings/Config.hs : mk/config.mk mk/project.mk | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	@echo 'Creating $$@ ... '
	@echo 'module GHC.Settings.Config'                                  >> $$@
	@echo '  ( module GHC.Version'                                      >> $$@
	@echo '  , cBuildPlatformString'                                    >> $$@
	@echo '  , cHostPlatformString'                                     >> $$@
	@echo '  , cProjectName'                                            >> $$@
	@echo '  , cBooterVersion'                                          >> $$@
	@echo '  , cStage'                                                  >> $$@
	@echo '  ) where'                                                   >> $$@
	@echo                                                               >> $$@
	@echo 'import GHC.Prelude'                                          >> $$@
	@echo                                                               >> $$@
	@echo 'import GHC.Version'                                          >> $$@
	@echo                                                               >> $$@
	@echo 'cBuildPlatformString :: String'                              >> $$@
	@echo 'cBuildPlatformString = "$(BUILDPLATFORM_$1)"'                >> $$@
	@echo                                                               >> $$@
	@echo 'cHostPlatformString :: String'                               >> $$@
	@echo 'cHostPlatformString = "$(HOSTPLATFORM_$1)"'                  >> $$@
	@echo                                                               >> $$@
	@echo 'cProjectName          :: String'                             >> $$@
	@echo 'cProjectName          = "$(ProjectName)"'                    >> $$@
	@echo                                                               >> $$@
	@echo 'cBooterVersion        :: String'                             >> $$@
	@echo 'cBooterVersion        = "$(GhcVersion)"'                     >> $$@
	@echo                                                               >> $$@
	@echo 'cStage                :: String'                             >> $$@
	@echo 'cStage                = show ($1 :: Int)'                    >> $$@
	@echo done.

compiler/stage$1/build/GHC/Platform/Constants.hs : $$(deriveConstants_INPLACE) | $$$$(dir $$$$@)/.
	$$< --gen-haskell-type -o $$@
endef

$(eval $(call compilerConfig,1))
$(eval $(call compilerConfig,2))
$(eval $(call compilerConfig,3))

# ----------------------------------------------------------------------------
#		Generate supporting stuff for GHC/Builtin/PrimOps.hs
#		from GHC/Builtin/primops.txt

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
                    primop-vector-tycons.hs-incl    \
                    primop-docs.hs-incl

PRIMOP_BITS_STAGE1 = $(addprefix compiler/stage1/build/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE2 = $(addprefix compiler/stage2/build/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE3 = $(addprefix compiler/stage3/build/,$(PRIMOP_BITS_NAMES))

define preprocessCompilerFiles
# $1 = compiler stage (build system stage + 1)
compiler/stage$1/build/primops.txt: \
		compiler/GHC/Builtin/primops.txt.pp
	$$(HS_CPP) -P \
		-x c $$< | grep -v '^#pragma GCC' > $$@

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
compiler/stage$1/build/primop-docs.hs-incl: compiler/stage$1/build/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --wired-in-docs             < $$< > $$@

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

ifeq "$(GhcWithInterpreter)" "YES"
compiler_stage2_CONFIGURE_OPTS += --flags=internal-interpreter

endif

ifeq "$(TargetOS_CPP)" "openbsd"
compiler_CONFIGURE_OPTS += --ld-options=-E
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

# compiler/GHC/Driver/Pipeline_HC_OPTS += -fprof-auto
compiler/GHC/Driver/Make_HC_OPTS     += -fprof-auto
compiler/GHC_HC_OPTS                 += -fprof-auto

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

compiler/stage1/package-data.mk : compiler/ghc.mk
compiler/stage2/package-data.mk : compiler/ghc.mk
compiler/stage3/package-data.mk : compiler/ghc.mk

# -----------------------------------------------------------------------------
# And build the package

compiler_PACKAGE = ghc

# Don't do splitting for the GHC package, it takes too long and
# there's not much benefit.
compiler_stage1_SplitSections = NO
compiler_stage2_SplitSections = NO
compiler_stage3_SplitSections = NO

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

$(compiler_stage1_depfile_haskell) : $(COMPILER_INCLUDES_DEPS) $(PRIMOP_BITS_STAGE1)
$(compiler_stage2_depfile_haskell) : $(COMPILER_INCLUDES_DEPS) $(PRIMOP_BITS_STAGE2)
$(compiler_stage3_depfile_haskell) : $(COMPILER_INCLUDES_DEPS) $(PRIMOP_BITS_STAGE3)

$(foreach way,$(compiler_stage1_WAYS),\
      compiler/stage1/build/PrimOp.$($(way)_osuf)) : $(PRIMOP_BITS_STAGE1)
$(foreach way,$(compiler_stage2_WAYS),\
      compiler/stage2/build/PrimOp.$($(way)_osuf)) : $(PRIMOP_BITS_STAGE2)
$(foreach way,$(compiler_stage3_WAYS),\
      compiler/stage3/build/PrimOp.$($(way)_osuf)) : $(PRIMOP_BITS_STAGE3)

endif
