# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

utils/mkUserGuidePart_GENERATED_FLAGS_SETS := \
  codegen                     \
  compiler-debugging          \
  cpp                         \
  finding-imports             \
  interactive                 \
  interface-files             \
  keeping-intermediates       \
  language                    \
  linking                     \
  misc                        \
  modes                       \
  optimization                \
  optimization-levels         \
  packages                    \
  phase-programs              \
  phases                      \
  phase-specific              \
  platform-specific           \
  plugin                      \
  profiling                   \
  program-coverage            \
  recompilating-checking      \
  recompilation-checking      \
  redirecting-output          \
  temporary-files             \
  verbosity                   \
  warnings

utils/mkUserGuidePart_GENERATED_RST_SOURCES := \
		$(addprefix docs/users_guide/flags-,$(addsuffix .gen.rst,$(utils/mkUserGuidePart_GENERATED_FLAGS_SETS))) \
		docs/users_guide/what_glasgow_exts_does.gen.rst \
    docs/man/all-flags.gen.rst

utils/mkUserGuidePart_USES_CABAL           = YES
utils/mkUserGuidePart_PACKAGE              = mkUserGuidePart
utils/mkUserGuidePart_dist_PROGNAME        = mkUserGuidePart
utils/mkUserGuidePart_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/mkUserGuidePart,dist,2))
$(eval $(call clean-target,utils/mkUserGuidePart,gen,$(utils/mkUserGuidePart_GENERATED_RST_SOURCES)))

$(utils/mkUserGuidePart_GENERATED_RST_SOURCES) : $(mkUserGuidePart_INPLACE)
	$(mkUserGuidePart_INPLACE)

all_utils/mkUserGuidePart: $(mkUserGuidePart_INPLACE)
