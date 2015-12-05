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
  recompilation-checking      \
  redirecting-output          \
  temporary-files             \
  verbosity                   \
  warnings

# See Note [Blessed make target file]
utils/mkUserGuidePart_GENERATED_RST_SOURCES_BLESSED_FILE := \
		docs/users_guide/what_glasgow_exts_does.gen.rst

utils/mkUserGuidePart_GENERATED_RST_SOURCES_OTHER_FILES := \
		$(addprefix docs/users_guide/flags-,$(addsuffix .gen.rst,$(utils/mkUserGuidePart_GENERATED_FLAGS_SETS))) \
		docs/users_guide/all-flags.gen.rst

utils/mkUserGuidePart_GENERATED_RST_SOURCES := \
		$(utils/mkUserGuidePart_GENERATED_RST_SOURCES_BLESSED_FILE) \
		$(utils/mkUserGuidePart_GENERATED_RST_SOURCES_OTHER_FILES)

utils/mkUserGuidePart_USES_CABAL           = YES
utils/mkUserGuidePart_PACKAGE              = mkUserGuidePart
utils/mkUserGuidePart_dist_PROGNAME        = mkUserGuidePart
utils/mkUserGuidePart_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/mkUserGuidePart,dist,2))
$(eval $(call clean-target,utils/mkUserGuidePart,gen,$(utils/mkUserGuidePart_GENERATED_RST_SOURCES)))

$(utils/mkUserGuidePart_GENERATED_RST_SOURCES_OTHER_FILES) :

$(utils/mkUserGuidePart_GENERATED_RST_SOURCES_BLESSED_FILE) : $(utils/mkUserGuidePart_GENERATED_RST_SOURCES_OTHER_FILES) $(mkUserGuidePart_INPLACE)
	$(mkUserGuidePart_INPLACE)
	$(TOUCH_CMD) $@

all_utils/mkUserGuidePart: $(mkUserGuidePart_INPLACE)

# Note [Blessed make target file]
#
# make cannot express nicely a single build rule
# with multiple targets:
#
#   > all: a b
#   > a b:
#   > 	touch a b
#
# This code will run 'touch' rule twice when parallel
# make is used:
#   > $ make -j
#   > touch a b
#   > touch a b
#
# But there is a workaround for it:
# We pick a single file of a group and depend on it
# as an ultimate target. We also need to make sure
# that file has latest timestamp in the group:
#
#   > all: a b
#   > b:
#   > a: b
#   > 	touch a b
#   > 	touch $@
