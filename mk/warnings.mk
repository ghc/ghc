SRC_CC_WARNING_OPTS =
SRC_HC_WARNING_OPTS =


#####################
# Warnings

ifneq "$(GccIsClang)" "YES"

# Debian doesn't turn -Werror=unused-but-set-variable on by default, so
# we turn it on explicitly for consistency with other users
ifeq "$(GccLT46)" "NO"
# Never set the flag on Windows as the host gcc may be too old.
ifneq "$(HostOS_CPP)" "mingw32"
SRC_CC_WARNING_OPTS += -Werror=unused-but-set-variable
endif
# gcc 4.6 gives 3 warning for giveCapabilityToTask not being inlined
SRC_CC_WARNING_OPTS += -Wno-error=inline
endif

else

# Don't warn about unknown GCC pragmas when using clang
SRC_CC_WARNING_OPTS += -Wno-unknown-pragmas

endif

SRC_CC_OPTS     += -Wall
SRC_HC_OPTS     += -Wall

GhcStage1HcOpts += -fwarn-tabs
GhcStage2HcOpts += -fwarn-tabs

utils/hpc_dist-install_EXTRA_HC_OPTS += -fwarn-tabs


######################################################################
# Disable some warnings in packages we use

# Cabal doesn't promise to be warning-free
utils/ghc-cabal_dist_EXTRA_HC_OPTS += -w
libraries/Cabal/Cabal_dist-boot_EXTRA_HC_OPTS += -w
libraries/Cabal/Cabal_dist-install_EXTRA_HC_OPTS += -w

# Turn off import warnings for bad unused imports
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports
libraries/bytestring_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports
utils/haddock_dist_EXTRA_HC_OPTS += -fno-warn-unused-imports
libraries/vector_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

# haddock's attoparsec uses deprecated `inlinePerformIO`
utils/haddock_dist_EXTRA_HC_OPTS += -fno-warn-deprecations

# containers uses bitSize at the moment
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations
libraries/containers_dist-install_EXTRA_HC_OPTS += -fno-warn-redundant-constraints

# On Windows, there are also some unused import warnings
ifeq "$(HostOS_CPP)" "mingw32"
libraries/time_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports -fno-warn-identities
endif

# haskeline has warnings about deprecated use of block/unblock
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-deprecations
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -fno-warn-redundant-constraints

# binary upstream has some warnings, so don't use -Werror for it
libraries/binary_dist-boot_EXTRA_HC_OPTS += -Wwarn
libraries/binary_dist-install_EXTRA_HC_OPTS += -Wwarn

# temporarily turn off unused-imports warnings for pretty
libraries/pretty_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

# primitive has a warning about deprecated use of GHC.IOBase
libraries/primitive_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

# temporarily turn off unused-imports warnings for terminfo
libraries/terminfo_dist-boot_EXTRA_HC_OPTS += -fno-warn-unused-imports
libraries/terminfo_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

# vector has some unused match warnings
libraries/vector_dist-install_EXTRA_HC_OPTS += -Wwarn

# temporarily turn off unused-imports warnings for xhtml
libraries/xhtml_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-imports

libraries/dph/dph-base_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-interface_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-seq_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-par_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-lifted-common-install_EXTRA_HC_OPTS += -Wwarn

# transformers has unused function parameters warnings
libraries/transformers_dist-boot_EXTRA_HC_OPTS += -fno-warn-unused-matches -fno-warn-unused-imports
libraries/transformers_dist-install_EXTRA_HC_OPTS += -fno-warn-unused-matches -fno-warn-unused-imports
libraries/transformers_dist-install_EXTRA_HC_OPTS += -fno-warn-redundant-constraints

# Turn of trustworthy-safe warning
libraries/base_dist-install_EXTRA_HC_OPTS += -fno-warn-trustworthy-safe
libraries/ghc-prim_dist-install_EXTRA_HC_OPTS += -fno-warn-trustworthy-safe
libraries/Win32_dist-install_EXTRA_HC_OPTS += -fno-warn-trustworthy-safe

# We need -fno-warn-deprecated-flags to avoid failure with -Werror
GhcLibExtraHcOpts += -fno-warn-deprecated-flags
GhcBootLibExtraHcOpts += -fno-warn-deprecated-flags

# The warning suppression flag below is a temporary kludge. While working with
# modules that contain tabs, please de-tab them so this flag can be eventually
# removed. See
# http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
# for details
#
GhcLibExtraHcOpts += -fno-warn-tabs
GhcBootLibExtraHcOpts += -fno-warn-tabs
