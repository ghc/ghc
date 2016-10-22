# See Note [Order of warning flags].
SRC_CC_OPTS     += -Wall $(WERROR)
SRC_HC_OPTS     += -Wall
# Don't add -Werror to SRC_HC_OPTS_STAGE0 (or SRC_HC_OPTS), because otherwise
# validate may unnecessarily fail when booting with an older compiler.
# It would be better to only exclude certain warnings from becoming errors
# (e.g. '-Werror -Wno-error=unused-imports -Wno-error=...'), but -Wno-error
# isn't supported yet (https://ghc.haskell.org/trac/ghc/wiki/Design/Warnings).
#
# See Note [Stage number in build variables] in mk/config.mk.in.
SRC_HC_OPTS_STAGE1 += $(WERROR)
SRC_HC_OPTS_STAGE2 += $(WERROR)


ifneq "$(GccIsClang)" "YES"

# Debian doesn't turn -Werror=unused-but-set-variable on by default, so
# we turn it on explicitly for consistency with other users
ifeq "$(GccLT46)" "NO"
# Never set the flag on Windows as the host gcc may be too old.
ifneq "$(HostOS_CPP)" "mingw32"
SRC_CC_WARNING_OPTS += -Werror=unused-but-set-variable
endif
endif

ifeq "$(GccLT44)" "NO"
# Suppress the warning about __sync_fetch_and_nand (#9678).
libraries/ghc-prim/cbits/atomic_CC_OPTS += -Wno-sync-nand
# gcc 4.6 gives 3 warnings for giveCapabilityToTask not being inlined
# gcc 4.4 gives 2 warnings for lockClosure not being inlined
SRC_CC_WARNING_OPTS += -Wno-error=inline
endif

else

# Don't warn about unknown GCC pragmas when using clang
SRC_CC_WARNING_OPTS += -Wno-unknown-pragmas

endif

SRC_HC_WARNING_OPTS_STAGE1 += -Wnoncanonical-monad-instances
SRC_HC_WARNING_OPTS_STAGE2 += -Wnoncanonical-monad-instances

# TODO #12618 see https://ghc.haskell.org/trac/ghc/ticket/12618#comment:25
SRC_HC_WARNING_OPTS += -fno-warn-inline-rule-shadowing

######################################################################
# Disable some warnings in packages we use

# NB: The GHC version used for bootstrapping may not support the
# `-W`-aliases for `-f(no-)warn` flags introduced in GHC 8.0, so in
# some cases (watch out for make-variables with a name containing
# "boot") we need to pass the legacy `-f(no-)warn`-flags instead.

# Libraries that have dubious RULES
libraries/bytestring_dist-install_EXTRA_HC_OPTS += -Wno-inline-rule-shadowing

# Turn off import warnings for bad unused imports
utils/haddock_dist_EXTRA_HC_OPTS += -Wno-unused-imports
libraries/vector_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports
libraries/directory_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports

# haddock's attoparsec uses deprecated `inlinePerformIO`
utils/haddock_dist_EXTRA_HC_OPTS += -Wno-deprecations
# binary too
libraries/binary_dist-install_EXTRA_HC_OPTS += -Wno-deprecations

# On Windows, there are/were some unused import warnings
ifeq "$(HostOS_CPP)" "mingw32"
libraries/time_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports -Wno-identities
endif

# On Windows, the pattern for CallConv is already exaustive. Ignore the warning
ifeq "$(HostOS_CPP)" "mingw32"
libraries/ghci_dist-install_EXTRA_HC_OPTS += -Wno-overlapping-patterns
endif

# haskeline has warnings about deprecated use of block/unblock
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -Wno-deprecations
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -Wno-redundant-constraints
libraries/haskeline_dist-install_EXTRA_HC_OPTS += -Wno-simplifiable-class-constraints


# temporarily turn off unused-imports warnings for pretty
libraries/pretty_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports

# primitive has a warning about deprecated use of GHC.IOBase
libraries/primitive_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports
# primitive uses deprecated Control.Monad.Trans.Error
libraries/primitive_dist-install_EXTRA_HC_OPTS += -Wno-deprecations

# See https://github.com/haskell/random/pull/20
libraries/random_dist-install_EXTRA_HC_OPTS += -Wno-redundant-constraints

# temporarily turn off unused-imports warnings for terminfo
libraries/terminfo_dist-boot_EXTRA_HC_OPTS += -fno-warn-unused-imports
libraries/terminfo_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports

# vector has some unused match warnings
libraries/vector_dist-install_EXTRA_HC_OPTS += -Wwarn

# temporarily turn off unused-imports warnings for xhtml
libraries/xhtml_dist-install_EXTRA_HC_OPTS += -Wno-unused-imports
libraries/xhtml_dist-install_EXTRA_HC_OPTS += -Wno-tabs

libraries/dph/dph-base_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-interface_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-seq_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-prim-par_dist-install_EXTRA_HC_OPTS += -Wwarn
libraries/dph/dph-lifted-common-install_EXTRA_HC_OPTS += -Wwarn

# transformers has unused function parameters warnings
libraries/transformers_dist-boot_EXTRA_HC_OPTS += -fno-warn-unused-matches -fno-warn-unused-imports
libraries/transformers_dist-install_EXTRA_HC_OPTS += -Wno-unused-matches -Wno-unused-imports
libraries/transformers_dist-install_EXTRA_HC_OPTS += -Wno-redundant-constraints
libraries/transformers_dist-install_EXTRA_HC_OPTS += -Wno-orphans

# Turn of trustworthy-safe warning
libraries/base_dist-install_EXTRA_HC_OPTS += -Wno-trustworthy-safe
libraries/ghc-prim_dist-install_EXTRA_HC_OPTS += -Wno-trustworthy-safe
libraries/Win32_dist-install_EXTRA_HC_OPTS += -Wno-trustworthy-safe

# We need -Wno-deprecated-flags to avoid failure with -Werror
GhcLibExtraHcOpts += -Wno-deprecated-flags
GhcBootLibExtraHcOpts += -fno-warn-deprecated-flags

# Note [Order of warning flags]
#
# In distdir-way-opts, build flags are added in the following order (this
# list is not exhaustive):
#
#   * SRC_HC_OPTS(_STAGE$4)
#   * ghc-options from .cabal files ($1_$2_HC_OPTS)
#   * SRC_HC_WARNING_OPTS(_STAGE$4)
#
# Considerations:
#
#   * Most .cabal files specify -Wall. But not all, and not all building we
#   do relies on .cabal files. So we have to add -Wall ourselves somewhere.
#
#   * Some .cabal also specify warning suppression flags. Because -Wall
#   overrides any warning suppression flags that come before it, we have to
#   make sure -Wall comes before any warning suppression flags. So we add it
#   to SRC_HC_OPTS.
#
#   * Similarly, our own warning suppression should come after the -Wall from
#   the .cabal files, so we do *not* add them to SRC_HC_OPTS.
