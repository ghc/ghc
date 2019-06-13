SRC_HC_OPTS        = -O -H64m
GhcStage1HcOpts    = -O0 -DDEBUG
GhcStage2HcOpts    = -O
GhcLibHcOpts       = -O -dcore-lint
BUILD_PROF_LIBS    = NO
SplitSections      = NO
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO
BUILD_MAN          = NO

LAX_DEPENDENCIES   = YES

# Reduce optimisation when building Cabal; this makes a significant difference
# in overall build time. See #16817.
libraries/Cabal_dist-install_HC_OPTS += -O0
