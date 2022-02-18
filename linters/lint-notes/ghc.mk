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

linters/lint-notes_USES_CABAL                   = YES
linters/lint-notes_PACKAGE                      = lint-notes
linters/lint-notes_dist-install_PROGNAME        = lint-notes
linters/lint-notes_dist-install_INSTALL         = NO
linters/lint-notes_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,linters/lint-notes,dist-install,1))
