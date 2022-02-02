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

utils/notes-util_USES_CABAL                   = YES
utils/notes-util_PACKAGE                      = notes-util
utils/notes-util_dist-install_PROGNAME        = notes-util
utils/notes-util_dist-install_INSTALL         = NO
utils/notes-util_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,utils/notes-util,dist-install,1))
