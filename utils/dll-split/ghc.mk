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

utils/dll-split_USES_CABAL                   = YES
utils/dll-split_PACKAGE                      = dll-split
utils/dll-split_dist-install_PROGNAME        = dll-split
utils/dll-split_dist-install_INSTALL         = NO
utils/dll-split_dist-install_INSTALL_INPLACE = YES
# Use the stage0 instead of the stage1 compiler to build dll-split, to
# prevent: "dll-split: cannot execute binary file: Exec format error".
# Programs built with the stage1 compiler can only run on TARGET
# architecture, whereas dll-split is used during the GHC build process (see
# rules/build-package-way.mk) on the BUILD (=HOST) architectue.
$(eval $(call build-prog,utils/dll-split,dist-install,0))
