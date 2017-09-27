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

testsuite/utils/check-ppr_USES_CABAL                   = YES
testsuite/utils/check-ppr_PACKAGE                      = check-ppr
testsuite/utils/check-ppr_dist-install_PROGNAME        = check-ppr
testsuite/utils/check-ppr_dist-install_INSTALL         = NO
testsuite/utils/check-ppr_dist-install_INSTALL_INPLACE = YES
$(eval $(call build-prog,testsuite/utils/check-ppr,dist-install,2))

testsuite_utils: $(testsuite/utils/check-ppr_dist-install_INPLACE)
