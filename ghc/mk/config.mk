# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.19 2003/03/05 10:22:22 simonmar Exp $
#
# GHC project configuration
#
# This file can only be included from the top of
# an fptools/ build tree, since the version.mk 'include'
# reaches in and grabs the project-specific settings.

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc libraries hslibs

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
GhcBinDistShScripts = ghc-$(ProjectVersion) ghci-$(ProjectVersion) ghc-pkg-$(ProjectVersion) hsc2hs
else
GhcBinDistShScripts =
endif

GhcBinDistPrlScripts = 
GhcBinDistLibPrlScripts = ghc-asm ghc-split
GhcBinDistBins = hp2ps ghcprof DrIFT DtdToHaskell Xtract
GhcBinDistLinks = ghc ghci ghc-pkg

include $(GhcMainDir)/mk/version.mk
