# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.14 2001/03/29 10:44:29 simonmar Exp $
#
# GHC project configuration
#
# This file can only be included from the top of
# an fptools/ build tree, since the version.mk 'include'
# reaches in and grabs the project-specific settings.

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc hslibs
GhcBinDistDocs = ghc/docs/set
GhcBinDistShScripts = ghc-$(ProjectVersion) ghci-$(ProjectVersion) ghc-pkg-$(ProjectVersion)
GhcBinDistPrlScripts = 
GhcBinDistLibPrlScripts = ghc-asm ghc-split
GhcBinDistBins = hp2ps ghcprof hsc2hs DrIFT DtdToHaskell Xtract
GhcBinDistLinks = ghc ghci ghci-pkg

include $(GhcMainDir)/mk/version.mk
