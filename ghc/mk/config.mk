# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.15 2001/04/11 10:41:46 sewardj Exp $
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
GhcBinDistLinks = ghc ghci ghc-pkg

include $(GhcMainDir)/mk/version.mk
