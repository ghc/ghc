# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.16 2002/01/10 10:46:12 sof Exp $
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
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
GhcBinDistShScripts = ghc-$(ProjectVersion) ghci-$(ProjectVersion) ghc-pkg-$(ProjectVersion)
else
GhcBinDistShScripts=
endif
GhcBinDistPrlScripts = 
GhcBinDistLibPrlScripts = ghc-asm ghc-split
GhcBinDistBins = hp2ps ghcprof hsc2hs DrIFT DtdToHaskell Xtract
GhcBinDistLinks = ghc ghci ghc-pkg

include $(GhcMainDir)/mk/version.mk
