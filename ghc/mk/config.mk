# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.10 2000/09/05 09:40:08 simonmar Exp $
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
GhcBinDistPrlScripts = ghc-$(ProjectVersion)
GhcBinDistLibPrlScripts = ghc-asm ghc-split ghc-stats
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
