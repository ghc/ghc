# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.13 2001/03/26 16:56:55 simonmar Exp $
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
GhcBinDistShScripts = ghc ghci
GhcBinDistPrlScripts = 
GhcBinDistLibPrlScripts = ghc-asm ghc-split
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
