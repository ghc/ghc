# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.12 2001/02/16 12:07:10 sewardj Exp $
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
GhcBinDistPrlScripts = 
GhcBinDistLibPrlScripts = ghc-asm ghc-split
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
