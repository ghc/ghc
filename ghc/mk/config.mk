# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.8 2000/07/18 17:09:02 simonmar Exp $
#
# GHC project configuration
#
# This file can only be included from the top of
# an fptools/ build tree, since the version.mk 'include'
# reaches in and grabs the project-specific settings.

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc hslibs
GhcBinDistDocs = docs ghc/docs/set
GhcBinDistPrlScripts = ghc-$(ProjectVersion) stat2resid
GhcBinDistLibPrlScripts = mkdependHS
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
