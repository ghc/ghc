# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.5 1999/12/13 17:34:54 simonmar Exp $
#
# GHC project configuration
#
# This file can only be included from the top of
# an fptools/ build tree, since the version.mk 'include'
# reaches in and grabs the project-specific settings.

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc hslibs
GhcBinDistDocs = docs ghc/docs/users_guide ghc/docs/libraries
GhcBinDistPrlScripts = ghc-$(ProjectVersion) stat2resid
GhcBinDistLibPrlScripts = hscpp mkdependHS
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
