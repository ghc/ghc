# -----------------------------------------------------------------------------
# $Id: config.mk,v 1.6 2000/01/20 14:53:42 simonmar Exp $
#
# GHC project configuration
#
# This file can only be included from the top of
# an fptools/ build tree, since the version.mk 'include'
# reaches in and grabs the project-specific settings.

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc hslibs
GhcBinDistDocs = docs ghc/docs/users_guide hslibs/doc
GhcBinDistPrlScripts = ghc-$(ProjectVersion) stat2resid
GhcBinDistLibPrlScripts = hscpp mkdependHS
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
