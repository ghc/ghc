#
# GHC project configuration
#
# This file can only be included from the top of
# an fptools/ build tree, since the version.mk 'include'
# reaches in and grabs the project-specific settings.

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc
GhcBinDistDocs = docs ghc/docs/users_guide ghc/docs/libraries
GhcBinDistPrlScripts = ghc-$(ProjectVersion) stat2resid hstags
GhcBinDistLibPrlScripts = hscpp mkdependHS
GhcBinDistBins = hp2ps

include $(GhcMainDir)/mk/version.mk
