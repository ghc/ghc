#
# GHC project configuration
#
#  

# what to include in a binary distribution
GhcMainDir = ghc
GhcBinDistDirs = ghc
GhcBinDistDocs = docs ghc/docs/users_guide ghc/docs/libraries
GhcBinDistPrlScripts = ghc-$(ProjectVersion) stat2resid hstags mkdependHS
GhcBinDistLibPrlScripts = hscpp
GhcBinDistBins = hp2ps

include version.mk
