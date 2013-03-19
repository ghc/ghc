# -----------------------------------------------------------------------------
#
# (c) 2010 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# $1 = paths to prepend
ifeq "$(TargetOS_CPP)" "mingw32"
prependLibraryPath = $(error Do not know how to prependLibraryPath on Windows)
else ifeq "$(TargetOS_CPP)" "darwin"
prependLibraryPath = export DYLD_LIBRARY_PATH="$1:$$DYLD_LIBRARY_PATH"
else
prependLibraryPath = export LD_LIBRARY_PATH="$1:$$LD_LIBRARY_PATH"
endif

