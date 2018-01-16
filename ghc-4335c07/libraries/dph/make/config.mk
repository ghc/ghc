# Default build configuration.
#   This file is under version control.
#   If you want to override these options then create a file make/config-override.mk
#   and assign the appropdiate variables there.

# -- Tools --------------------------------------------------------------------
GHC_DPH         = ghc
GHC_PKG         = ghc-pkg
GHC_FRAMEWORK   = ghc

# -- Backend ------------------------------------------------------------------
# What unlifted backend to use when compiling in-place.
# Options are {par, seq}
BACKEND         = par

# What lifted frontend to use when compiling in-place.
# Options are {copy, vseg}
FRONTEND        = vseg

# -- Flags --------------------------------------------------------------------
# How many threads to use with make
THREADS_MAKE    = 4

# Optimisations to compile with.
GHC_OPTS = \
        -Odph \
        -fno-liberate-case

# GHC language extensions that DPH code needs.
GHC_EXTS = \
	-XCPP \
	-XFlexibleInstances \
	-XGADTs \
	-XMagicHash \
	-XRankNTypes \
	-XTypeFamilies \
	-XTypeOperators \
	-XUnboxedTuples \
        -XBangPatterns \
        -XCPP \
        -XDeriveDataTypeable \
        -XEmptyDataDecls \
        -XExistentialQuantification \
        -XExplicitForAll \
        -XFlexibleContexts \
        -XFlexibleInstances \
        -XMagicHash \
        -XMultiParamTypeClasses \
        -XNoMonomorphismRestriction \
        -XParallelListComp \
        -XPatternGuards \
        -XRankNTypes \
        -XScopedTypeVariables \
        -XStandaloneDeriving \
        -XTemplateHaskell \
        -XTypeFamilies \
        -XTypeOperators \
        -XUnboxedTuples

# External packages that we need
GHC_PACKAGES = \
        ghc
        

# -- Override -----------------------------------------------------------------
# Override the above config.
-include make/config-override.mk
