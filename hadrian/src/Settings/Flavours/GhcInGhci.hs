module Settings.Flavours.GhcInGhci (ghcInGhciFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
ghcInGhciFlavour :: Flavour
ghcInGhciFlavour = disableProfiledLibs $ defaultFlavour
    { name        = "ghc-in-ghci"
    , extraArgs        = ghciArgs
    }

ghciArgs :: Args
ghciArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc = mempty }
