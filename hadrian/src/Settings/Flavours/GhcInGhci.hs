module Settings.Flavours.GhcInGhci (ghcInGhciFlavour) where

import qualified Data.Set as Set
import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
ghcInGhciFlavour :: Flavour
ghcInGhciFlavour = disableProfiledLibs $ defaultFlavour
    { name        = "ghc-in-ghci"
    , extraArgs   = ghciArgs
    , libraryWays =
        Set.fromList
            <$> mconcat
                [ pure [vanilla]
                , platformSupportsSharedLibs ? pure [dynamic]
                ]
    }

ghciArgs :: Args
ghciArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc = mempty }
