module Settings.Flavours.GhcInGhci (ghcInGhciFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Common

-- Please update doc/flavours.md when changing this file.
ghcInGhciFlavour :: Flavour
ghcInGhciFlavour = defaultFlavour
    { name        = "ghc-in-ghci"
    , args        = defaultBuilderArgs <> ghciArgs <> defaultPackageArgs
    , libraryWays = pure [vanilla, dynamic]
    , rtsWays     = pure [vanilla, threaded, dynamic]
    , dynamicGhcPrograms = return False }

ghciArgs :: Args
ghciArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        , naturalInBaseFixArgs
        ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc = mempty }
