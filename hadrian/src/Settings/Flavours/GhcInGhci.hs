module Settings.Flavours.GhcInGhci (ghcInGhciFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
ghcInGhciFlavour :: Flavour
ghcInGhciFlavour = defaultFlavour
    { name        = "ghc-in-ghci"
    , extraArgs        = mconcat [
                            ghciArgs
                            , builder (Cabal Flags) ? arg "+internal-interpreter"
                            ]
    -- We can't build DLLs on Windows (yet). Actually we should only
    -- include the dynamic way when we have a dynamic host GHC, but just
    -- checking for Windows seems simpler for now.
    , libraryWays = pure (Set.fromList [vanilla]) <> pure (Set.fromList [ dynamic | not windowsHost ])
    , rtsWays     = pure (Set.fromList [vanilla]) <> (targetSupportsThreadedRts ? pure (Set.fromList [threaded])) <> pure (Set.fromList [ dynamic | not windowsHost ])
    }

ghciArgs :: Args
ghciArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc = mempty }
