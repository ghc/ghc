module Settings.Flavours.Quickest (quickestFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , extraArgs        = quickestArgs
    , libraryWays = pure (Set.fromList [vanilla])
    , rtsWays     = pure (Set.fromList [vanilla]) <> (staged targetSupportsThreadedRts ? pure (Set.fromList [threaded]))
    , dynamicGhcPrograms = const (return False) }

quickestArgs :: Args
quickestArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = stage0 ? arg "-O" }
