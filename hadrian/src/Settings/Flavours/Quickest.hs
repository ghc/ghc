module Settings.Flavours.Quickest (quickestFlavour) where

import qualified Data.Set as Set

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = pure (Set.fromList [vanilla])
    , rtsWays     = pure (Set.fromList [vanilla, threaded, threadedLogging, logging])
    , dynamicGhcPrograms = return False }

quickestArgs :: Args
quickestArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        ]
    , hsLibrary  = mempty
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = stage0 ? arg "-O" }
