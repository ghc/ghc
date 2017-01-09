module Settings.Flavours.Quickest (quickestFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default
import Settings.Optimisation

quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = append [vanilla]
    , rtsWays     = quickestRtsWays }

quickestArgs :: Args
quickestArgs = optimisationArgs $ Optimisation
    { hsDefault  = append ["-O0", "-H64m"]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc      = mempty }

quickestRtsWays :: Ways
quickestRtsWays = mconcat
    [ append [vanilla]
    , buildHaddock defaultFlavour ? append [threaded] ]
