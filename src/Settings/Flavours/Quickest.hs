module Settings.Flavours.Quickest (quickestFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default

quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = append [vanilla]
    , rtsWays     = append [vanilla] }

quickestArgs :: Args
quickestArgs = builder Ghc ? arg "-O0"
