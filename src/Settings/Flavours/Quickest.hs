module Settings.Flavours.Quickest (quickestFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default

quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name         = "quickest"
    , args         = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays  = append [vanilla]
    , rtsWays      = quickestRtsWays }

quickestArgs :: Args
quickestArgs = builder Ghc ? arg "-O0"

quickestRtsWays :: Ways
quickestRtsWays = mconcat
    [ append [vanilla]
    , buildHaddock defaultFlavour ? append [threaded] ]
