module Settings.Flavours.Quickest (quickestFlavour) where

import Flavour
import Predicate
import {-# SOURCE #-} Settings.Default

quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = defaultLibraryWays <> quickestLibraryWays }

quickestArgs :: Args
quickestArgs = builder Ghc ? arg "-O0"

quickestLibraryWays :: Ways
quickestLibraryWays = remove [profiling]
