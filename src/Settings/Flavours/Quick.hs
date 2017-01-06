module Settings.Flavours.Quick (quickFlavour) where

import Context
import Flavour
import GHC
import Predicate
import {-# SOURCE #-} Settings.Default

quickFlavour :: Flavour
quickFlavour = defaultFlavour
    { name        = "quick"
    , args        = defaultBuilderArgs <> quickArgs <> defaultPackageArgs
    , libraryWays = defaultLibraryWays <> quickLibraryWays }

optimise :: Context -> Bool
optimise Context {..} =
    package `elem` [compiler, ghc] || stage == Stage1 && isLibrary package

quickArgs :: Args
quickArgs = builder Ghc ? do
    context <- getContext
    if optimise context then arg "-O" else arg "-O0"

quickLibraryWays :: Ways
quickLibraryWays = remove [profiling]
