module Settings.Flavours.Perf (perfFlavour) where

import Context
import Flavour
import GHC
import Predicate
import {-# SOURCE #-} Settings.Default

perfFlavour :: Flavour
perfFlavour = defaultFlavour
    { name = "perf"
    , args = defaultBuilderArgs <> perfArgs <> defaultPackageArgs }

optimise :: Context -> Bool
optimise Context {..} =
    package `elem` [compiler, ghc] && stage == Stage2 || isLibrary package

perfArgs :: Args
perfArgs = builder Ghc ? do
    context <- getContext
    if optimise context then arg "-O2" else arg "-O"
