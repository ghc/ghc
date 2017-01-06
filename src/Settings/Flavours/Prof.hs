module Settings.Flavours.Prof (profFlavour) where

import Context
import Flavour
import GHC
import Predicate
import {-# SOURCE #-} Settings.Default

profFlavour :: Flavour
profFlavour = defaultFlavour
    { name        = "prof"
    , args        = defaultArgs <> profArgs
    , ghcProfiled = True }

optimise :: Context -> Bool
optimise Context {..} = package `elem` [compiler, ghc] || isLibrary package

profArgs :: Args
profArgs = builder Ghc ? do
    context <- getContext
    if optimise context then arg "-O" else arg "-O0"
