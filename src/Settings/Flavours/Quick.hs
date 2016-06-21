module Settings.Flavours.Quick (quickFlavourArgs, quickFlavourWays) where

import Context
import GHC
import Predicate

optimise :: Context -> Bool
optimise Context {..} = package `elem` [compiler, ghc]
                     || stage == Stage1 && isLibrary package

quickFlavourArgs :: Args
quickFlavourArgs = builder Ghc ? do
    context <- getContext
    if optimise context then arg "-O" else arg "-O0"

quickFlavourWays :: Ways
quickFlavourWays = remove [profiling]
