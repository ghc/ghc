module Settings.Flavours.Quickest (quickestFlavourArgs, quickestFlavourWays) where

import Context
import GHC
import Predicate

optimise :: Context -> Bool
optimise Context {..} = stage == Stage0 && package `elem` [compiler, ghc]

quickestFlavourArgs :: Args
quickestFlavourArgs = builder Ghc ? do
    context <- getContext
    if optimise context then arg "-O" else arg "-O0"

quickestFlavourWays :: Ways
quickestFlavourWays = remove [profiling]
