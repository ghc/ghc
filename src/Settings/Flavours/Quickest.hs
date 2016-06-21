module Settings.Flavours.Quickest (quickestFlavourArgs, quickestFlavourWays) where

import Predicate

quickestFlavourArgs :: Args
quickestFlavourArgs = builder Ghc ? arg "-O0"

quickestFlavourWays :: Ways
quickestFlavourWays = remove [profiling]
