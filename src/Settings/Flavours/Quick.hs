module Settings.Flavours.Quick (quickFlavourArgs, quickFlavourWays) where

import Predicate

quickFlavourArgs :: Args
quickFlavourArgs = builder Ghc ? arg "-O0"

quickFlavourWays :: Ways
quickFlavourWays = remove [profiling]
