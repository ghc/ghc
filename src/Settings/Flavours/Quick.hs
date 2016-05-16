module Settings.Flavours.Quick (quickFlavourArgs, quickFlavourWays) where

import Expression
import Predicates

quickFlavourArgs :: Args
quickFlavourArgs = builder Ghc ? arg "-O0"

quickFlavourWays :: Ways
quickFlavourWays = remove [profiling]
