module Settings.Flavours.Quick (quickFlavourArgs, quickFlavourWays) where

import Expression
import Predicates (builderGhc)

quickFlavourArgs :: Args
quickFlavourArgs = builderGhc ? arg "-O0"

quickFlavourWays :: Ways
quickFlavourWays = remove [profiling]
