module GHC.Builtin.RebindableNames where

import GHC.Data.FastString

reboundIfSymbol :: FastString
reboundIfSymbol = fsLit "ifThenElse"

reboundGetFieldSymbol :: FastString
reboundGetFieldSymbol = fsLit "getField"

reboundSetFieldSymbol :: FastString
reboundSetFieldSymbol = fsLit "setField"
