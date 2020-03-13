module GHC.Builtin.RebindableNames where

import GHC.Data.FastString

reboundIfSymbol :: FastString
reboundIfSymbol = fsLit "ifThenElse"
