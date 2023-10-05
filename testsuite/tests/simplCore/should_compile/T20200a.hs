module T20200a where

import qualified Data.Map.Strict as Map

f :: [Maybe (Int, Bool)]
f = map Just
     $ Map.keys
     $ Map.fromListWith (||) []
