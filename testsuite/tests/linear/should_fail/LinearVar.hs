{-# LANGUAGE LinearTypes #-}
module LinearVar where

import GHC.Types (Multiplicity)

f :: a %(m :: Multiplicity) -> b
f = undefined :: a -> b
