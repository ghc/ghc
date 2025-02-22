{-# LANGUAGE LinearTypes #-}

module T19361 where

import GHC.Types

f :: a %(m :: Multiplicity) -> a -- MODS_TODO error message here just has %m, not %(m :: Multiplicity)
f x = g x

g :: a -> a
g x = x
