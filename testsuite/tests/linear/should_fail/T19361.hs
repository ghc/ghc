{-# LANGUAGE LinearTypes #-}

module T19361 where

import GHC.Types

f :: a %(m :: Multiplicity) -> a
f x = g x

g :: a -> a
g x = x
