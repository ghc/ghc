{-# OPTIONS -fth  -ddump-splices #-}

module TH_genEx where

import TH_genExLib
import Language.Haskell.TH

class MyInterface a where
  foo :: a -> Int
  foo1 :: Int -> a -> Int

$(genAny (reify ''MyInterface))

