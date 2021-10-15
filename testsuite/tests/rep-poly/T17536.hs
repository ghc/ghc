{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}

module T17536 where

import Data.Kind
import GHC.Exts

data A (r :: RuntimeRep)

type family IsA r where
  IsA (A _) = Char
  IsA _     = Int

f :: IsA (A UnliftedRep)
f = 'a'
