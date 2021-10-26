{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module T17536 where

import Data.Kind
import GHC.Types

type R :: RuntimeRep -> Type
type family R r where
  R _ = Int

r :: R FloatRep -> Int
r x = x

type L :: Levity -> Type
type family L l where
  L _ = Int

l :: L Unlifted -> Int
l x = x

type M :: Multiplicity -> Type
type family M m where
  M _ = Int

g :: M One -> Int
g x = x
