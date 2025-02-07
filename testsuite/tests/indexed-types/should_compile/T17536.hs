{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoPolyKinds #-}
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


-- make sure wildcard and non-wildcard type variables are treated the same

type R1 :: RuntimeRep -> Type
type family R1 r where
  R1 r = Int

r1 :: R1 FloatRep -> Int
r1 x = x

type L1 :: Levity -> Type
type family L1 l where
  L1 l = Int

l1 :: L1 Unlifted -> Int
l1 x = x

type M1 :: Multiplicity -> Type
type family M1 m where
  M1 m = Int

g1 :: M1 One -> Int
g1 x = x
