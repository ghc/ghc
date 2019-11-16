{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
module Bug where

import Data.Kind
import Type.Reflection

newtype Foo = MkFoo (forall a. a)

foo :: TypeRep MkFoo
foo = typeRep @MkFoo

type family F a
type instance F Int = Type

data Bar = forall (a :: F Int). MkBar a

bar :: TypeRep (MkBar True)
bar = typeRep

data Quux = MkQuux (# Bool | Int #)

quux :: TypeRep MkQuux
quux = typeRep

data Quuz :: (Type ~ Type) => Type where
  MkQuuz :: Quuz

quuz :: TypeRep MkQuuz
quuz = typeRep
