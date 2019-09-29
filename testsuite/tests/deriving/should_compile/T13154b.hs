{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module T13154b where

import Data.Kind
import Data.Typeable
import GHC.Exts
import GHC.TypeLits

class Foo1 (a :: TYPE ('TupleRep '[]))
deriving instance Foo1 a

class Foo2 (a :: TYPE ('TupleRep '[]))
deriving instance Foo2 (##)

class Foo3 (a :: TYPE ('SumRep '[ 'LiftedRep, 'LiftedRep ]))
deriving instance Foo3 a

class Foo4 (a :: TYPE ('SumRep '[ 'LiftedRep, 'LiftedRep ]))
deriving instance Foo4 (# a | b #)

class Foo5 (a :: Type)
deriving instance Foo5 a

class Foo6
deriving instance Foo6

class Foo7 (a :: Nat)
deriving anyclass instance Foo7 0
deriving          instance Foo7 1

class Foo8 (a :: Symbol)
deriving anyclass instance Foo8 "a"
deriving          instance Foo8 "b"

class Typeable a => Foo9 a
deriving instance _ => Foo9 (f a)

data family D1 a
newtype ByBar a = ByBar a
class Foo10 a where
  baz :: a -> a
instance Foo10 (ByBar a) where
  baz = id
deriving via ByBar (D1 a) instance Foo10 (D1 a)

data family D2
data family D3
class Foo11 a where
deriving anyclass instance Foo11 D2
deriving          instance Foo11 D3
