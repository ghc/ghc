{-# LANGUAGE GHC2021, AllowAmbiguousTypes, DataKinds, MagicHash, TypeFamilies #-}
module T23903 where

import Data.Kind(Type)
import GHC.Exts(Float#, Int#, RuntimeRep(FloatRep, IntRep), TYPE)

type Rep :: Type -> RuntimeRep
type family Rep t where
  Rep Int = IntRep
  Rep Float = FloatRep

type Unbox :: forall (t :: Type) -> TYPE (Rep t)
type family Unbox t where
  Unbox Int = Int#
  Unbox Float = Float#

type family a #-> b where
  a #-> b = Unbox a -> b

f :: a #-> ()
f _ = ()
