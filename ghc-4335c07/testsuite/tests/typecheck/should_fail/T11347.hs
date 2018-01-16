-- Should AllowAmbiguousTypes really be needed here?
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}
module T11347 where

newtype Id1 a = MkId1 a
newtype Id2 a = MkId2 (Id1 a) deriving (UnsafeCast b)

type family Discern a b
type instance Discern (Id1 a) b = a
type instance Discern (Id2 a) b = b

class UnsafeCast to from where
  unsafe :: from -> Discern from to

instance UnsafeCast b (Id1 a) where
  unsafe (MkId1 x) = x

unsafeCoerce :: a -> b
unsafeCoerce x = unsafe (MkId2 (MkId1 x))
