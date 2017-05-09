{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module T13662 (run) where

newtype Value a = Value a

type family Repr (f :: * -> *) a :: *
type instance Repr f Int = f Int

class (Repr Value i ~ Value ir) => Native i ir where

instance Native Int Int where


fromInt :: (Native i ir) => i -> a
fromInt = undefined

apply :: (Int -> a -> a) -> a -> a
apply weight = id

run :: Float -> Float
run =
   let weight = \clip v -> fromInt clip * v
   in  apply weight

