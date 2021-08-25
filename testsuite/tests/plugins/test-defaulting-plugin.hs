{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, DataKinds, PolyKinds, RankNTypes, AllowAmbiguousTypes, TypeOperators, TypeFamilies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin DefaultLifted -fwarn-type-defaults #-}

-- Tests defaulting plugins
module Main where
import GHC.TypeLits
import Data.Proxy
import DefaultLifted

instance DefaultType Nat 4
instance DefaultType Nat 2
instance DefaultType Nat 0

class MyClass (a :: Nat) (b :: Nat) where
  mc :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> Integer

instance MyClass a b where
  mc a b = natVal a + natVal b

q :: forall (a :: Nat). (KnownNat a) => Integer
q = natVal (Proxy :: Proxy a)

w :: forall (a :: Nat). (KnownNat a, 2 <= a) => Integer
w = natVal (Proxy :: Proxy a)

main :: IO ()
main = do
  print $ q + w
  print $ mc Proxy Proxy
