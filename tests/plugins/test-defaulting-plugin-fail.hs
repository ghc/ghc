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

q :: forall (a :: Nat). (KnownNat a) => Integer
q = natVal (Proxy :: Proxy a)

w :: forall (a :: Nat). (KnownNat a, 2 <= a) => Integer
w = natVal (Proxy :: Proxy a)

e :: forall (a :: Nat). (KnownNat a, 5 <= a) => Integer
e = natVal (Proxy :: Proxy a)

main :: IO ()
main = do
  print $ q + w + e
