{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications #-}
module NativeGeneralGadtOperandProvider (Both(..), make) where

import Data.Proxy (Proxy(..))

-- The constructor binds its type parameters in the opposite order to the indices.
data Both i j where
    Mk2 :: forall b a. b -> a -> Both (Maybe a) [b]

{-# NOINLINE mark #-}
mark :: forall i j. foreach (v :: Both i j) -> (Both i j, Proxy v)
mark v = (v, Proxy)

{-# INLINE make #-}
make :: forall a b. foreach (x :: a) -> foreach (y :: b)
     -> (Both (Maybe a) [b], Proxy ('Mk2 y x))
make x y = mark @(Maybe a) @[b] (Mk2 y x)
