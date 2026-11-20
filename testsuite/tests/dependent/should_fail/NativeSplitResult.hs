{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module NativeSplitResult where
import Data.Proxy
import PackedVecLibrary

wrongPrefix :: foreach (n :: Nat) -> foreach (m :: Nat) -> foreach (other :: Nat)
            -> Vec Bool (Add n m) -> (Vec Bool other, Vec Bool m)
wrongPrefix n m other xs = splitAtVec n xs

wrongSuffix :: foreach (n :: Nat) -> foreach (m :: Nat) -> foreach (other :: Nat)
            -> Vec Bool (Add n m) -> (Vec Bool n, Vec Bool other)
wrongSuffix n m other xs = splitAtVec n xs

type family Hidden (n :: Nat) (m :: Nat) :: Nat

project :: forall (m :: Nat). foreach (n :: Nat) -> (Proxy n, Proxy (Hidden n m))
project _ = (Proxy, Proxy)

hidden :: foreach (n :: Nat) -> foreach (m :: Nat) -> (Proxy n, Proxy (Hidden n m))
hidden n m = project n
