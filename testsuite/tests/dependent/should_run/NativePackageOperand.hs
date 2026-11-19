{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, RankNTypes, TypeFamilies #-}
module NativePackageOperand where

import Data.Kind (Type)
import Data.Proxy
import PackedVecLibrary
import RetainedNat

keep :: foreach (p :: PackedVec Int) -> Proxy p
keep _ = Proxy

consume :: Proxy p -> Int
consume _ = 0

-- | Use a constructor application as the retained operand.
operand :: foreach (n :: Nat) -> Vec Int n -> Int
operand n xs = consume (keep (PackVec n xs))

-- | Check the same operand under a case on a retained call.
branch :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n
       -> (foreach (p :: PackedVec Int) -> Proxy p) -> Int
branch f n xs use = case f n of
    Z -> consume (use (PackVec n xs))
    S _ -> 0

#ifdef WRONG_WITNESS
wrong :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int m -> Int
wrong n _ xs = consume (keep (PackVec n xs))
#endif

#ifdef WRONG_FAMILY_RESULT
wrongFamilyResult :: foreach (q :: Retained p) -> p 'Z
wrongFamilyResult (Retain _ value) = value
#endif

#ifdef WRONG_FAMILY_KIND
type family Calculate (n :: Nat) :: Type
type WrongFamily = Retained Calculate
#endif

#ifdef SHIFTED_WITNESS
data Shifted (p :: Nat -> Type) where
    Shift :: foreach (n :: Nat) -> p ('S n) -> Shifted p
type family ShiftWitness (q :: Shifted p) :: Nat where
    ShiftWitness ('Shift n _) = n
#endif
