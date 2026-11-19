{-# LANGUAGE CPP, ExperimentalDependentTypes, GADTs, RankNTypes #-}
module NativePackageOperand where

import Data.Proxy
import PackedVecLibrary

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
