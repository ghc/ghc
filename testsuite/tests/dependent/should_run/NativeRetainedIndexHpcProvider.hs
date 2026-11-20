{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}
module NativeRetainedIndexHpcProvider where

data Nat = Z | S Nat
data Tag (n :: Nat) = Tag Int

keep :: foreach (n :: Nat) -> Tag n
keep _ = Tag 10
{-# NOINLINE keep #-}

shallow :: foreach (n :: Nat) -> Tag n
shallow n = Tag (case n of Z -> 0; S _ -> 1)
{-# NOINLINE shallow #-}

deep :: foreach (n :: Nat) -> Tag n
deep n = Tag (case n of Z -> 0; S Z -> 2; S (S _) -> 3)
{-# NOINLINE deep #-}

skippedIndex :: Tag ('S 'Z)
skippedIndex = keep (S Z)
{-# NOINLINE skippedIndex #-}

selectedIndex :: Tag ('S 'Z)
selectedIndex = shallow (S Z)
{-# NOINLINE selectedIndex #-}

guardedIndex :: Tag ('S 'Z)
guardedIndex = deep (S Z)
{-# NOINLINE guardedIndex #-}
