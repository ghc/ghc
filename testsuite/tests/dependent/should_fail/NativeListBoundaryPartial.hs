{-# LANGUAGE ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module NativeListBoundaryPartial where
import PackedVecLibrary (Nat(..))

-- | Omit the nonempty-list row for a successor index.
partial :: foreach (n :: Nat) -> [Int] -> Bool
partial Z [] = True
partial Z (_ : _) = False
partial (S _) [] = False
