{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
-- | Observe the length stored in an ordinary imported package.
module PackedLength (packageLength) where

import PackedVecLibrary

-- | Read the witness without inspecting the vector.
packageLength :: PackedVec a -> Nat
packageLength (PackVec n _) = n
