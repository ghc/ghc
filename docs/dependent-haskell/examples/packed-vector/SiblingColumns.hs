{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
-- | Two vectors that share one stored length.
module SiblingColumns (Columns(..), size, letters, flags, bottomFields) where

import PackedVecLibrary (Nat, Vec)

-- | Store the length before both vectors.
data Columns where
    Columns :: foreach (n :: Nat) -> Vec Char n -> Vec Bool n -> Columns

-- | Read the stored length.
size :: Columns -> Nat
size (Columns n _ _) = n

-- | Read the characters at the stored length.
letters :: foreach (p :: Columns) -> Vec Char (size p)
letters (Columns _ xs _) = xs

-- | Read the flags at the same length.
flags :: foreach (p :: Columns) -> Vec Bool (size p)
flags (Columns _ _ bs) = bs

-- | Each field has a distinct exception.
bottomFields :: Columns
bottomFields = build (error "forced length")
  where
    build :: foreach (n :: Nat) -> Columns
    build n = Columns n (error "forced letters") (error "forced flags")
