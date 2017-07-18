{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fomit-type-family-coercions #-}
module OmitTyFamCoercions where

data N = Z | S N

type family Countdown n where
  Countdown (S k) = Countdown k
  Countdown Z = Int

-- The test checks that the long chain of coercions from reducing
-- `Countdown` is not present in Core
foo :: Countdown (S(S(S(S(S(S(S(S(S(S Z)))))))))) -> Int
foo = id
