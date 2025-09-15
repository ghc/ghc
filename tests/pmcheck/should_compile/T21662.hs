{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module T21662 where

import GHC.TypeNats (Nat, KnownNat)

view_fn :: forall (n :: Nat). KnownNat n => Int -> Bool
view_fn i = i > 0

foo :: Int -> Int
foo (view_fn @12 -> True) = 0
foo (view_fn @12 -> False) = 0

  -- The point is that the two view pattern functions "view_fn"
  -- may get different uniques for the KnownNat 12 dictionary,
  -- which leads to a spurious pattern match warning.
