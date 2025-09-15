{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -O0 #-}
module OpaqueNoCastWW where

import GHC.TypeNats

newtype Signed (n :: Nat) = S { unsafeToInteger :: Integer}

-- Normally introduces a worker of type: Signed m -> Signed n -> Integer
times :: Signed m -> Signed n -> Signed (m + n)
times (S a) (S b) = S (a * b)
{-# OPAQUE times #-}
