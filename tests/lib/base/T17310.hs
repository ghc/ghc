{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import Data.Type.Equality
import GHC.TypeLits
import GHC.TypeNats

data A (t :: Nat)    = A
data B (t :: Nat)    = B
data C (t :: Symbol) = C
data D (t :: Symbol) = D

cmpNats :: (KnownNat n, KnownNat m) => A n -> B m -> Bool
cmpNats a b = case a `sameNat` b of
  Nothing   -> False
  Just Refl -> True

cmpSymbols :: (KnownSymbol n, KnownSymbol m) => C n -> D m -> Bool
cmpSymbols c d = case c `sameSymbol` d of
  Nothing   -> False
  Just Refl -> True
