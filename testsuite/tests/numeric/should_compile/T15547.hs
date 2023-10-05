{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module T15547 where

import GHC.TypeNats
import Data.Proxy
import GHC.Word
import GHC.Exts

nat2Word# :: KnownNat n => Proxy# n -> Word#
nat2Word# p = case fromIntegral (natVal' p) of
  W# w -> w

foo (# #) = nat2Word# (proxy# :: Proxy# 18)


-- functions from the ticket
fd (_ :: Proxy n) = nat2Word# (proxy# @(Div (n + 63) 64))

fm (_ :: Proxy n) = nat2Word# (proxy# @(Mod (n - 1) 64 + 1))

fp (_ :: Proxy n) = nat2Word# (proxy# @(2^(Mod (n + 63) 64 + 1)))

d (# #) = fd (Proxy @137)
m (# #) = fm (Proxy @137)
p (# #) = fp (Proxy @137)
