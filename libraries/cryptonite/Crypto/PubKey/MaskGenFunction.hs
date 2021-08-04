-- |
-- Module      : Crypto.PubKey.MaskGenFunction
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE BangPatterns #-}
module Crypto.PubKey.MaskGenFunction
    ( MaskGenAlgorithm
    , mgf1
    ) where

import           Crypto.Number.Serialize (i2ospOf_)
import           Crypto.Hash
import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, Bytes)
import qualified Crypto.Internal.ByteArray as B

-- | Represent a mask generation algorithm
type MaskGenAlgorithm seed output =
       seed   -- ^ seed
    -> Int    -- ^ length to generate
    -> output

-- | Mask generation algorithm MGF1
mgf1 :: (ByteArrayAccess seed, ByteArray output, HashAlgorithm hashAlg)
     => hashAlg
     -> seed
     -> Int
     -> output
mgf1 hashAlg seed len =
    let !seededCtx = hashUpdate (hashInitWith hashAlg) seed
     in B.take len $ B.concat $ map (hashCounter seededCtx) [0..fromIntegral (maxCounter-1)]
  where
    digestLen     = hashDigestSize hashAlg
    (chunks,left) = len `divMod` digestLen
    maxCounter    = if left > 0 then chunks + 1 else chunks

    hashCounter :: HashAlgorithm a => Context a -> Integer -> Digest a
    hashCounter ctx counter = hashFinalize $ hashUpdate ctx (i2ospOf_ 4 counter :: Bytes)
