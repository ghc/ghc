-- |
-- Module      : Crypto.PubKey.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.PubKey.Internal
    ( and'
    , (&&!)
    , dsaTruncHash
    , dsaTruncHashDigest
    ) where

import Data.Bits (shiftR)
import Data.List (foldl')

import Crypto.Hash
import Crypto.Internal.ByteArray (ByteArrayAccess)
import Crypto.Number.Basic (numBits)
import Crypto.Number.Serialize

-- | This is a strict version of and
and' :: [Bool] -> Bool
and' l = foldl' (&&!) True l

-- | This is a strict version of &&.
(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False

-- | Truncate and hash for DSA and ECDSA.
dsaTruncHash :: (ByteArrayAccess msg, HashAlgorithm hash) => hash -> msg -> Integer -> Integer
dsaTruncHash hashAlg = dsaTruncHashDigest . hashWith hashAlg

-- | Truncate a digest for DSA and ECDSA.
dsaTruncHashDigest :: HashAlgorithm hash => Digest hash -> Integer -> Integer
dsaTruncHashDigest digest n
    | d > 0 = shiftR e d
    | otherwise = e
  where e = os2ip digest
        d = hashDigestSize (getHashAlg digest) * 8 - numBits n

getHashAlg :: Digest hash -> hash
getHashAlg _ = undefined
