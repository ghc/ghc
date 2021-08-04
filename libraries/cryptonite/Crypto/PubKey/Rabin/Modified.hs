-- |
-- Module      : Crypto.PubKey.Rabin.Modified
-- License     : BSD-style
-- Maintainer  : Carlos Rodriguez-Vega <crodveg@yahoo.es>
-- Stability   : experimental
-- Portability : unknown
--
-- Modified-Rabin public-key digital signature algorithm.
-- See algorithm 11.30 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.PubKey.Rabin.Modified
    ( PublicKey(..)
    , PrivateKey(..)
    , generate
    , sign
    , verify
    ) where

import           Data.ByteString
import           Data.Data

import           Crypto.Hash
import           Crypto.Number.ModArithmetic (expSafe, jacobi)
import           Crypto.Number.Serialize (os2ip)
import           Crypto.PubKey.Rabin.Types
import           Crypto.Random.Types

-- | Represent a Modified-Rabin public key.
data PublicKey = PublicKey
    { public_size :: Int      -- ^ size of key in bytes
    , public_n    :: Integer  -- ^ public p*q
    } deriving (Show, Read, Eq, Data)

-- | Represent a Modified-Rabin private key.
data PrivateKey = PrivateKey
    { private_pub :: PublicKey
    , private_p   :: Integer   -- ^ p prime number
    , private_q   :: Integer   -- ^ q prime number
    , private_d   :: Integer
    } deriving (Show, Read, Eq, Data)

-- | Generate a pair of (private, public) key of size in bytes.
-- Prime p is congruent 3 mod 8 and prime q is congruent 7 mod 8.
generate :: MonadRandom m
         => Int           
         -> m (PublicKey, PrivateKey)
generate size = do
    (p, q) <- generatePrimes size (\p -> p `mod` 8 == 3) (\q -> q `mod` 8 == 7)
    return $ generateKeys p q
  where 
    generateKeys p q =
        let n = p*q   
            d = (n - p - q + 5) `div` 8
            publicKey = PublicKey { public_size = size
                                    , public_n    = n }
            privateKey = PrivateKey { private_pub = publicKey
                                    , private_p   = p
                                    , private_q   = q
                                    , private_d   = d }
            in (publicKey, privateKey)

-- | Sign message using hash algorithm and private key.
sign :: HashAlgorithm hash
     => PrivateKey    -- ^ private key
     -> hash          -- ^ hash function
     -> ByteString    -- ^ message to sign
     -> Either Error Integer
sign pk hashAlg m =
    let d = private_d pk
        n = public_n $ private_pub pk
        h = os2ip $ hashWith hashAlg m
        limit = (n - 6) `div` 16
     in if h > limit then Left MessageTooLong
        else let h' = 16*h + 6
              in case jacobi h' n of
                    Just 1    -> Right $ expSafe h' d n
                    Just (-1) -> Right $ expSafe (h' `div` 2) d n
                    _         -> Left InvalidParameters

-- | Verify signature using hash algorithm and public key.
verify :: HashAlgorithm hash
       => PublicKey     -- ^ public key
       -> hash          -- ^ hash function
       -> ByteString    -- ^ message
       -> Integer       -- ^ signature
       -> Bool
verify pk hashAlg m s =
    let n   = public_n pk
        h   = os2ip $ hashWith hashAlg m
        s'  = expSafe s 2 n
        s'' = case s' `mod` 8 of
            6 -> s'
            3 -> 2*s'
            7 -> n - s'
            2 -> 2*(n - s')
            _ -> 0
     in case s'' `mod` 16 of
            6 -> let h' = (s'' - 6) `div` 16
                  in h' == h 
            _ -> False
