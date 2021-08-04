-- |
-- Module      : Crypto.PubKey.Rabin.Basic
-- License     : BSD-style
-- Maintainer  : Carlos Rodriguez-Vega <crodveg@yahoo.es>
-- Stability   : experimental
-- Portability : unknown
--
-- Rabin cryptosystem for public-key cryptography and digital signature.
--
{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.PubKey.Rabin.Basic
    ( PublicKey(..)
    , PrivateKey(..)
    , Signature(..)
    , generate
    , encrypt
    , encryptWithSeed
    , decrypt
    , sign
    , signWith
    , verify
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Data
import           Data.Either (rights)

import           Crypto.Hash
import           Crypto.Number.Basic (gcde, numBytes)
import           Crypto.Number.ModArithmetic (expSafe, jacobi)
import           Crypto.Number.Serialize (i2osp, i2ospOf_, os2ip)
import           Crypto.PubKey.Rabin.OAEP 
import           Crypto.PubKey.Rabin.Types
import           Crypto.Random (MonadRandom, getRandomBytes)

-- | Represent a Rabin public key.
data PublicKey = PublicKey
    { public_size :: Int      -- ^ size of key in bytes
    , public_n    :: Integer  -- ^ public p*q
    } deriving (Show, Read, Eq, Data)

-- | Represent a Rabin private key.
data PrivateKey = PrivateKey
    { private_pub :: PublicKey
    , private_p   :: Integer   -- ^ p prime number
    , private_q   :: Integer   -- ^ q prime number
    , private_a   :: Integer
    , private_b   :: Integer
    } deriving (Show, Read, Eq, Data)

-- | Rabin Signature.
data Signature = Signature (Integer, Integer) deriving (Show, Read, Eq, Data)

-- | Generate a pair of (private, public) key of size in bytes.
-- Primes p and q are both congruent 3 mod 4.
--
-- See algorithm 8.11 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
generate :: MonadRandom m
         => Int
         -> m (PublicKey, PrivateKey)
generate size = do
    (p, q) <- generatePrimes size (\p -> p `mod` 4 == 3) (\q -> q `mod` 4 == 3)
    return $ generateKeys p q
  where 
    generateKeys p q =
        let n = p*q
            (a, b, _) = gcde p q 
            publicKey = PublicKey { public_size = size
                                    , public_n    = n }
            privateKey = PrivateKey { private_pub = publicKey
                                    , private_p   = p
                                    , private_q   = q
                                    , private_a   = a
                                    , private_b   = b }
            in (publicKey, privateKey)

-- | Encrypt plaintext using public key an a predefined OAEP seed.
--
-- See algorithm 8.11 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
encryptWithSeed :: HashAlgorithm hash
                => ByteString                               -- ^ Seed
                -> OAEPParams hash ByteString ByteString    -- ^ OAEP padding
                -> PublicKey                                -- ^ public key
                -> ByteString                               -- ^ plaintext
                -> Either Error ByteString
encryptWithSeed seed oaep pk m =
    let n  = public_n pk
        k  = numBytes n
     in do
        m' <- pad seed oaep k m
        let m'' = os2ip m'
        return $ i2osp $ expSafe m'' 2 n

-- | Encrypt plaintext using public key.
encrypt :: (HashAlgorithm hash, MonadRandom m)
        => OAEPParams hash ByteString ByteString    -- ^ OAEP padding parameters
        -> PublicKey                                -- ^ public key
        -> ByteString                               -- ^ plaintext 
        -> m (Either Error ByteString)
encrypt oaep pk m = do
    seed <- getRandomBytes hashLen
    return $ encryptWithSeed seed oaep pk m
  where
    hashLen = hashDigestSize (oaepHash oaep) 

-- | Decrypt ciphertext using private key.
--
-- See algorithm 8.12 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
decrypt :: HashAlgorithm hash
        => OAEPParams hash ByteString ByteString    -- ^ OAEP padding parameters
        -> PrivateKey                               -- ^ private key
        -> ByteString                               -- ^ ciphertext
        -> Maybe ByteString
decrypt oaep pk c =
    let p  = private_p pk 
        q  = private_q pk     
        a  = private_a pk 
        b  = private_b pk
        n  = public_n $ private_pub pk
        k  = numBytes n
        c' = os2ip c
        solutions = rights $ toList $ mapTuple (unpad oaep k . i2ospOf_ k) $ sqroot' c' p q a b n
     in if length solutions /= 1 then Nothing
        else Just $ head solutions
      where toList (w, x, y, z) = w:x:y:z:[]
            mapTuple f (w, x, y, z) = (f w, f x, f y, f z)

-- | Sign message using padding, hash algorithm and private key.
--
-- See <https://en.wikipedia.org/wiki/Rabin_signature_algorithm>.
signWith :: HashAlgorithm hash
         => ByteString    -- ^ padding
         -> PrivateKey    -- ^ private key
         -> hash          -- ^ hash function
         -> ByteString    -- ^ message to sign
         -> Either Error Signature
signWith padding pk hashAlg m = do
    h <- calculateHash padding pk hashAlg m
    signature <- calculateSignature h
    return signature
  where
    calculateSignature h =
        let p = private_p pk
            q = private_q pk     
            a = private_a pk 
            b = private_b pk
            n = public_n $ private_pub pk
         in if h >= n then Left MessageTooLong
            else let (r, _, _, _) = sqroot' h p q a b n
                  in Right $ Signature (os2ip padding, r)

-- | Sign message using hash algorithm and private key.
--
-- See <https://en.wikipedia.org/wiki/Rabin_signature_algorithm>.
sign :: (MonadRandom m, HashAlgorithm hash)
     => PrivateKey    -- ^ private key
     -> hash          -- ^ hash function
     -> ByteString    -- ^ message to sign
     -> m (Either Error Signature)
sign pk hashAlg m = do
    padding <- findPadding
    return $ signWith padding pk hashAlg m
  where 
    findPadding = do
        padding <- getRandomBytes 8
        case calculateHash padding pk hashAlg m of
            Right _ -> return padding
            _       -> findPadding

-- | Calculate hash of message and padding.
-- If the padding is valid, then the result of the hash operation is returned, otherwise an error.
calculateHash :: HashAlgorithm hash
              => ByteString    -- ^ padding
              -> PrivateKey    -- ^ private key
              -> hash          -- ^ hash function
              -> ByteString    -- ^ message to sign
              -> Either Error Integer
calculateHash padding pk hashAlg m = 
    let p = private_p pk
        q = private_q pk
        h = os2ip $ hashWith hashAlg $ B.append padding m
     in case (jacobi (h `mod` p) p, jacobi (h `mod` q) q) of
            (Just 1, Just 1) -> Right h
            _                -> Left InvalidParameters

-- | Verify signature using hash algorithm and public key.
--
-- See <https://en.wikipedia.org/wiki/Rabin_signature_algorithm>.
verify :: HashAlgorithm hash
       => PublicKey     -- ^ private key
       -> hash          -- ^ hash function
       -> ByteString    -- ^ message
       -> Signature     -- ^ signature
       -> Bool
verify pk hashAlg m (Signature (padding, s)) =
    let n  = public_n pk
        p  = i2osp padding
        h  = os2ip $ hashWith hashAlg $ B.append p m 
        h' = expSafe s 2 n
     in h' == h

-- | Square roots modulo prime p where p is congruent 3 mod 4
-- Value a must be a quadratic residue modulo p (i.e. jacobi symbol (a/n) = 1).
--
-- See algorithm 3.36 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
sqroot :: Integer
       -> Integer   -- ^ prime p
       -> (Integer, Integer)
sqroot a p =
    let r = expSafe a ((p + 1) `div` 4) p
     in (r, -r)

-- | Square roots modulo n given its prime factors p and q (both congruent 3 mod 4)
-- Value a must be a quadratic residue of both modulo p and modulo q (i.e. jacobi symbols (a/p) = (a/q) = 1).
-- 
-- See algorithm 3.44 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
sqroot' :: Integer 
        -> Integer  -- ^ prime p
        -> Integer  -- ^ prime q
        -> Integer  -- ^ c such that c*p + d*q = 1
        -> Integer  -- ^ d such that c*p + d*q = 1
        -> Integer  -- ^ n = p*q
        -> (Integer, Integer, Integer, Integer)
sqroot' a p q c d n =
    let (r, _) = sqroot a p
        (s, _) = sqroot a q
        x      = (r*d*q + s*c*p) `mod` n
        y      = (r*d*q - s*c*p) `mod` n
     in (x, (-x) `mod` n, y, (-y) `mod` n)
