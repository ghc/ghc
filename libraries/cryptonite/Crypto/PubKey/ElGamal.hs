-- |
-- Module      : Crypto.PubKey.ElGamal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- This module is a work in progress. do not use:
-- it might eat your dog, your data or even both.
--
-- TODO: provide a mapping between integer and ciphertext
--       generate numbers correctly
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.PubKey.ElGamal
    ( Params
    , PublicNumber
    , PrivateNumber
    , EphemeralKey(..)
    , SharedKey
    , Signature
    -- * Generation
    , generatePrivate
    , generatePublic
    -- * Encryption and decryption with no scheme
    , encryptWith
    , encrypt
    , decrypt
    -- * Signature primitives
    , signWith
    , sign
    -- * Verification primitives
    , verify
    ) where

import Data.Maybe (fromJust)
import Crypto.Internal.Imports
import Crypto.Internal.ByteArray (ByteArrayAccess)
import Crypto.Number.ModArithmetic (expSafe, expFast, inverse)
import Crypto.Number.Generate (generateMax)
import Crypto.Number.Serialize (os2ip)
import Crypto.Number.Basic (gcde)
import Crypto.Random.Types
import Crypto.PubKey.DH (PrivateNumber(..), PublicNumber(..), Params(..), SharedKey(..))
import Crypto.Hash

-- | ElGamal Signature
data Signature = Signature (Integer, Integer)

-- | ElGamal Ephemeral key. also called Temporary key.
newtype EphemeralKey = EphemeralKey Integer
    deriving (NFData)

-- | generate a private number with no specific property
-- this number is usually called a and need to be between
-- 0 and q (order of the group G).
--
generatePrivate :: MonadRandom m => Integer -> m PrivateNumber
generatePrivate q = PrivateNumber <$> generateMax q

-- | generate an ephemeral key which is a number with no specific property,
-- and need to be between 0 and q (order of the group G).
--
generateEphemeral :: MonadRandom m => Integer -> m EphemeralKey
generateEphemeral q = toEphemeral <$> generatePrivate q
    where toEphemeral (PrivateNumber n) = EphemeralKey n

-- | generate a public number that is for the other party benefits.
-- this number is usually called h=g^a
generatePublic :: Params -> PrivateNumber -> PublicNumber
generatePublic (Params p g _) (PrivateNumber a) = PublicNumber $ expSafe g a p

-- | encrypt with a specified ephemeral key
-- do not reuse ephemeral key.
encryptWith :: EphemeralKey -> Params -> PublicNumber -> Integer -> (Integer,Integer)
encryptWith (EphemeralKey b) (Params p g _) (PublicNumber h) m = (c1,c2)
    where s  = expSafe h b p
          c1 = expSafe g b p
          c2 = (s * m) `mod` p

-- | encrypt a message using params and public keys
-- will generate b (called the ephemeral key)
encrypt :: MonadRandom m => Params -> PublicNumber -> Integer -> m (Integer,Integer)
encrypt params@(Params p _ _) public m = (\b -> encryptWith b params public m) <$> generateEphemeral q
    where q = p-1 -- p is prime, hence order of the group is p-1

-- | decrypt message
decrypt :: Params -> PrivateNumber -> (Integer, Integer) -> Integer
decrypt (Params p _ _) (PrivateNumber a) (c1,c2) = (c2 * sm1) `mod` p
    where s   = expSafe c1 a p
          sm1 = fromJust $ inverse s p -- always inversible in Zp

-- | sign a message with an explicit k number
--
-- if k is not appropriate, then no signature is returned.
--
-- with some appropriate value of k, the signature generation can fail,
-- and no signature is returned. User of this function need to retry
-- with a different k value.
signWith :: (ByteArrayAccess msg, HashAlgorithm hash)
         => Integer         -- ^ random number k, between 0 and p-1 and gcd(k,p-1)=1
         -> Params          -- ^ DH params (p,g)
         -> PrivateNumber   -- ^ DH private key
         -> hash            -- ^ collision resistant hash algorithm
         -> msg             -- ^ message to sign
         -> Maybe Signature
signWith k (Params p g _) (PrivateNumber x) hashAlg msg
    | k >= p-1 || d > 1 = Nothing -- gcd(k,p-1) is not 1
    | s == 0            = Nothing
    | otherwise         = Just $ Signature (r,s)
    where r          = expSafe g k p
          h          = os2ip $ hashWith hashAlg msg
          s          = ((h - x*r) * kInv) `mod` (p-1)
          (kInv,_,d) = gcde k (p-1)

-- | sign message
--
-- This function will generate a random number, however
-- as the signature might fail, the function will automatically retry
-- until a proper signature has been created.
--
sign :: (ByteArrayAccess msg, HashAlgorithm hash, MonadRandom m)
     => Params         -- ^ DH params (p,g)
     -> PrivateNumber  -- ^ DH private key
     -> hash           -- ^ collision resistant hash algorithm
     -> msg            -- ^ message to sign
     -> m Signature
sign params@(Params p _ _) priv hashAlg msg = do
    k <- generateMax (p-1)
    case signWith k params priv hashAlg msg of
        Nothing  -> sign params priv hashAlg msg
        Just sig -> return sig

-- | verify a signature
verify :: (ByteArrayAccess msg, HashAlgorithm hash)
       => Params
       -> PublicNumber
       -> hash
       -> msg
       -> Signature
       -> Bool
verify (Params p g _) (PublicNumber y) hashAlg msg (Signature (r,s))
    | or [r <= 0,r >= p,s <= 0,s >= (p-1)] = False
    | otherwise                            = lhs == rhs
    where h   = os2ip $ hashWith hashAlg msg
          lhs = expFast g h p
          rhs = (expFast y r p * expFast r s p) `mod` p
