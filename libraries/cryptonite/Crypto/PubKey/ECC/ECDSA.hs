-- | /WARNING:/ Signature operations may leak the private key. Signature verification
-- should be safe.
{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.PubKey.ECC.ECDSA
    ( Signature(..)
    , PublicPoint
    , PublicKey(..)
    , PrivateNumber
    , PrivateKey(..)
    , KeyPair(..)
    , toPublicKey
    , toPrivateKey
    , signWith
    , signDigestWith
    , sign
    , signDigest
    , verify
    , verifyDigest
    ) where

import Control.Monad
import Data.Data

import Crypto.Hash
import Crypto.Internal.ByteArray (ByteArrayAccess)
import Crypto.Number.ModArithmetic (inverse)
import Crypto.Number.Generate
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.Internal (dsaTruncHashDigest)
import Crypto.Random.Types

-- | Represent a ECDSA signature namely R and S.
data Signature = Signature
    { sign_r :: Integer -- ^ ECDSA r
    , sign_s :: Integer -- ^ ECDSA s
    } deriving (Show,Read,Eq,Data)

-- | ECDSA Private Key.
data PrivateKey = PrivateKey
    { private_curve :: Curve
    , private_d     :: PrivateNumber
    } deriving (Show,Read,Eq,Data)

-- | ECDSA Public Key.
data PublicKey = PublicKey
    { public_curve :: Curve
    , public_q     :: PublicPoint
    } deriving (Show,Read,Eq,Data)

-- | ECDSA Key Pair.
data KeyPair = KeyPair Curve PublicPoint PrivateNumber
    deriving (Show,Read,Eq,Data)

-- | Public key of a ECDSA Key pair.
toPublicKey :: KeyPair -> PublicKey
toPublicKey (KeyPair curve pub _) = PublicKey curve pub

-- | Private key of a ECDSA Key pair.
toPrivateKey :: KeyPair -> PrivateKey
toPrivateKey (KeyPair curve _ priv) = PrivateKey curve priv

-- | Sign digest using the private key and an explicit k number.
--
-- /WARNING:/ Vulnerable to timing attacks.
signDigestWith :: HashAlgorithm hash
               => Integer     -- ^ k random number
               -> PrivateKey  -- ^ private key
               -> Digest hash -- ^ digest to sign
               -> Maybe Signature
signDigestWith k (PrivateKey curve d) digest = do
    let z = dsaTruncHashDigest digest n
        CurveCommon _ _ g n _ = common_curve curve
    let point = pointMul curve k g
    r <- case point of
              PointO    -> Nothing
              Point x _ -> return $ x `mod` n
    kInv <- inverse k n
    let s = kInv * (z + r * d) `mod` n
    when (r == 0 || s == 0) Nothing
    return $ Signature r s

-- | Sign message using the private key and an explicit k number.
--
-- /WARNING:/ Vulnerable to timing attacks.
signWith :: (ByteArrayAccess msg, HashAlgorithm hash)
         => Integer    -- ^ k random number
         -> PrivateKey -- ^ private key
         -> hash       -- ^ hash function
         -> msg        -- ^ message to sign
         -> Maybe Signature
signWith k pk hashAlg msg = signDigestWith k pk (hashWith hashAlg msg)

-- | Sign digest using the private key.
--
-- /WARNING:/ Vulnerable to timing attacks.
signDigest :: (HashAlgorithm hash, MonadRandom m)
           => PrivateKey -> Digest hash -> m Signature
signDigest pk digest = do
    k <- generateBetween 1 (n - 1)
    case signDigestWith k pk digest of
         Nothing  -> signDigest pk digest
         Just sig -> return sig
  where n = ecc_n . common_curve $ private_curve pk

-- | Sign message using the private key.
--
-- /WARNING:/ Vulnerable to timing attacks.
sign :: (ByteArrayAccess msg, HashAlgorithm hash, MonadRandom m)
     => PrivateKey -> hash -> msg -> m Signature
sign pk hashAlg msg = signDigest pk (hashWith hashAlg msg)

-- | Verify a digest using the public key.
verifyDigest :: HashAlgorithm hash => PublicKey -> Signature -> Digest hash -> Bool
verifyDigest (PublicKey _ PointO) _ _ = False
verifyDigest pk@(PublicKey curve q) (Signature r s) digest
    | r < 1 || r >= n || s < 1 || s >= n = False
    | otherwise = maybe False (r ==) $ do
        w <- inverse s n
        let z  = dsaTruncHashDigest digest n
            u1 = z * w `mod` n
            u2 = r * w `mod` n
            x  = pointAddTwoMuls curve u1 g u2 q
        case x of
             PointO     -> Nothing
             Point x1 _ -> return $ x1 `mod` n
  where n = ecc_n cc
        g = ecc_g cc
        cc = common_curve $ public_curve pk

-- | Verify a bytestring using the public key.
verify :: (ByteArrayAccess msg, HashAlgorithm hash) => hash -> PublicKey -> Signature -> msg -> Bool
verify hashAlg pk sig msg = verifyDigest pk sig (hashWith hashAlg msg)
