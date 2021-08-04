-- |
-- Module      : Crypto.PubKey.DSA
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- An implementation of the Digital Signature Algorithm (DSA)
{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.PubKey.DSA
    ( Params(..)
    , Signature(..)
    , PublicKey(..)
    , PrivateKey(..)
    , PublicNumber
    , PrivateNumber
    -- * Generation
    , generatePrivate
    , calculatePublic
    -- * Signature primitive
    , sign
    , signWith
    -- * Verification primitive
    , verify
    -- * Key pair
    , KeyPair(..)
    , toPublicKey
    , toPrivateKey
    ) where


import Data.Data
import Data.Maybe

import Crypto.Number.ModArithmetic (expFast, expSafe, inverse)
import Crypto.Number.Generate
import Crypto.Internal.ByteArray (ByteArrayAccess)
import Crypto.Internal.Imports
import Crypto.Hash
import Crypto.PubKey.Internal (dsaTruncHash)
import Crypto.Random.Types

-- | DSA Public Number, usually embedded in DSA Public Key
type PublicNumber = Integer

-- | DSA Private Number, usually embedded in DSA Private Key
type PrivateNumber = Integer

-- | Represent DSA parameters namely P, G, and Q.
data Params = Params
    { params_p :: Integer -- ^ DSA p
    , params_g :: Integer -- ^ DSA g
    , params_q :: Integer -- ^ DSA q
    } deriving (Show,Read,Eq,Data)

instance NFData Params where
    rnf (Params p g q) = p `seq` g `seq` q `seq` ()

-- | Represent a DSA signature namely R and S.
data Signature = Signature
    { sign_r :: Integer -- ^ DSA r
    , sign_s :: Integer -- ^ DSA s
    } deriving (Show,Read,Eq,Data)

instance NFData Signature where
    rnf (Signature r s) = r `seq` s `seq` ()

-- | Represent a DSA public key.
data PublicKey = PublicKey
    { public_params :: Params       -- ^ DSA parameters
    , public_y      :: PublicNumber -- ^ DSA public Y
    } deriving (Show,Read,Eq,Data)

instance NFData PublicKey where
    rnf (PublicKey params y) = y `seq` params `seq` ()

-- | Represent a DSA private key.
--
-- Only x need to be secret.
-- the DSA parameters are publicly shared with the other side.
data PrivateKey = PrivateKey
    { private_params :: Params        -- ^ DSA parameters
    , private_x      :: PrivateNumber -- ^ DSA private X
    } deriving (Show,Read,Eq,Data)

instance NFData PrivateKey where
    rnf (PrivateKey params x) = x `seq` params `seq` ()

-- | Represent a DSA key pair
data KeyPair = KeyPair Params PublicNumber PrivateNumber
    deriving (Show,Read,Eq,Data)

instance NFData KeyPair where
    rnf (KeyPair params y x) = x `seq` y `seq` params `seq` ()

-- | Public key of a DSA Key pair
toPublicKey :: KeyPair -> PublicKey
toPublicKey (KeyPair params pub _) = PublicKey params pub

-- | Private key of a DSA Key pair
toPrivateKey :: KeyPair -> PrivateKey
toPrivateKey (KeyPair params _ priv) = PrivateKey params priv

-- | generate a private number with no specific property
-- this number is usually called X in DSA text.
generatePrivate :: MonadRandom m => Params -> m PrivateNumber
generatePrivate (Params _ _ q) = generateMax q

-- | Calculate the public number from the parameters and the private key
calculatePublic :: Params -> PrivateNumber -> PublicNumber
calculatePublic (Params p g _) x = expSafe g x p

-- | sign message using the private key and an explicit k number.
signWith :: (ByteArrayAccess msg, HashAlgorithm hash)
         => Integer         -- ^ k random number
         -> PrivateKey      -- ^ private key
         -> hash            -- ^ hash function
         -> msg             -- ^ message to sign
         -> Maybe Signature
signWith k pk hashAlg msg
    | r == 0 || s == 0  = Nothing
    | otherwise         = Just $ Signature r s
    where -- parameters
          (Params p g q) = private_params pk
          x              = private_x pk
          -- compute r,s
          kInv      = fromJust $ inverse k q
          hm        = dsaTruncHash hashAlg msg q
          r         = expSafe g k p `mod` q
          s         = (kInv * (hm + x * r)) `mod` q

-- | sign message using the private key.
sign :: (ByteArrayAccess msg, HashAlgorithm hash, MonadRandom m) => PrivateKey -> hash -> msg -> m Signature
sign pk hashAlg msg = do
    k <- generateMax q
    case signWith k pk hashAlg msg of
        Nothing  -> sign pk hashAlg msg
        Just sig -> return sig
  where
    (Params _ _ q) = private_params pk

-- | verify a bytestring using the public key.
verify :: (ByteArrayAccess msg, HashAlgorithm hash) => hash -> PublicKey -> Signature -> msg -> Bool
verify hashAlg pk (Signature r s) m
    -- Reject the signature if either 0 < r < q or 0 < s < q is not satisfied.
    | r <= 0 || r >= q || s <= 0 || s >= q = False
    | otherwise                            = v == r
    where (Params p g q) = public_params pk
          y       = public_y pk
          hm      = dsaTruncHash hashAlg m q
          w       = fromJust $ inverse s q
          u1      = (hm*w) `mod` q
          u2      = (r*w) `mod` q
          v       = ((expFast g u1 p) * (expFast y u2 p)) `mod` p `mod` q
