-- |
-- Module      : Crypto.PubKey.ECIES
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- IES with Elliptic curve <https://en.wikipedia.org/wiki/Integrated_Encryption_Scheme>
--
-- This is a simple cryptographic system between 2 parties using Elliptic Curve.
--
-- The sending party create a shared secret using the receiver public key, and use the shared secret
-- to generate cryptographic material for an symmetric encryption scheme (preferably authenticated encryption).
--
-- The receiving party receive the temporary ephemeral public key which is combined to its secret key
-- to create the shared secret which just like on the sending is used to generate cryptographic material.
--
-- This module doesn't provide any symmetric data encryption capability or any mean to derive
-- cryptographic key material for a symmetric key from the shared secret.
-- this is left to the user for now.
--
module Crypto.PubKey.ECIES
    ( deriveEncrypt
    , deriveDecrypt
    ) where

import           Crypto.ECC
import           Crypto.Error
import           Crypto.Random

-- | Generate random a new Shared secret and the associated point
-- to do a ECIES style encryption
deriveEncrypt :: (MonadRandom randomly, EllipticCurveDH curve)
              => proxy curve -- ^ representation of the curve
              -> Point curve -- ^ the public key of the receiver
              -> randomly (CryptoFailable (Point curve, SharedSecret))
deriveEncrypt proxy pub = do
    (KeyPair rPoint rScalar) <- curveGenerateKeyPair proxy
    return $ (\s -> (rPoint, s)) `fmap` ecdh proxy rScalar pub

-- | Derive the shared secret with the receiver key
-- and the R point of the scheme.
deriveDecrypt :: EllipticCurveDH curve
              => proxy curve  -- ^ representation of the curve
              -> Point curve  -- ^ The received R (supposedly, randomly generated on the encrypt side)
              -> Scalar curve -- ^ The secret key of the receiver
              -> CryptoFailable SharedSecret
deriveDecrypt proxy point secret = ecdh proxy secret point
