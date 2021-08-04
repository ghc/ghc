-- |
-- Module      : Crypto.PubKey.ECDSA
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Elliptic Curve Digital Signature Algorithm, with the parameterized
-- curve implementations provided by module "Crypto.ECC".
--
-- Public/private key pairs can be generated using
-- 'curveGenerateKeyPair' or decoded from binary.
--
-- /WARNING:/ Only curve P-256 has constant-time implementation.
-- Signature operations with P-384 and P-521 may leak the private key.
--
-- Signature verification should be safe for all curves.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Crypto.PubKey.ECDSA
    ( EllipticCurveECDSA (..)
    -- * Public keys
    , PublicKey
    , encodePublic
    , decodePublic
    , toPublic
    -- * Private keys
    , PrivateKey
    , encodePrivate
    , decodePrivate
    -- * Signatures
    , Signature(..)
    , signatureFromIntegers
    , signatureToIntegers
    -- * Generation and verification
    , signWith
    , signDigestWith
    , sign
    , signDigest
    , verify
    , verifyDigest
    ) where

import           Control.Monad

import           Crypto.ECC
import qualified Crypto.ECC.Simple.Types as Simple
import           Crypto.Error
import           Crypto.Hash
import           Crypto.Hash.Types
import           Crypto.Internal.ByteArray (ByteArray, ByteArrayAccess)
import           Crypto.Internal.Imports
import           Crypto.Number.ModArithmetic (inverseFermat)
import qualified Crypto.PubKey.ECC.P256 as P256
import           Crypto.Random.Types

import           Data.Bits
import qualified Data.ByteArray as B
import           Data.Data

import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peekByteOff, pokeByteOff)

-- | Represent a ECDSA signature namely R and S.
data Signature curve = Signature
    { sign_r :: Scalar curve -- ^ ECDSA r
    , sign_s :: Scalar curve -- ^ ECDSA s
    }

deriving instance Eq (Scalar curve) => Eq (Signature curve)
deriving instance Show (Scalar curve) => Show (Signature curve)

instance NFData (Scalar curve) => NFData (Signature curve) where
    rnf (Signature r s) = rnf r `seq` rnf s `seq` ()

-- | ECDSA Public Key.
type PublicKey curve = Point curve

-- | ECDSA Private Key.
type PrivateKey curve = Scalar curve

-- | Elliptic curves with ECDSA capabilities.
class EllipticCurveBasepointArith curve => EllipticCurveECDSA curve where
    -- | Is a scalar in the accepted range for ECDSA
    scalarIsValid :: proxy curve -> Scalar curve -> Bool

    -- | Test whether the scalar is zero
    scalarIsZero :: proxy curve -> Scalar curve -> Bool
    scalarIsZero prx s = s == throwCryptoError (scalarFromInteger prx 0)

    -- | Scalar inversion modulo the curve order
    scalarInv :: proxy curve -> Scalar curve -> Maybe (Scalar curve)

    -- | Return the point X coordinate as a scalar
    pointX :: proxy curve -> Point curve -> Maybe (Scalar curve)

instance EllipticCurveECDSA Curve_P256R1 where
    scalarIsValid _ s = not (P256.scalarIsZero s)
                            && P256.scalarCmp s P256.scalarN == LT

    scalarIsZero _ = P256.scalarIsZero

    scalarInv _ s = let inv = P256.scalarInvSafe s
                     in if P256.scalarIsZero inv then Nothing else Just inv

    pointX _  = P256.pointX

instance EllipticCurveECDSA Curve_P384R1 where
    scalarIsValid _ = ecScalarIsValid (Proxy :: Proxy Simple.SEC_p384r1)

    scalarIsZero _ = ecScalarIsZero

    scalarInv _ = ecScalarInv (Proxy :: Proxy Simple.SEC_p384r1)

    pointX _  = ecPointX (Proxy :: Proxy Simple.SEC_p384r1)

instance EllipticCurveECDSA Curve_P521R1 where
    scalarIsValid _ = ecScalarIsValid (Proxy :: Proxy Simple.SEC_p521r1)

    scalarIsZero _ = ecScalarIsZero

    scalarInv _ = ecScalarInv (Proxy :: Proxy Simple.SEC_p521r1)

    pointX _  = ecPointX (Proxy :: Proxy Simple.SEC_p521r1)


-- | Create a signature from integers (R, S).
signatureFromIntegers :: EllipticCurveECDSA curve
                      => proxy curve -> (Integer, Integer) -> CryptoFailable (Signature curve)
signatureFromIntegers prx (r, s) =
    liftA2 Signature (scalarFromInteger prx r) (scalarFromInteger prx s)

-- | Get integers (R, S) from a signature.
--
-- The values can then be used to encode the signature to binary with
-- ASN.1.
signatureToIntegers :: EllipticCurveECDSA curve
                    => proxy curve -> Signature curve -> (Integer, Integer)
signatureToIntegers prx sig =
    (scalarToInteger prx $ sign_r sig, scalarToInteger prx $ sign_s sig)

-- | Encode a public key into binary form, i.e. the uncompressed encoding
-- referenced from <https://tools.ietf.org/html/rfc5480 RFC 5480> section 2.2.
encodePublic :: (EllipticCurve curve, ByteArray bs)
             => proxy curve -> PublicKey curve -> bs
encodePublic = encodePoint

-- | Try to decode the binary form of a public key.
decodePublic :: (EllipticCurve curve, ByteArray bs)
             => proxy curve -> bs -> CryptoFailable (PublicKey curve)
decodePublic = decodePoint

-- | Encode a private key into binary form, i.e. the @privateKey@ field
-- described in <https://tools.ietf.org/html/rfc5915 RFC 5915>.
encodePrivate :: (EllipticCurveECDSA curve, ByteArray bs)
              => proxy curve -> PrivateKey curve -> bs
encodePrivate = encodeScalar

-- | Try to decode the binary form of a private key.
decodePrivate :: (EllipticCurveECDSA curve, ByteArray bs)
              => proxy curve -> bs -> CryptoFailable (PrivateKey curve)
decodePrivate = decodeScalar

-- | Create a public key from a private key.
toPublic :: EllipticCurveECDSA curve
         => proxy curve -> PrivateKey curve -> PublicKey curve
toPublic = pointBaseSmul

-- | Sign digest using the private key and an explicit k scalar.
signDigestWith :: (EllipticCurveECDSA curve, HashAlgorithm hash)
               => proxy curve -> Scalar curve -> PrivateKey curve -> Digest hash -> Maybe (Signature curve)
signDigestWith prx k d digest = do
    let z = tHashDigest prx digest
        point = pointBaseSmul prx k
    r <- pointX prx point
    kInv <- scalarInv prx k
    let s = scalarMul prx kInv (scalarAdd prx z (scalarMul prx r d))
    when (scalarIsZero prx r || scalarIsZero prx s) Nothing
    return $ Signature r s

-- | Sign message using the private key and an explicit k scalar.
signWith :: (EllipticCurveECDSA curve, ByteArrayAccess msg, HashAlgorithm hash)
         => proxy curve -> Scalar curve -> PrivateKey curve -> hash -> msg -> Maybe (Signature curve)
signWith prx k d hashAlg msg = signDigestWith prx k d (hashWith hashAlg msg)

-- | Sign a digest using hash and private key.
signDigest :: (EllipticCurveECDSA curve, MonadRandom m, HashAlgorithm hash)
           => proxy curve -> PrivateKey curve -> Digest hash -> m (Signature curve)
signDigest prx pk digest = do
    k <- curveGenerateScalar prx
    case signDigestWith prx k pk digest of
        Nothing  -> signDigest prx pk digest
        Just sig -> return sig

-- | Sign a message using hash and private key.
sign :: (EllipticCurveECDSA curve, MonadRandom m, ByteArrayAccess msg, HashAlgorithm hash)
     => proxy curve -> PrivateKey curve -> hash -> msg -> m (Signature curve)
sign prx pk hashAlg msg = signDigest prx pk (hashWith hashAlg msg)

-- | Verify a digest using hash and public key.
verifyDigest :: (EllipticCurveECDSA curve, HashAlgorithm hash)
       => proxy curve -> PublicKey curve -> Signature curve -> Digest hash -> Bool
verifyDigest prx q (Signature r s) digest
    | not (scalarIsValid prx r) = False
    | not (scalarIsValid prx s) = False
    | otherwise = maybe False (r ==) $ do
        w <- scalarInv prx s
        let z  = tHashDigest prx digest
            u1 = scalarMul prx z w
            u2 = scalarMul prx r w
            x  = pointsSmulVarTime prx u1 u2 q
        pointX prx x
    -- Note: precondition q /= PointO is not tested because we assume
    -- point decoding never decodes point at infinity.

-- | Verify a signature using hash and public key.
verify :: (EllipticCurveECDSA curve, ByteArrayAccess msg, HashAlgorithm hash)
       => proxy curve -> hash -> PublicKey curve -> Signature curve -> msg -> Bool
verify prx hashAlg q sig msg = verifyDigest prx q sig (hashWith hashAlg msg)

-- | Truncate a digest based on curve order size.
tHashDigest :: (EllipticCurveECDSA curve, HashAlgorithm hash)
            => proxy curve -> Digest hash -> Scalar curve
tHashDigest prx (Digest digest) = throwCryptoError $ decodeScalar prx encoded
  where m      = curveOrderBits prx
        d      = m - B.length digest * 8
        (n, r) = m `divMod` 8
        n'     = if r > 0 then succ n else n

        encoded
            | d >  0    = B.zero (n' - B.length digest) `B.append` digest
            | d == 0    = digest
            | r == 0    = B.take n digest
            | otherwise = shiftBytes digest

        shiftBytes bs = B.allocAndFreeze n' $ \dst ->
            B.withByteArray bs $ \src -> go dst src 0 0

        go :: Ptr Word8 -> Ptr Word8 -> Word8 -> Int -> IO ()
        go dst src !a i
            | i >= n'   = return ()
            | otherwise = do
                b <- peekByteOff src i
                pokeByteOff dst i (unsafeShiftR b (8 - r) .|. unsafeShiftL a r)
                go dst src b (succ i)


ecScalarIsValid :: Simple.Curve c => proxy c -> Simple.Scalar c -> Bool
ecScalarIsValid prx (Simple.Scalar s) = s > 0 && s < n
  where n = Simple.curveEccN $ Simple.curveParameters prx

ecScalarIsZero :: forall curve . Simple.Curve curve
               => Simple.Scalar curve -> Bool
ecScalarIsZero (Simple.Scalar a) = a == 0

ecScalarInv :: Simple.Curve c
            => proxy c -> Simple.Scalar c -> Maybe (Simple.Scalar c)
ecScalarInv prx (Simple.Scalar s)
    | i == 0    = Nothing
    | otherwise = Just $ Simple.Scalar i
  where n = Simple.curveEccN $ Simple.curveParameters prx
        i = inverseFermat s n

ecPointX :: Simple.Curve c
         => proxy c -> Simple.Point c -> Maybe (Simple.Scalar c)
ecPointX _   Simple.PointO      = Nothing
ecPointX prx (Simple.Point x _) = Just (Simple.Scalar $ x `mod` n)
  where n = Simple.curveEccN $ Simple.curveParameters prx
