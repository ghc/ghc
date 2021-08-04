-- |
-- Module      : Crypto.PubKey.EdDSA
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- EdDSA signature generation and verification, implemented in Haskell and
-- parameterized with elliptic curve and hash algorithm.  Only edwards25519 is
-- supported at the moment.
--
-- The module provides \"context\" and \"prehash\" variants defined in
-- <https://tools.ietf.org/html/rfc8032 RFC 8032>.
--
-- This implementation is most useful when wanting to customize the hash
-- algorithm.  See module "Crypto.PubKey.Ed25519" for faster Ed25519 with
-- SHA-512.
--
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Crypto.PubKey.EdDSA
    ( SecretKey
    , PublicKey
    , Signature
    -- * Curves with EdDSA implementation
    , EllipticCurveEdDSA(CurveDigestSize)
    , publicKeySize
    , secretKeySize
    , signatureSize
    -- * Smart constructors
    , signature
    , publicKey
    , secretKey
    -- * Methods
    , toPublic
    , sign
    , signCtx
    , signPh
    , verify
    , verifyCtx
    , verifyPh
    , generateSecretKey
    ) where

import           Data.Bits
import           Data.ByteArray (ByteArray, ByteArrayAccess, Bytes, ScrubbedBytes, View)
import qualified Data.ByteArray as B
import           Data.ByteString (ByteString)
import           Data.Proxy

import           Crypto.ECC
import qualified Crypto.ECC.Edwards25519 as Edwards25519
import           Crypto.Error
import           Crypto.Hash (Digest)
import           Crypto.Hash.IO
import           Crypto.Random

import           GHC.TypeLits (KnownNat, Nat)

import           Crypto.Internal.Builder
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Internal.Nat (integralNatVal)

import           Foreign.Storable


-- API

-- | An EdDSA Secret key
newtype SecretKey curve = SecretKey ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | An EdDSA public key
newtype PublicKey curve hash = PublicKey Bytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | An EdDSA signature
newtype Signature curve hash = Signature Bytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | Elliptic curves with an implementation of EdDSA
class ( EllipticCurveBasepointArith curve
      , KnownNat (CurveDigestSize curve)
      ) => EllipticCurveEdDSA curve where

    -- | Size of the digest for this curve (in bytes)
    type CurveDigestSize curve :: Nat

    -- | Size of secret keys for this curve (in bytes)
    secretKeySize :: proxy curve -> Int

    -- hash with specified parameters
    hashWithDom :: (HashAlgorithm hash, ByteArrayAccess ctx, ByteArrayAccess msg)
                => proxy curve -> hash -> Bool -> ctx -> Builder -> msg -> Bytes

    -- conversion between scalar, point and public key
    pointPublic :: proxy curve -> Point curve -> PublicKey curve hash
    publicPoint :: proxy curve -> PublicKey curve hash -> CryptoFailable (Point curve)
    encodeScalarLE :: ByteArray bs => proxy curve -> Scalar curve -> bs
    decodeScalarLE :: ByteArrayAccess bs => proxy curve -> bs -> CryptoFailable (Scalar curve)

    -- how to use bits in a secret key
    scheduleSecret :: ( HashAlgorithm hash
                      , HashDigestSize hash ~ CurveDigestSize curve
                      )
                   => proxy curve
                   -> hash
                   -> SecretKey curve
                   -> (Scalar curve, View Bytes)

-- | Size of public keys for this curve (in bytes)
publicKeySize :: EllipticCurveEdDSA curve => proxy curve -> Int
publicKeySize prx = signatureSize prx `div` 2

-- | Size of signatures for this curve (in bytes)
signatureSize :: forall proxy curve . EllipticCurveEdDSA curve
              => proxy curve -> Int
signatureSize _ = integralNatVal (Proxy :: Proxy (CurveDigestSize curve))


-- Constructors

-- | Try to build a public key from a bytearray
publicKey :: ( EllipticCurveEdDSA curve
             , HashAlgorithm hash
             , HashDigestSize hash ~ CurveDigestSize curve
             , ByteArrayAccess ba
             )
          => proxy curve -> hash -> ba -> CryptoFailable (PublicKey curve hash)
publicKey prx _ bs
    | B.length bs == publicKeySize prx =
        CryptoPassed (PublicKey $ B.convert bs)
    | otherwise =
        CryptoFailed CryptoError_PublicKeySizeInvalid

-- | Try to build a secret key from a bytearray
secretKey :: (EllipticCurveEdDSA curve, ByteArrayAccess ba)
          => proxy curve -> ba -> CryptoFailable (SecretKey curve)
secretKey prx bs
    | B.length bs == secretKeySize prx =
        CryptoPassed (SecretKey $ B.convert bs)
    | otherwise                        =
        CryptoFailed CryptoError_SecretKeyStructureInvalid

-- | Try to build a signature from a bytearray
signature :: ( EllipticCurveEdDSA curve
             , HashAlgorithm hash
             , HashDigestSize hash ~ CurveDigestSize curve
             , ByteArrayAccess ba
             )
          => proxy curve -> hash -> ba -> CryptoFailable (Signature curve hash)
signature prx _ bs
    | B.length bs == signatureSize prx =
        CryptoPassed (Signature $ B.convert bs)
    | otherwise =
        CryptoFailed CryptoError_SecretKeyStructureInvalid


-- Conversions

-- | Generate a secret key
generateSecretKey :: (EllipticCurveEdDSA curve, MonadRandom m)
                  => proxy curve -> m (SecretKey curve)
generateSecretKey prx = SecretKey <$> getRandomBytes (secretKeySize prx)

-- | Create a public key from a secret key
toPublic :: ( EllipticCurveEdDSA curve
            , HashAlgorithm hash
            , HashDigestSize hash ~ CurveDigestSize curve
            )
         => proxy curve -> hash -> SecretKey curve -> PublicKey curve hash
toPublic prx alg priv =
    let p = pointBaseSmul prx (secretScalar prx alg priv)
     in pointPublic prx p

secretScalar :: ( EllipticCurveEdDSA curve
                , HashAlgorithm hash
                , HashDigestSize hash ~ CurveDigestSize curve
                )
             => proxy curve -> hash -> SecretKey curve -> Scalar curve
secretScalar prx alg priv = fst (scheduleSecret prx alg priv)


-- EdDSA signature generation & verification

-- | Sign a message using the key pair
sign :: ( EllipticCurveEdDSA curve
        , HashAlgorithm hash
        , HashDigestSize hash ~ CurveDigestSize curve
        , ByteArrayAccess msg
        )
     => proxy curve -> SecretKey curve -> PublicKey curve hash -> msg -> Signature curve hash
sign prx = signCtx prx emptyCtx

-- | Verify a message
verify :: ( EllipticCurveEdDSA curve
          , HashAlgorithm hash
          , HashDigestSize hash ~ CurveDigestSize curve
          , ByteArrayAccess msg
          )
       => proxy curve -> PublicKey curve hash -> msg -> Signature curve hash -> Bool
verify prx = verifyCtx prx emptyCtx

-- | Sign a message using the key pair under context @ctx@
signCtx :: ( EllipticCurveEdDSA curve
           , HashAlgorithm hash
           , HashDigestSize hash ~ CurveDigestSize curve
           , ByteArrayAccess ctx
           , ByteArrayAccess msg
           )
        => proxy curve -> ctx -> SecretKey curve -> PublicKey curve hash -> msg -> Signature curve hash
signCtx prx = signPhCtx prx False

-- | Verify a message under context @ctx@
verifyCtx :: ( EllipticCurveEdDSA curve
             , HashAlgorithm hash
             , HashDigestSize hash ~ CurveDigestSize curve
             , ByteArrayAccess ctx
             , ByteArrayAccess msg
             )
          => proxy curve -> ctx -> PublicKey curve hash -> msg -> Signature curve hash -> Bool
verifyCtx prx = verifyPhCtx prx False

-- | Sign a prehashed message using the key pair under context @ctx@
signPh :: ( EllipticCurveEdDSA curve
          , HashAlgorithm hash
          , HashDigestSize hash ~ CurveDigestSize curve
          , ByteArrayAccess ctx
          )
       => proxy curve -> ctx -> SecretKey curve -> PublicKey curve hash -> Digest prehash -> Signature curve hash
signPh prx = signPhCtx prx True

-- | Verify a prehashed message under context @ctx@
verifyPh :: ( EllipticCurveEdDSA curve
            , HashAlgorithm hash
            , HashDigestSize hash ~ CurveDigestSize curve
            , ByteArrayAccess ctx
            )
         => proxy curve -> ctx -> PublicKey curve hash -> Digest prehash -> Signature curve hash -> Bool
verifyPh prx = verifyPhCtx prx True

signPhCtx :: forall proxy curve hash ctx msg .
             ( EllipticCurveEdDSA curve
             , HashAlgorithm hash
             , HashDigestSize hash ~ CurveDigestSize curve
             , ByteArrayAccess ctx
             , ByteArrayAccess msg
             )
          => proxy curve -> Bool -> ctx -> SecretKey curve -> PublicKey curve hash -> msg -> Signature curve hash
signPhCtx prx ph ctx priv pub msg =
    let alg  = undefined :: hash
        (s, prefix) = scheduleSecret prx alg priv
        digR = hashWithDom prx alg ph ctx (bytes prefix) msg
        r    = decodeScalarNoErr prx digR
        pR   = pointBaseSmul prx r
        bsR  = encodePoint prx pR
        sK   = getK prx ph ctx pub bsR msg
        sS   = scalarAdd prx r (scalarMul prx sK s)
     in encodeSignature prx (bsR, pR, sS)

verifyPhCtx :: ( EllipticCurveEdDSA curve
               , HashAlgorithm hash
               , HashDigestSize hash ~ CurveDigestSize curve
               , ByteArrayAccess ctx
               , ByteArrayAccess msg
               )
            => proxy curve -> Bool -> ctx -> PublicKey curve hash -> msg -> Signature curve hash -> Bool
verifyPhCtx prx ph ctx pub msg sig =
    case doVerify of
        CryptoPassed verified -> verified
        CryptoFailed _        -> False
  where
    doVerify = do
        (bsR, pR, sS) <- decodeSignature prx sig
        nPub <- pointNegate prx `fmap` publicPoint prx pub
        let sK  = getK prx ph ctx pub bsR msg
            pR' = pointsSmulVarTime prx sS sK nPub
        return (pR == pR')

emptyCtx :: Bytes
emptyCtx = B.empty

getK :: forall proxy curve hash ctx msg .
        ( EllipticCurveEdDSA curve
        , HashAlgorithm hash
        , HashDigestSize hash ~ CurveDigestSize curve
        , ByteArrayAccess ctx
        , ByteArrayAccess msg
        )
     => proxy curve -> Bool -> ctx -> PublicKey curve hash -> Bytes -> msg -> Scalar curve
getK prx ph ctx (PublicKey pub) bsR msg =
    let alg  = undefined :: hash
        digK = hashWithDom prx alg ph ctx (bytes bsR <> bytes pub) msg
     in decodeScalarNoErr prx digK

encodeSignature :: EllipticCurveEdDSA curve
                => proxy curve
                -> (Bytes, Point curve, Scalar curve)
                -> Signature curve hash
encodeSignature prx (bsR, _, sS) = Signature $ buildAndFreeze $
    bytes bsR <> bytes bsS <> zero len0
  where
    bsS  = encodeScalarLE prx sS :: Bytes
    len0 = signatureSize prx - B.length bsR - B.length bsS

decodeSignature :: ( EllipticCurveEdDSA curve
                   , HashDigestSize hash ~ CurveDigestSize curve
                   )
                => proxy curve
                -> Signature curve hash
                -> CryptoFailable (Bytes, Point curve, Scalar curve)
decodeSignature prx (Signature bs) = do
    let (bsR, bsS) = B.splitAt (publicKeySize prx) bs
    pR <- decodePoint prx bsR
    sS <- decodeScalarLE prx bsS
    return (bsR, pR, sS)

-- implementations are supposed to decode any scalar up to the size of the digest
decodeScalarNoErr :: (EllipticCurveEdDSA curve, ByteArrayAccess bs)
                  => proxy curve -> bs -> Scalar curve
decodeScalarNoErr prx = unwrap "decodeScalarNoErr" . decodeScalarLE prx

unwrap :: String -> CryptoFailable a -> a
unwrap name (CryptoFailed _) = error (name ++ ": assumption failed")
unwrap _    (CryptoPassed x) = x


-- Ed25519 implementation

instance EllipticCurveEdDSA Curve_Edwards25519 where
    type CurveDigestSize Curve_Edwards25519 = 64
    secretKeySize _ = 32

    hashWithDom _ alg ph ctx bss
        | not ph && B.null ctx = digestDomMsg alg bss
        | otherwise            = digestDomMsg alg (dom <> bss)
      where dom = bytes ("SigEd25519 no Ed25519 collisions" :: ByteString) <>
                  byte (if ph then 1 else 0) <>
                  byte (fromIntegral $ B.length ctx) <>
                  bytes ctx

    pointPublic _ = PublicKey . Edwards25519.pointEncode
    publicPoint _ = Edwards25519.pointDecode
    encodeScalarLE _ = Edwards25519.scalarEncode
    decodeScalarLE _ = Edwards25519.scalarDecodeLong

    scheduleSecret prx alg priv =
        (decodeScalarNoErr prx clamped, B.dropView hashed 32)
      where
        hashed  = digest alg $ \update -> update priv

        clamped :: Bytes
        clamped = B.copyAndFreeze (B.takeView hashed 32) $ \p -> do
                      b0  <- peekElemOff p 0  :: IO Word8
                      b31 <- peekElemOff p 31 :: IO Word8
                      pokeElemOff p 31 ((b31 .&. 0x7F) .|. 0x40)
                      pokeElemOff p 0  (b0 .&. 0xF8)


{-
  Optimize hashing by limiting the number of roundtrips between Haskell and C.
  Hash "update" functions do not use unsafe FFI call, so better concanetate
  small fragments together and call the update function once.

  Using the IO hash interface avoids context buffer copies.

  Data type Digest is not used directly but converted to Bytes early. Any use of
  withByteArray on the unpinned Digest backend would require copy through a
  pinned trampoline.
-}

digestDomMsg :: (HashAlgorithm alg, ByteArrayAccess msg)
             => alg -> Builder -> msg -> Bytes
digestDomMsg alg bss bs = digest alg $ \update ->
    update (buildAndFreeze bss :: Bytes) >> update bs

digest :: HashAlgorithm alg
       => alg
       -> ((forall bs . ByteArrayAccess bs => bs -> IO ()) -> IO ())
       -> Bytes
digest alg fn = B.convert $ unsafeDoIO $ do
    mc <- hashMutableInitWith alg
    fn (hashMutableUpdate mc)
    hashMutableFinalize mc
