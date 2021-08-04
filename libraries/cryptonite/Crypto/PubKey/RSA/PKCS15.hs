-- |
-- Module      : Crypto.PubKey.RSA.PKCS15
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.PubKey.RSA.PKCS15
    (
    -- * Padding and unpadding
      pad
    , padSignature
    , unpad
    -- * Private key operations
    , decrypt
    , decryptSafer
    , sign
    , signSafer
    -- * Public key operations
    , encrypt
    , verify
    -- * Hash ASN1 description
    , HashAlgorithmASN1
    ) where

import           Crypto.Random.Types
import           Crypto.PubKey.Internal (and')
import           Crypto.PubKey.RSA.Types
import           Crypto.PubKey.RSA.Prim
import           Crypto.PubKey.RSA (generateBlinder)
import           Crypto.Hash

import           Data.ByteString (ByteString)
import           Data.Word

import           Crypto.Internal.ByteArray (ByteArray, Bytes)
import qualified Crypto.Internal.ByteArray as B

-- | A specialized class for hash algorithm that can product
-- a ASN1 wrapped description the algorithm plus the content
-- of the digest.
class HashAlgorithm hashAlg => HashAlgorithmASN1 hashAlg where
    -- | Convert a Digest into an ASN1 wrapped descriptive ByteArray
    hashDigestASN1 :: ByteArray out => Digest hashAlg -> out

-- http://uk.emc.com/emc-plus/rsa-labs/pkcs/files/h11300-wp-pkcs-1v2-2-rsa-cryptography-standard.pdf
-- EMSA-PKCS1-v1_5
instance HashAlgorithmASN1 MD2 where
    hashDigestASN1 = addDigestPrefix [0x30,0x20,0x30,0x0c,0x06,0x08,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x02,0x02,0x05,0x00,0x04,0x10]
instance HashAlgorithmASN1 MD5 where
    hashDigestASN1 = addDigestPrefix [0x30,0x20,0x30,0x0c,0x06,0x08,0x2a,0x86,0x48,0x86,0xf7,0x0d,0x02,0x05,0x05,0x00,0x04,0x10]
instance HashAlgorithmASN1 SHA1 where
    hashDigestASN1 = addDigestPrefix [0x30,0x21,0x30,0x09,0x06,0x05,0x2b,0x0e,0x03,0x02,0x1a,0x05,0x00,0x04,0x14]
instance HashAlgorithmASN1 SHA224 where
    hashDigestASN1 = addDigestPrefix [0x30,0x2d,0x30,0x0d,0x06,0x09,0x60,0x86,0x48,0x01,0x65,0x03,0x04,0x02,0x04,0x05,0x00,0x04,0x1c]
instance HashAlgorithmASN1 SHA256 where
    hashDigestASN1 = addDigestPrefix [0x30,0x31,0x30,0x0d,0x06,0x09,0x60,0x86,0x48,0x01,0x65,0x03,0x04,0x02,0x01,0x05,0x00,0x04,0x20]
instance HashAlgorithmASN1 SHA384 where
    hashDigestASN1 = addDigestPrefix [0x30,0x41,0x30,0x0d,0x06,0x09,0x60,0x86,0x48,0x01,0x65,0x03,0x04,0x02,0x02,0x05,0x00,0x04,0x30]
instance HashAlgorithmASN1 SHA512 where
    hashDigestASN1 = addDigestPrefix [0x30,0x51,0x30,0x0d,0x06,0x09,0x60,0x86,0x48,0x01,0x65,0x03,0x04,0x02,0x03,0x05,0x00,0x04,0x40]
instance HashAlgorithmASN1 SHA512t_224 where
    hashDigestASN1 = addDigestPrefix [0x30,0x2d,0x30,0x0d,0x06,0x09,0x60,0x86,0x48,0x01,0x65,0x03,0x04,0x02,0x05,0x05,0x00,0x04,0x1c]
instance HashAlgorithmASN1 SHA512t_256 where
    hashDigestASN1 = addDigestPrefix [0x30,0x31,0x30,0x0d,0x06,0x09,0x60,0x86,0x48,0x01,0x65,0x03,0x04,0x02,0x06,0x05,0x00,0x04,0x20]
instance HashAlgorithmASN1 RIPEMD160 where
    hashDigestASN1 = addDigestPrefix [0x30,0x21,0x30,0x09,0x06,0x05,0x2b,0x24,0x03,0x02,0x01,0x05,0x00,0x04,0x14]

--
-- ** Hack **
--
-- this happens to not need a real ASN1 encoder, because
-- thanks to the digest being a specific size AND
-- that the digest data is the last bytes in the encoding,
-- this allows to just prepend the right prefix to the
-- computed digest, to make it in the expected and valid shape.
--
-- Otherwise the expected structure is in the following form:
--
--   Start Sequence
--     ,Start Sequence
--       ,OID oid
--       ,Null
--     ,End Sequence
--     ,OctetString digest
--   ,End Sequence
addDigestPrefix :: ByteArray out => [Word8] -> Digest hashAlg -> out
addDigestPrefix prefix digest =
    B.pack prefix `B.append` B.convert digest

-- | This produce a standard PKCS1.5 padding for encryption
pad :: (MonadRandom m, ByteArray message) => Int -> message -> m (Either Error message)
pad len m
    | B.length m > len - 11 = return (Left MessageTooLong)
    | otherwise             = do
        padding <- getNonNullRandom (len - B.length m - 3)
        return $ Right $ B.concat [ B.pack [0,2], padding, B.pack [0], m ]

  where
    -- get random non-null bytes
    getNonNullRandom :: (ByteArray bytearray, MonadRandom m) => Int -> m bytearray
    getNonNullRandom n = do
        bs0 <- getRandomBytes n
        let bytes = B.pack $ filter (/= 0) $ B.unpack (bs0 :: Bytes)
            left  = n - B.length bytes
        if left == 0
            then return bytes
            else do bend <- getNonNullRandom left
                    return (bytes `B.append` bend)

-- | Produce a standard PKCS1.5 padding for signature
padSignature :: ByteArray signature => Int -> signature -> Either Error signature
padSignature klen signature
    | klen < siglen + 11 = Left SignatureTooLong
    | otherwise          = Right (B.pack padding `B.append` signature)
  where
        siglen    = B.length signature
        padding   = 0 : 1 : (replicate (klen - siglen - 3) 0xff ++ [0])

-- | Try to remove a standard PKCS1.5 encryption padding.
unpad :: ByteArray bytearray => bytearray -> Either Error bytearray
unpad packed
    | paddingSuccess = Right m
    | otherwise      = Left MessageNotRecognized
  where
        (zt, ps0m)   = B.splitAt 2 packed
        (ps, zm)     = B.span (/= 0) ps0m
        (z, m)       = B.splitAt 1 zm
        paddingSuccess = and' [ zt `B.constEq` (B.pack [0,2] :: Bytes)
                              , z == B.zero 1
                              , B.length ps >= 8
                              ]

-- | decrypt message using the private key.
--
-- When the decryption is not in a context where an attacker could gain
-- information from the timing of the operation, the blinder can be set to None.
--
-- If unsure always set a blinder or use decryptSafer
--
-- The message is returned un-padded.
decrypt :: Maybe Blinder -- ^ optional blinder
        -> PrivateKey    -- ^ RSA private key
        -> ByteString    -- ^ cipher text
        -> Either Error ByteString
decrypt blinder pk c
    | B.length c /= (private_size pk) = Left MessageSizeIncorrect
    | otherwise                       = unpad $ dp blinder pk c

-- | decrypt message using the private key and by automatically generating a blinder.
decryptSafer :: MonadRandom m
             => PrivateKey -- ^ RSA private key
             -> ByteString -- ^ cipher text
             -> m (Either Error ByteString)
decryptSafer pk b = do
    blinder <- generateBlinder (private_n pk)
    return (decrypt (Just blinder) pk b)

-- | encrypt a bytestring using the public key.
--
-- The message needs to be smaller than the key size - 11.
-- The message should not be padded.
encrypt :: MonadRandom m => PublicKey -> ByteString -> m (Either Error ByteString)
encrypt pk m = do
    r <- pad (public_size pk) m
    case r of
        Left err -> return $ Left err
        Right em -> return $ Right (ep pk em)

-- | sign message using private key, a hash and its ASN1 description
--
-- When the signature is not in a context where an attacker could gain
-- information from the timing of the operation, the blinder can be set to None.
--
-- If unsure always set a blinder or use signSafer
sign :: HashAlgorithmASN1 hashAlg
     => Maybe Blinder -- ^ optional blinder
     -> Maybe hashAlg -- ^ hash algorithm
     -> PrivateKey    -- ^ private key
     -> ByteString    -- ^ message to sign
     -> Either Error ByteString
sign blinder hashDescr pk m = dp blinder pk `fmap` makeSignature hashDescr (private_size pk) m

-- | sign message using the private key and by automatically generating a blinder.
signSafer :: (HashAlgorithmASN1 hashAlg, MonadRandom m)
          => Maybe hashAlg -- ^ Hash algorithm
          -> PrivateKey    -- ^ private key
          -> ByteString    -- ^ message to sign
          -> m (Either Error ByteString)
signSafer hashAlg pk m = do
    blinder <- generateBlinder (private_n pk)
    return (sign (Just blinder) hashAlg pk m)

-- | verify message with the signed message
verify :: HashAlgorithmASN1 hashAlg
       => Maybe hashAlg
       -> PublicKey
       -> ByteString
       -> ByteString
       -> Bool
verify hashAlg pk m sm =
    case makeSignature hashAlg (public_size pk) m of
        Left _  -> False
        Right s -> s == (ep pk sm)

-- | make signature digest, used in 'sign' and 'verify'
makeSignature :: HashAlgorithmASN1 hashAlg
              => Maybe hashAlg -- ^ optional hashing algorithm
              -> Int
              -> ByteString
              -> Either Error ByteString
makeSignature Nothing        klen m = padSignature klen m
makeSignature (Just hashAlg) klen m = padSignature klen (hashDigestASN1 $ hashWith hashAlg m)
