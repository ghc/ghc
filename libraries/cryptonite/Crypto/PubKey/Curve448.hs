-- |
-- Module      : Crypto.PubKey.Curve448
-- License     : BSD-style
-- Maintainer  : John Galt <jgalt@centromere.net>
-- Stability   : experimental
-- Portability : unknown
--
-- Curve448 support
--
-- Internally uses Decaf point compression to omit the cofactor
-- and implementation by Mike Hamburg.  Externally API and
-- data types are compatible with the encoding specified in RFC 7748.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.PubKey.Curve448
    ( SecretKey
    , PublicKey
    , DhSecret
    -- * Smart constructors
    , dhSecret
    , publicKey
    , secretKey
    -- * Methods
    , dh
    , toPublic
    , generateSecretKey
    ) where

import           Data.Word
import           Foreign.Ptr

import           Crypto.Error
import           Crypto.Random
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Internal.ByteArray (ByteArrayAccess, ScrubbedBytes, Bytes, withByteArray)
import qualified Crypto.Internal.ByteArray as B

-- | A Curve448 Secret key
newtype SecretKey = SecretKey ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | A Curve448 public key
newtype PublicKey = PublicKey Bytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | A Curve448 Diffie Hellman secret related to a
-- public key and a secret key.
newtype DhSecret = DhSecret ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | Try to build a public key from a bytearray
publicKey :: ByteArrayAccess bs => bs -> CryptoFailable PublicKey
publicKey bs
    | B.length bs == x448_bytes = CryptoPassed $ PublicKey $ B.copyAndFreeze bs (\_ -> return ())
    | otherwise                 = CryptoFailed CryptoError_PublicKeySizeInvalid

-- | Try to build a secret key from a bytearray
secretKey :: ByteArrayAccess bs => bs -> CryptoFailable SecretKey
secretKey bs
    | B.length bs == x448_bytes = unsafeDoIO $
        withByteArray bs $ \inp -> do
            valid <- isValidPtr inp
            if valid
                then (CryptoPassed . SecretKey) <$> B.copy bs (\_ -> return ())
                else return $ CryptoFailed CryptoError_SecretKeyStructureInvalid
    | otherwise = CryptoFailed CryptoError_SecretKeySizeInvalid
  where
        isValidPtr :: Ptr Word8 -> IO Bool
        isValidPtr _ =
            return True
{-# NOINLINE secretKey #-}

-- | Create a DhSecret from a bytearray object
dhSecret :: ByteArrayAccess b => b -> CryptoFailable DhSecret
dhSecret bs
    | B.length bs == x448_bytes = CryptoPassed $ DhSecret $ B.copyAndFreeze bs (\_ -> return ())
    | otherwise                 = CryptoFailed CryptoError_SharedSecretSizeInvalid

-- | Compute the Diffie Hellman secret from a public key and a secret key.
--
-- This implementation may return an all-zero value as it does not check for
-- the condition.
dh :: PublicKey -> SecretKey -> DhSecret
dh (PublicKey pub) (SecretKey sec) = DhSecret <$>
    B.allocAndFreeze x448_bytes $ \result ->
    withByteArray sec           $ \psec   ->
    withByteArray pub           $ \ppub   ->
        decaf_x448 result ppub psec
{-# NOINLINE dh #-}

-- | Create a public key from a secret key
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey sec) = PublicKey <$>
    B.allocAndFreeze x448_bytes     $ \result ->
    withByteArray sec               $ \psec   ->
        decaf_x448_derive_public_key result psec
{-# NOINLINE toPublic #-}

-- | Generate a secret key.
generateSecretKey :: MonadRandom m => m SecretKey
generateSecretKey = SecretKey <$> getRandomBytes x448_bytes

x448_bytes :: Int
x448_bytes = 448 `quot` 8

foreign import ccall "cryptonite_decaf_x448"
    decaf_x448 :: Ptr Word8 -- ^ public
               -> Ptr Word8 -- ^ basepoint
               -> Ptr Word8 -- ^ secret
               -> IO ()

foreign import ccall "cryptonite_decaf_x448_derive_public_key"
    decaf_x448_derive_public_key :: Ptr Word8 -- ^ public
                                 -> Ptr Word8 -- ^ secret
                                 -> IO ()
