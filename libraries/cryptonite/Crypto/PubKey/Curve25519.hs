-- |
-- Module      : Crypto.PubKey.Curve25519
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Curve25519 support
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.PubKey.Curve25519
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

import           Data.Bits
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Ptr

import           Crypto.Error
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Internal.ByteArray (ByteArrayAccess, ScrubbedBytes, Bytes, withByteArray)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Random

-- | A Curve25519 Secret key
newtype SecretKey = SecretKey ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | A Curve25519 public key
newtype PublicKey = PublicKey Bytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | A Curve25519 Diffie Hellman secret related to a
-- public key and a secret key.
newtype DhSecret = DhSecret ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | Try to build a public key from a bytearray
publicKey :: ByteArrayAccess bs => bs -> CryptoFailable PublicKey
publicKey bs
    | B.length bs == 32 = CryptoPassed $ PublicKey $ B.copyAndFreeze bs (\_ -> return ())
    | otherwise         = CryptoFailed CryptoError_PublicKeySizeInvalid

-- | Try to build a secret key from a bytearray
secretKey :: ByteArrayAccess bs => bs -> CryptoFailable SecretKey
secretKey bs
    | B.length bs == 32 = unsafeDoIO $ do
        withByteArray bs $ \inp -> do
            valid <- isValidPtr inp
            if valid
                then (CryptoPassed . SecretKey) <$> B.copy bs (\_ -> return ())
                else return $ CryptoFailed CryptoError_SecretKeyStructureInvalid
    | otherwise = CryptoFailed CryptoError_SecretKeySizeInvalid
  where
        --  e[0] &= 0xf8;
        --  e[31] &= 0x7f;
        --  e[31] |= 40;
        isValidPtr :: Ptr Word8 -> IO Bool
        isValidPtr _ = do
            --b0  <- peekElemOff inp 0
            --b31 <- peekElemOff inp 31
            return True
{-
            return $ and [ testBit b0  0 == False
                         , testBit b0  1 == False
                         , testBit b0  2 == False
                         , testBit b31 7 == False
                         , testBit b31 6 == True
                         ]
-}
{-# NOINLINE secretKey #-}

-- | Create a DhSecret from a bytearray object
dhSecret :: ByteArrayAccess b => b -> CryptoFailable DhSecret
dhSecret bs
    | B.length bs == 32 = CryptoPassed $ DhSecret $ B.copyAndFreeze bs (\_ -> return ())
    | otherwise         = CryptoFailed CryptoError_SharedSecretSizeInvalid

-- | Compute the Diffie Hellman secret from a public key and a secret key.
--
-- This implementation may return an all-zero value as it does not check for
-- the condition.
dh :: PublicKey -> SecretKey -> DhSecret
dh (PublicKey pub) (SecretKey sec) = DhSecret <$>
    B.allocAndFreeze 32        $ \result ->
    withByteArray sec          $ \psec   ->
    withByteArray pub          $ \ppub   ->
        ccryptonite_curve25519 result psec ppub
{-# NOINLINE dh #-}

-- | Create a public key from a secret key
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey sec) = PublicKey <$>
    B.allocAndFreeze 32     $ \result ->
    withByteArray sec       $ \psec   ->
        ccryptonite_curve25519 result psec basePoint
  where
        basePoint = Ptr "\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#
{-# NOINLINE toPublic #-}

-- | Generate a secret key.
generateSecretKey :: MonadRandom m => m SecretKey
generateSecretKey = tweakToSecretKey <$> getRandomBytes 32
  where
    tweakToSecretKey :: ScrubbedBytes -> SecretKey
    tweakToSecretKey bin = SecretKey $ B.copyAndFreeze bin $ \inp -> do
        modifyByte inp 0 (\e0 -> e0 .&. 0xf8)
        modifyByte inp 31 (\e31 -> (e31 .&. 0x7f) .|. 0x40)

    modifyByte :: Ptr Word8 -> Int -> (Word8 -> Word8) -> IO ()
    modifyByte p n f = peekByteOff p n >>= pokeByteOff p n . f

foreign import ccall "cryptonite_curve25519_donna"
    ccryptonite_curve25519 :: Ptr Word8 -- ^ public
                           -> Ptr Word8 -- ^ secret
                           -> Ptr Word8 -- ^ basepoint
                           -> IO ()
