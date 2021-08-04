-- |
-- Module      : Crypto.PubKey.Ed448
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Ed448 support
--
-- Internally uses Decaf point compression to omit the cofactor
-- and implementation by Mike Hamburg.  Externally API and
-- data types are compatible with the encoding specified in RFC 8032.
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.PubKey.Ed448
    ( SecretKey
    , PublicKey
    , Signature
    -- * Size constants
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
    , verify
    , generateSecretKey
    ) where

import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           Crypto.Error
import           Crypto.Internal.ByteArray (ByteArrayAccess, Bytes,
                                            ScrubbedBytes, withByteArray)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Random

-- | An Ed448 Secret key
newtype SecretKey = SecretKey ScrubbedBytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | An Ed448 public key
newtype PublicKey = PublicKey Bytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | An Ed448 signature
newtype Signature = Signature Bytes
    deriving (Show,Eq,ByteArrayAccess,NFData)

-- | Try to build a public key from a bytearray
publicKey :: ByteArrayAccess ba => ba -> CryptoFailable PublicKey
publicKey bs
    | B.length bs == publicKeySize =
        CryptoPassed $ PublicKey $ B.copyAndFreeze bs (\_ -> return ())
    | otherwise =
        CryptoFailed $ CryptoError_PublicKeySizeInvalid

-- | Try to build a secret key from a bytearray
secretKey :: ByteArrayAccess ba => ba -> CryptoFailable SecretKey
secretKey bs
    | B.length bs == secretKeySize = unsafeDoIO $ withByteArray bs initialize
    | otherwise                    = CryptoFailed CryptoError_SecretKeyStructureInvalid
  where
        initialize inp = do
            valid <- isValidPtr inp
            if valid
                then (CryptoPassed . SecretKey) <$> B.copy bs (\_ -> return ())
                else return $ CryptoFailed CryptoError_SecretKeyStructureInvalid
        isValidPtr _ =
            return True
{-# NOINLINE secretKey #-}

-- | Try to build a signature from a bytearray
signature :: ByteArrayAccess ba => ba -> CryptoFailable Signature
signature bs
    | B.length bs == signatureSize =
        CryptoPassed $ Signature $ B.copyAndFreeze bs (\_ -> return ())
    | otherwise =
        CryptoFailed CryptoError_SecretKeyStructureInvalid

-- | Create a public key from a secret key
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey sec) = PublicKey <$>
    B.allocAndFreeze publicKeySize $ \result ->
    withByteArray sec              $ \psec   ->
        decaf_ed448_derive_public_key result psec
{-# NOINLINE toPublic #-}

-- | Sign a message using the key pair
sign :: ByteArrayAccess ba => SecretKey -> PublicKey -> ba -> Signature
sign secret public message =
    Signature $ B.allocAndFreeze signatureSize $ \sig ->
        withByteArray secret  $ \sec ->
        withByteArray public  $ \pub ->
        withByteArray message $ \msg ->
             decaf_ed448_sign sig sec pub msg (fromIntegral msgLen) 0 no_context 0
  where
    !msgLen = B.length message

-- | Verify a message
verify :: ByteArrayAccess ba => PublicKey -> ba -> Signature -> Bool
verify public message signatureVal = unsafeDoIO $
    withByteArray signatureVal $ \sig ->
    withByteArray public       $ \pub ->
    withByteArray message      $ \msg -> do
      r <- decaf_ed448_verify sig pub msg (fromIntegral msgLen) 0 no_context 0
      return (r /= 0)
  where
    !msgLen = B.length message

-- | Generate a secret key
generateSecretKey :: MonadRandom m => m SecretKey
generateSecretKey = SecretKey <$> getRandomBytes secretKeySize

-- | A public key is 57 bytes
publicKeySize :: Int
publicKeySize = 57

-- | A secret key is 57 bytes
secretKeySize :: Int
secretKeySize = 57

-- | A signature is 114 bytes
signatureSize :: Int
signatureSize = 114

no_context :: Ptr Word8
no_context = nullPtr -- not supported yet

foreign import ccall "cryptonite_decaf_ed448_derive_public_key"
    decaf_ed448_derive_public_key :: Ptr PublicKey -- public key
                                  -> Ptr SecretKey -- secret key
                                  -> IO ()

foreign import ccall "cryptonite_decaf_ed448_sign"
    decaf_ed448_sign :: Ptr Signature -- signature
                     -> Ptr SecretKey -- secret
                     -> Ptr PublicKey -- public
                     -> Ptr Word8     -- message
                     -> CSize         -- message len
                     -> Word8         -- prehashed
                     -> Ptr Word8     -- context
                     -> Word8         -- context len
                     -> IO ()

foreign import ccall "cryptonite_decaf_ed448_verify"
    decaf_ed448_verify :: Ptr Signature -- signature
                       -> Ptr PublicKey -- public
                       -> Ptr Word8     -- message
                       -> CSize         -- message len
                       -> Word8         -- prehashed
                       -> Ptr Word8     -- context
                       -> Word8         -- context len
                       -> IO CInt
