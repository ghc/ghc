-- |
-- Module      : Crypto.Cipher.AESGCMSIV
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Implementation of AES-GCM-SIV, an AEAD scheme with nonce misuse resistance
-- defined in <https://tools.ietf.org/html/rfc8452 RFC 8452>.
--
-- To achieve the nonce misuse-resistance property, encryption requires two
-- passes on the plaintext, hence no streaming API is provided.  This AEAD
-- operates on complete inputs held in memory.  For simplicity, the
-- implementation of decryption uses a similar pattern, with performance
-- penalty compared to an implementation which is able to merge both passes.
--
-- The specification allows inputs up to 2^36 bytes but this implementation
-- requires AAD and plaintext/ciphertext to be both smaller than 2^32 bytes.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.Cipher.AESGCMSIV
    ( Nonce
    , nonce
    , generateNonce
    , encrypt
    , decrypt
    ) where

import Data.Bits
import Data.Word

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, poke, pokeElemOff)

import           Data.ByteArray
import qualified Data.ByteArray as B
import           Data.Memory.Endian (toLE)
import           Data.Memory.PtrMethods (memXor)

import Crypto.Cipher.AES.Primitive
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Internal.Compat (unsafeDoIO)
import Crypto.Random


-- 12-byte nonces

-- | Nonce value for AES-GCM-SIV, always 12 bytes.
newtype Nonce = Nonce Bytes deriving (Show, Eq, ByteArrayAccess)

-- | Nonce smart constructor.  Accepts only 12-byte inputs.
nonce :: ByteArrayAccess iv => iv -> CryptoFailable Nonce
nonce iv
    | B.length iv == 12 = CryptoPassed (Nonce $ B.convert iv)
    | otherwise         = CryptoFailed CryptoError_IvSizeInvalid

-- | Generate a random nonce for use with AES-GCM-SIV.
generateNonce :: MonadRandom m => m Nonce
generateNonce = Nonce <$> getRandomBytes 12


-- POLYVAL (mutable context)

newtype Polyval = Polyval Bytes

polyvalInit :: ScrubbedBytes -> IO Polyval
polyvalInit h = Polyval <$> doInit
  where doInit = B.alloc 272 $ \pctx -> B.withByteArray h $ \ph ->
            c_aes_polyval_init pctx ph

polyvalUpdate :: ByteArrayAccess ba => Polyval -> ba -> IO ()
polyvalUpdate (Polyval ctx) bs = B.withByteArray ctx $ \pctx ->
    B.withByteArray bs $ \pbs -> c_aes_polyval_update pctx pbs sz
  where sz = fromIntegral (B.length bs)

polyvalFinalize :: Polyval -> IO ScrubbedBytes
polyvalFinalize (Polyval ctx) = B.alloc 16 $ \dst ->
    B.withByteArray ctx $ \pctx -> c_aes_polyval_finalize pctx dst

foreign import ccall unsafe "cryptonite_aes.h cryptonite_aes_polyval_init"
    c_aes_polyval_init :: Ptr Polyval -> CString -> IO ()

foreign import ccall "cryptonite_aes.h cryptonite_aes_polyval_update"
    c_aes_polyval_update :: Ptr Polyval -> CString -> CUInt -> IO ()

foreign import ccall unsafe "cryptonite_aes.h cryptonite_aes_polyval_finalize"
    c_aes_polyval_finalize :: Ptr Polyval -> CString -> IO ()


-- Key Generation

le32iv :: Word32 -> Nonce -> Bytes
le32iv n (Nonce iv) = B.allocAndFreeze 16 $ \ptr -> do
    poke ptr (toLE n)
    copyByteArrayToPtr iv (ptr `plusPtr` 4)

deriveKeys :: BlockCipher128 aes => aes -> Nonce -> (ScrubbedBytes, AES)
deriveKeys aes iv =
    case cipherKeySize aes of
        KeySizeFixed sz | sz `mod` 8 == 0 ->
            let mak = buildKey [0 .. 1]
                key = buildKey [2 .. fromIntegral (sz `div` 8) + 1]
                mek = throwCryptoError (cipherInit key)
             in (mak, mek)
        _ -> error "AESGCMSIV: invalid cipher"
  where
    idx n = ecbEncrypt aes (le32iv n iv) `takeView` 8
    buildKey = B.concat . map idx


-- Encryption and decryption

lengthInvalid :: ByteArrayAccess ba => ba -> Bool
lengthInvalid bs
    | finiteBitSize len > 32 = len >= 1 `unsafeShiftL` 32
    | otherwise              = False
  where len = B.length bs

-- | AEAD encryption with the specified key and nonce.  The key must be given
-- as an initialized 'Crypto.Cipher.AES.AES128' or 'Crypto.Cipher.AES.AES256'
-- cipher.
--
-- Lengths of additional data and plaintext must be less than 2^32 bytes,
-- otherwise an exception is thrown.
encrypt :: (BlockCipher128 aes, ByteArrayAccess aad, ByteArray ba)
        => aes -> Nonce -> aad -> ba -> (AuthTag, ba)
encrypt aes iv aad plaintext
    | lengthInvalid aad = error "AESGCMSIV: aad is too large"
    | lengthInvalid plaintext = error "AESGCMSIV: plaintext is too large"
    | otherwise = (AuthTag tag, ciphertext)
  where
    (mak, mek) = deriveKeys aes iv
    ss = getSs mak aad plaintext
    tag = buildTag mek ss iv
    ciphertext = combineC32 mek (transformTag tag) plaintext

-- | AEAD decryption with the specified key and nonce.  The key must be given
-- as an initialized 'Crypto.Cipher.AES.AES128' or 'Crypto.Cipher.AES.AES256'
-- cipher.
--
-- Lengths of additional data and ciphertext must be less than 2^32 bytes,
-- otherwise an exception is thrown.
decrypt :: (BlockCipher128 aes, ByteArrayAccess aad, ByteArray ba)
        => aes -> Nonce -> aad -> ba -> AuthTag -> Maybe ba
decrypt aes iv aad ciphertext (AuthTag tag)
    | lengthInvalid aad = error "AESGCMSIV: aad is too large"
    | lengthInvalid ciphertext = error "AESGCMSIV: ciphertext is too large"
    | tag `constEq` buildTag mek ss iv = Just plaintext
    | otherwise = Nothing
  where
    (mak, mek) = deriveKeys aes iv
    ss = getSs mak aad plaintext
    plaintext = combineC32 mek (transformTag tag) ciphertext

-- Calculate S_s = POLYVAL(mak, X_1, X_2, ...).
getSs :: (ByteArrayAccess aad, ByteArrayAccess ba)
      => ScrubbedBytes -> aad -> ba -> ScrubbedBytes
getSs mak aad plaintext = unsafeDoIO $ do
    ctx <- polyvalInit mak
    polyvalUpdate ctx aad
    polyvalUpdate ctx plaintext
    polyvalUpdate ctx (lb :: Bytes)  -- the "length block"
    polyvalFinalize ctx
  where
    lb = B.allocAndFreeze 16 $ \ptr -> do
            pokeElemOff ptr 0 (toLE64 $ B.length aad)
            pokeElemOff ptr 1 (toLE64 $ B.length plaintext)
    toLE64 x = toLE (fromIntegral x * 8 :: Word64)

-- XOR the first 12 bytes of S_s with the nonce and clear the most significant
-- bit of the last byte.
tagInput :: ScrubbedBytes -> Nonce -> Bytes
tagInput ss (Nonce iv) =
    B.copyAndFreeze ss $ \ptr ->
    B.withByteArray iv $ \ivPtr -> do
        memXor ptr ptr ivPtr 12
        b <- peekElemOff ptr 15
        pokeElemOff ptr 15 (b .&. (0x7f :: Word8))

-- Encrypt the result with AES using the message-encryption key to produce the
-- tag.
buildTag :: BlockCipher128 aes => aes -> ScrubbedBytes -> Nonce -> Bytes
buildTag mek ss iv = ecbEncrypt mek (tagInput ss iv)

-- The initial counter block is the tag with the most significant bit of the
-- last byte set to one.
transformTag :: Bytes -> IV AES
transformTag tag = toIV $ B.copyAndFreeze tag $ \ptr ->
    peekElemOff ptr 15 >>= pokeElemOff ptr 15 . (.|. (0x80 :: Word8))
  where toIV bs = let Just iv = makeIV (bs :: Bytes) in iv
