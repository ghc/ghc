-- |
-- Module      : Crypto.Cipher.ChaChaPoly1305
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
-- A simple AEAD scheme using ChaCha20 and Poly1305. See
-- <https://tools.ietf.org/html/rfc7539 RFC 7539>.
--
-- The State is not modified in place, so each function changing the State,
-- returns a new State.
--
-- Authenticated Data need to be added before any call to 'encrypt' or 'decrypt',
-- and once all the data has been added, then 'finalizeAAD' need to be called.
--
-- Once 'finalizeAAD' has been called, no further 'appendAAD' call should be make.
--
-- >import Data.ByteString.Char8 as B
-- >import Data.ByteArray
-- >import Crypto.Error
-- >import Crypto.Cipher.ChaChaPoly1305 as C
-- >
-- >encrypt
-- >    :: ByteString -- nonce (12 random bytes)
-- >    -> ByteString -- symmetric key
-- >    -> ByteString -- optional associated data (won't be encrypted)
-- >    -> ByteString -- input plaintext to be encrypted
-- >    -> CryptoFailable ByteString -- ciphertext with a 128-bit tag attached
-- >encrypt nonce key header plaintext = do
-- >    st1 <- C.nonce12 nonce >>= C.initialize key
-- >    let
-- >        st2 = C.finalizeAAD $ C.appendAAD header st1
-- >        (out, st3) = C.encrypt plaintext st2
-- >        auth = C.finalize st3
-- >    return $ out `B.append` Data.ByteArray.convert auth
--
module Crypto.Cipher.ChaChaPoly1305
    ( State
    , Nonce
    , nonce12
    , nonce8
    , incrementNonce
    , initialize
    , appendAAD
    , finalizeAAD
    , encrypt
    , decrypt
    , finalize
    ) where

import           Control.Monad             (when)
import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, Bytes, ScrubbedBytes)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.Imports
import           Crypto.Error
import qualified Crypto.Cipher.ChaCha as ChaCha
import qualified Crypto.MAC.Poly1305  as Poly1305
import           Data.Memory.Endian
import qualified Data.ByteArray.Pack as P
import           Foreign.Ptr
import           Foreign.Storable

-- | A ChaChaPoly1305 State.
--
-- The state is immutable, and only new state can be created
data State = State !ChaCha.State
                   !Poly1305.State
                   !Word64 -- AAD length
                   !Word64 -- ciphertext length

-- | Valid Nonce for ChaChaPoly1305.
--
-- It can be created with 'nonce8' or 'nonce12'
data Nonce = Nonce8 Bytes | Nonce12 Bytes

instance ByteArrayAccess Nonce where
  length (Nonce8  n) = B.length n
  length (Nonce12 n) = B.length n

  withByteArray (Nonce8  n) = B.withByteArray n
  withByteArray (Nonce12 n) = B.withByteArray n

-- Based on the following pseudo code:
--
-- chacha20_aead_encrypt(aad, key, iv, constant, plaintext):
--     nonce = constant | iv
--     otk = poly1305_key_gen(key, nonce)
--     ciphertext = chacha20_encrypt(key, 1, nonce, plaintext)
--     mac_data = aad | pad16(aad)
--     mac_data |= ciphertext | pad16(ciphertext)
--     mac_data |= num_to_4_le_bytes(aad.length)
--     mac_data |= num_to_4_le_bytes(ciphertext.length)
--     tag = poly1305_mac(mac_data, otk)
--     return (ciphertext, tag)

pad16 :: Word64 -> Bytes
pad16 n
    | modLen == 0 = B.empty
    | otherwise   = B.replicate (16 - modLen) 0
  where
    modLen = fromIntegral (n `mod` 16)

-- | Nonce smart constructor 12 bytes IV, nonce constructor
nonce12 :: ByteArrayAccess iv => iv -> CryptoFailable Nonce
nonce12 iv
    | B.length iv /= 12 = CryptoFailed CryptoError_IvSizeInvalid
    | otherwise         = CryptoPassed . Nonce12 . B.convert $ iv

-- | 8 bytes IV, nonce constructor
nonce8 :: ByteArrayAccess ba
       => ba -- ^ 4 bytes constant
       -> ba -- ^ 8 bytes IV
       -> CryptoFailable Nonce
nonce8 constant iv
    | B.length constant /= 4 = CryptoFailed CryptoError_IvSizeInvalid
    | B.length iv       /= 8 = CryptoFailed CryptoError_IvSizeInvalid
    | otherwise              = CryptoPassed . Nonce8 . B.concat $ [constant, iv]

-- | Increment a nonce
incrementNonce :: Nonce -> Nonce
incrementNonce (Nonce8  n) = Nonce8  $ incrementNonce' n 4
incrementNonce (Nonce12 n) = Nonce12 $ incrementNonce' n 0

incrementNonce' :: Bytes -> Int -> Bytes
incrementNonce' b offset = B.copyAndFreeze b $ \s ->
    loop s (s `plusPtr` offset)
    where
      loop :: Ptr Word8 -> Ptr Word8 -> IO ()
      loop s p
          | s == (p `plusPtr` (B.length b - offset - 1)) = peek s >>= poke s . (+) 1
          | otherwise = do
              r <- (+) 1 <$> peek p
              poke p r
              when (r == 0) $ loop s (p `plusPtr` 1)

-- | Initialize a new ChaChaPoly1305 State
--
-- The key length need to be 256 bits, and the nonce
-- procured using either `nonce8` or `nonce12`
initialize :: ByteArrayAccess key
           => key -> Nonce -> CryptoFailable State
initialize key (Nonce8  nonce) = initialize' key nonce
initialize key (Nonce12 nonce) = initialize' key nonce

initialize' :: ByteArrayAccess key
            => key -> Bytes -> CryptoFailable State
initialize' key nonce
    | B.length key /= 32 = CryptoFailed CryptoError_KeySizeInvalid
    | otherwise          = CryptoPassed $ State encState polyState 0 0
  where
    rootState           = ChaCha.initialize 20 key nonce
    (polyKey, encState) = ChaCha.generate rootState 64
    polyState           = throwCryptoError $ Poly1305.initialize (B.take 32 polyKey :: ScrubbedBytes)

-- | Append Authenticated Data to the State and return
-- the new modified State.
--
-- Once no further call to this function need to be make,
-- the user should call 'finalizeAAD'
appendAAD :: ByteArrayAccess ba => ba -> State -> State
appendAAD ba (State encState macState aadLength plainLength) =
    State encState newMacState newLength plainLength
  where
    newMacState = Poly1305.update macState ba
    newLength   = aadLength + fromIntegral (B.length ba)

-- | Finalize the Authenticated Data and return the finalized State
finalizeAAD :: State -> State
finalizeAAD (State encState macState aadLength plainLength) =
    State encState newMacState aadLength plainLength
  where
    newMacState = Poly1305.update macState $ pad16 aadLength

-- | Encrypt a piece of data and returns the encrypted Data and the
-- updated State.
encrypt :: ByteArray ba => ba -> State -> (ba, State)
encrypt input (State encState macState aadLength plainLength) =
    (output, State newEncState newMacState aadLength newPlainLength)
  where
    (output, newEncState) = ChaCha.combine encState input
    newMacState           = Poly1305.update macState output
    newPlainLength        = plainLength + fromIntegral (B.length input)

-- | Decrypt a piece of data and returns the decrypted Data and the
-- updated State.
decrypt :: ByteArray ba => ba -> State -> (ba, State)
decrypt input (State encState macState aadLength plainLength) =
    (output, State newEncState newMacState aadLength newPlainLength)
  where
    (output, newEncState) = ChaCha.combine encState input
    newMacState           = Poly1305.update macState input
    newPlainLength        = plainLength + fromIntegral (B.length input)

-- | Generate an authentication tag from the State.
finalize :: State -> Poly1305.Auth
finalize (State _ macState aadLength plainLength) =
    Poly1305.finalize $ Poly1305.updates macState
        [ pad16 plainLength
        , either (error "finalize: internal error") id $ P.fill 16 (P.putStorable (toLE aadLength) >> P.putStorable (toLE plainLength))
        ]
