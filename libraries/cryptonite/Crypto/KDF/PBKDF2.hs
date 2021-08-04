-- |
-- Module      : Crypto.KDF.PBKDF2
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Password Based Key Derivation Function 2
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Crypto.KDF.PBKDF2
    ( PRF
    , prfHMAC
    , Parameters(..)
    , generate
    , fastPBKDF2_SHA1
    , fastPBKDF2_SHA256
    , fastPBKDF2_SHA512
    ) where

import           Data.Word
import           Data.Bits
import           Foreign.Marshal.Alloc
import           Foreign.Ptr (plusPtr, Ptr)
import           Foreign.C.Types (CUInt(..), CSize(..))

import           Crypto.Hash (HashAlgorithm)
import qualified Crypto.MAC.HMAC as HMAC

import           Crypto.Internal.ByteArray (ByteArray, ByteArrayAccess, Bytes)
import qualified Crypto.Internal.ByteArray as B
import           Data.Memory.PtrMethods

-- | The PRF used for PBKDF2
type PRF password =
       password -- ^ the password parameters
    -> Bytes    -- ^ the content
    -> Bytes    -- ^ prf(password,content)

-- | PRF for PBKDF2 using HMAC with the hash algorithm as parameter
prfHMAC :: (HashAlgorithm a, ByteArrayAccess password)
        => a
        -> PRF password
prfHMAC alg k = hmacIncr alg (HMAC.initialize k)
  where hmacIncr :: HashAlgorithm a => a -> HMAC.Context a -> (Bytes -> Bytes)
        hmacIncr _ !ctx = \b -> B.convert $ HMAC.finalize $ HMAC.update ctx b

-- | Parameters for PBKDF2
data Parameters = Parameters
    { iterCounts   :: Int -- ^ the number of user-defined iterations for the algorithms. e.g. WPA2 uses 4000.
    , outputLength :: Int -- ^ the number of bytes to generate out of PBKDF2
    }

-- | generate the pbkdf2 key derivation function from the output
generate :: (ByteArrayAccess password, ByteArrayAccess salt, ByteArray ba)
         => PRF password
         -> Parameters
         -> password
         -> salt
         -> ba
generate prf params password salt =
    B.allocAndFreeze (outputLength params) $ \p -> do
        memSet p 0 (outputLength params)
        loop 1 (outputLength params) p
  where
    !runPRF = prf password
    !hLen   = B.length $ runPRF B.empty

    -- run the following f function on each complete chunk.
    -- when having an incomplete chunk, we call partial.
    -- partial need to be the last call.
    --
    -- f(pass,salt,c,i) = U1 xor U2 xor .. xor Uc
    -- U1 = PRF(pass,salt || BE32(i))
    -- Uc = PRF(pass,Uc-1)
    loop iterNb len p
        | len == 0   = return ()
        | len < hLen = partial iterNb len p
        | otherwise  = do
            let applyMany 0 _     = return ()
                applyMany i uprev = do
                    let uData = runPRF uprev
                    B.withByteArray uData $ \u -> memXor p p u hLen
                    applyMany (i-1) uData
            applyMany (iterCounts params) (B.convert salt `B.append` toBS iterNb)
            loop (iterNb+1) (len - hLen) (p `plusPtr` hLen)

    partial iterNb len p = allocaBytesAligned hLen 8 $ \tmp -> do
        let applyMany :: Int -> Bytes -> IO ()
            applyMany 0 _     = return ()
            applyMany i uprev = do
                let uData = runPRF uprev
                B.withByteArray uData $ \u -> memXor tmp tmp u hLen
                applyMany (i-1) uData
        memSet tmp 0 hLen
        applyMany (iterCounts params) (B.convert salt `B.append` toBS iterNb)
        memCopy p tmp len

    -- big endian encoding of Word32
    toBS :: ByteArray ba => Word32 -> ba
    toBS w = B.pack [a,b,c,d]
      where a = fromIntegral (w `shiftR` 24)
            b = fromIntegral ((w `shiftR` 16) .&. 0xff)
            c = fromIntegral ((w `shiftR` 8) .&. 0xff)
            d = fromIntegral (w .&. 0xff)
{-# NOINLINE generate #-}

fastPBKDF2_SHA1 :: (ByteArrayAccess password, ByteArrayAccess salt, ByteArray out)
                => Parameters
                -> password
                -> salt
                -> out
fastPBKDF2_SHA1 params password salt =
    B.allocAndFreeze (outputLength params) $ \outPtr ->
    B.withByteArray password $ \passPtr ->
    B.withByteArray salt $ \saltPtr ->
        c_cryptonite_fastpbkdf2_hmac_sha1
            passPtr (fromIntegral $ B.length password)
            saltPtr (fromIntegral $ B.length salt)
            (fromIntegral $ iterCounts params)
            outPtr (fromIntegral $ outputLength params)

fastPBKDF2_SHA256 :: (ByteArrayAccess password, ByteArrayAccess salt, ByteArray out)
                  => Parameters
                  -> password
                  -> salt
                  -> out
fastPBKDF2_SHA256 params password salt =
    B.allocAndFreeze (outputLength params) $ \outPtr ->
    B.withByteArray password $ \passPtr ->
    B.withByteArray salt $ \saltPtr ->
        c_cryptonite_fastpbkdf2_hmac_sha256
            passPtr (fromIntegral $ B.length password)
            saltPtr (fromIntegral $ B.length salt)
            (fromIntegral $ iterCounts params)
            outPtr (fromIntegral $ outputLength params)

fastPBKDF2_SHA512 :: (ByteArrayAccess password, ByteArrayAccess salt, ByteArray out)
                  => Parameters
                  -> password
                  -> salt
                  -> out
fastPBKDF2_SHA512 params password salt =
    B.allocAndFreeze (outputLength params) $ \outPtr ->
    B.withByteArray password $ \passPtr ->
    B.withByteArray salt $ \saltPtr ->
        c_cryptonite_fastpbkdf2_hmac_sha512
            passPtr (fromIntegral $ B.length password)
            saltPtr (fromIntegral $ B.length salt)
            (fromIntegral $ iterCounts params)
            outPtr (fromIntegral $ outputLength params)


foreign import ccall unsafe "cryptonite_pbkdf2.h cryptonite_fastpbkdf2_hmac_sha1"
    c_cryptonite_fastpbkdf2_hmac_sha1 :: Ptr Word8 -> CSize
                                      -> Ptr Word8 -> CSize
                                      -> CUInt
                                      -> Ptr Word8 -> CSize
                                      -> IO ()

foreign import ccall unsafe "cryptonite_pbkdf2.h cryptonite_fastpbkdf2_hmac_sha256"
    c_cryptonite_fastpbkdf2_hmac_sha256 :: Ptr Word8 -> CSize
                                        -> Ptr Word8 -> CSize
                                        -> CUInt
                                        -> Ptr Word8 -> CSize
                                        -> IO ()

foreign import ccall unsafe "cryptonite_pbkdf2.h cryptonite_fastpbkdf2_hmac_sha512"
    c_cryptonite_fastpbkdf2_hmac_sha512 :: Ptr Word8 -> CSize
                                        -> Ptr Word8 -> CSize
                                        -> CUInt
                                        -> Ptr Word8 -> CSize
                                        -> IO ()
