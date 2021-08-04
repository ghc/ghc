-- |
-- Module      : Crypto.Cipher.Salsa
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.Cipher.Salsa
    ( initialize
    , combine
    , generate
    , State(..)
    ) where

import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, ScrubbedBytes)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Foreign.Ptr
import           Foreign.C.Types

-- | Salsa context
newtype State = State ScrubbedBytes
    deriving (NFData)

-- | Initialize a new Salsa context with the number of rounds,
-- the key and the nonce associated.
initialize :: (ByteArrayAccess key, ByteArrayAccess nonce)
           => Int    -- ^ number of rounds (8,12,20)
           -> key    -- ^ the key (128 or 256 bits)
           -> nonce  -- ^ the nonce (64 or 96 bits)
           -> State  -- ^ the initial Salsa state
initialize nbRounds key nonce
    | kLen `notElem` [16,32]          = error "Salsa: key length should be 128 or 256 bits"
    | nonceLen `notElem` [8,12]       = error "Salsa: nonce length should be 64 or 96 bits"
    | nbRounds `notElem` [8,12,20]    = error "Salsa: rounds should be 8, 12 or 20"
    | otherwise = unsafeDoIO $ do
        stPtr <- B.alloc 132 $ \stPtr ->
            B.withByteArray nonce $ \noncePtr  ->
            B.withByteArray key   $ \keyPtr ->
                ccryptonite_salsa_init stPtr nbRounds kLen keyPtr nonceLen noncePtr
        return $ State stPtr
  where kLen     = B.length key
        nonceLen = B.length nonce

-- | Combine the salsa output and an arbitrary message with a xor,
-- and return the combined output and the new state.
combine :: ByteArray ba
        => State      -- ^ the current Salsa state
        -> ba         -- ^ the source to xor with the generator
        -> (ba, State)
combine prevSt@(State prevStMem) src
    | B.null src = (B.empty, prevSt)
    | otherwise  = unsafeDoIO $ do
        (out, st) <- B.copyRet prevStMem $ \ctx ->
            B.alloc (B.length src) $ \dstPtr ->
            B.withByteArray src    $ \srcPtr -> do
                ccryptonite_salsa_combine dstPtr ctx srcPtr (fromIntegral $ B.length src)
        return (out, State st)

-- | Generate a number of bytes from the Salsa output directly
generate :: ByteArray ba
         => State -- ^ the current Salsa state
         -> Int   -- ^ the length of data to generate
         -> (ba, State)
generate prevSt@(State prevStMem) len
    | len <= 0  = (B.empty, prevSt)
    | otherwise = unsafeDoIO $ do
        (out, st) <- B.copyRet prevStMem $ \ctx ->
            B.alloc len $ \dstPtr ->
                ccryptonite_salsa_generate dstPtr ctx (fromIntegral len)
        return (out, State st)

foreign import ccall "cryptonite_salsa_init"
    ccryptonite_salsa_init :: Ptr State -> Int -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> IO ()

foreign import ccall "cryptonite_salsa_combine"
    ccryptonite_salsa_combine :: Ptr Word8 -> Ptr State -> Ptr Word8 -> CUInt -> IO ()

foreign import ccall "cryptonite_salsa_generate"
    ccryptonite_salsa_generate :: Ptr Word8 -> Ptr State -> CUInt -> IO ()
