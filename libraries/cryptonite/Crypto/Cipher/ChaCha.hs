-- |
-- Module      : Crypto.Cipher.ChaCha
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.Cipher.ChaCha
    ( initialize
    , combine
    , generate
    , State
    -- * Simple interface for DRG purpose
    , initializeSimple
    , generateSimple
    , StateSimple
    ) where

import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, ScrubbedBytes)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Foreign.Ptr
import           Foreign.C.Types

-- | ChaCha context
newtype State = State ScrubbedBytes
    deriving (NFData)

-- | ChaCha context for DRG purpose (see Crypto.Random.ChaChaDRG)
newtype StateSimple = StateSimple ScrubbedBytes -- just ChaCha's state
    deriving (NFData)

-- | Initialize a new ChaCha context with the number of rounds,
-- the key and the nonce associated.
initialize :: (ByteArrayAccess key, ByteArrayAccess nonce)
           => Int   -- ^ number of rounds (8,12,20)
           -> key   -- ^ the key (128 or 256 bits)
           -> nonce -- ^ the nonce (64 or 96 bits)
           -> State -- ^ the initial ChaCha state
initialize nbRounds key nonce
    | kLen `notElem` [16,32]          = error "ChaCha: key length should be 128 or 256 bits"
    | nonceLen `notElem` [8,12]       = error "ChaCha: nonce length should be 64 or 96 bits"
    | nbRounds `notElem` [8,12,20]    = error "ChaCha: rounds should be 8, 12 or 20"
    | otherwise                       = unsafeDoIO $ do
        stPtr <- B.alloc 132 $ \stPtr ->
            B.withByteArray nonce $ \noncePtr  ->
            B.withByteArray key   $ \keyPtr ->
                ccryptonite_chacha_init stPtr  nbRounds kLen keyPtr nonceLen noncePtr
        return $ State stPtr
  where kLen     = B.length key
        nonceLen = B.length nonce

-- | Initialize simple ChaCha State
--
-- The seed need to be at least 40 bytes long
initializeSimple :: ByteArrayAccess seed
                 => seed -- ^ a 40 bytes long seed
                 -> StateSimple
initializeSimple seed
    | sLen < 40 = error "ChaCha Random: seed length should be 40 bytes"
    | otherwise = unsafeDoIO $ do
        stPtr <- B.alloc 64 $ \stPtr ->
                    B.withByteArray seed $ \seedPtr ->
                        ccryptonite_chacha_init_core stPtr 32 seedPtr 8 (seedPtr `plusPtr` 32)
        return $ StateSimple stPtr
  where
    sLen = B.length seed

-- | Combine the chacha output and an arbitrary message with a xor,
-- and return the combined output and the new state.
combine :: ByteArray ba
        => State       -- ^ the current ChaCha state
        -> ba          -- ^ the source to xor with the generator
        -> (ba, State)
combine prevSt@(State prevStMem) src
    | B.null src = (B.empty, prevSt)
    | otherwise  = unsafeDoIO $ do
        (out, st) <- B.copyRet prevStMem $ \ctx ->
            B.alloc (B.length src) $ \dstPtr ->
            B.withByteArray src    $ \srcPtr ->
                ccryptonite_chacha_combine dstPtr ctx srcPtr (fromIntegral $ B.length src)
        return (out, State st)

-- | Generate a number of bytes from the ChaCha output directly
generate :: ByteArray ba
         => State -- ^ the current ChaCha state
         -> Int   -- ^ the length of data to generate
         -> (ba, State)
generate prevSt@(State prevStMem) len
    | len <= 0  = (B.empty, prevSt)
    | otherwise = unsafeDoIO $ do
        (out, st) <- B.copyRet prevStMem $ \ctx ->
            B.alloc len $ \dstPtr ->
                ccryptonite_chacha_generate dstPtr ctx (fromIntegral len)
        return (out, State st)

-- | similar to 'generate' but assume certains values
generateSimple :: ByteArray ba
               => StateSimple
               -> Int
               -> (ba, StateSimple)
generateSimple (StateSimple prevSt) nbBytes = unsafeDoIO $ do
    newSt  <- B.copy prevSt (\_ -> return ())
    output <- B.alloc nbBytes $ \dstPtr ->
        B.withByteArray newSt $ \stPtr ->
            ccryptonite_chacha_random 8 dstPtr stPtr (fromIntegral nbBytes)
    return (output, StateSimple newSt)

foreign import ccall "cryptonite_chacha_init_core"
    ccryptonite_chacha_init_core :: Ptr StateSimple -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> IO ()

foreign import ccall "cryptonite_chacha_init"
    ccryptonite_chacha_init :: Ptr State -> Int -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> IO ()

foreign import ccall "cryptonite_chacha_combine"
    ccryptonite_chacha_combine :: Ptr Word8 -> Ptr State -> Ptr Word8 -> CUInt -> IO ()

foreign import ccall "cryptonite_chacha_generate"
    ccryptonite_chacha_generate :: Ptr Word8 -> Ptr State -> CUInt -> IO ()

foreign import ccall "cryptonite_chacha_random"
    ccryptonite_chacha_random :: Int -> Ptr Word8 -> Ptr StateSimple -> CUInt -> IO ()

