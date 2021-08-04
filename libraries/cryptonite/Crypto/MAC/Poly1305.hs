
-- |
-- Module      : Crypto.MAC.Poly1305
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Poly1305 implementation
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.MAC.Poly1305
    ( Ctx
    , State
    , Auth(..)
    , authTag
    -- * Incremental MAC Functions
    , initialize -- :: State
    , update     -- :: State -> ByteString -> State
    , updates    -- :: State -> [ByteString] -> State
    , finalize   -- :: State -> Auth
    -- * One-pass MAC function
    , auth
    ) where

import           Foreign.Ptr
import           Foreign.C.Types
import           Data.Word
import           Crypto.Internal.ByteArray (ByteArrayAccess, ScrubbedBytes, Bytes)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.DeepSeq
import           Crypto.Error

-- | Poly1305 State
--
-- This type is an instance of 'ByteArrayAccess' for debugging purpose. Internal
-- layout is architecture dependent, may contain uninitialized data fragments,
-- and change in future versions.  The bytearray should not be used as input to
-- cryptographic algorithms.
newtype State = State ScrubbedBytes
    deriving (ByteArrayAccess)

-- | Poly1305 State. use State instead of Ctx
type Ctx = State
{-# DEPRECATED Ctx "use Poly1305 State instead" #-}

-- | Poly1305 Auth
newtype Auth = Auth Bytes
    deriving (ByteArrayAccess,NFData)

authTag :: ByteArrayAccess b => b -> CryptoFailable Auth
authTag b
    | B.length b /= 16 = CryptoFailed $ CryptoError_AuthenticationTagSizeInvalid
    | otherwise        = CryptoPassed $ Auth $ B.convert b

instance Eq Auth where
    (Auth a1) == (Auth a2) = B.constEq a1 a2

foreign import ccall unsafe "cryptonite_poly1305.h cryptonite_poly1305_init"
    c_poly1305_init :: Ptr State -> Ptr Word8 -> IO ()

foreign import ccall "cryptonite_poly1305.h cryptonite_poly1305_update"
    c_poly1305_update :: Ptr State -> Ptr Word8 -> CUInt -> IO ()

foreign import ccall unsafe "cryptonite_poly1305.h cryptonite_poly1305_finalize"
    c_poly1305_finalize :: Ptr Word8 -> Ptr State -> IO ()

-- | initialize a Poly1305 context
initialize :: ByteArrayAccess key
           => key
           -> CryptoFailable State
initialize key
    | B.length key /= 32 = CryptoFailed $ CryptoError_MacKeyInvalid
    | otherwise          = CryptoPassed $ State $ B.allocAndFreeze 84 $ \ctxPtr ->
        B.withByteArray key $ \keyPtr ->
            c_poly1305_init (castPtr ctxPtr) keyPtr
{-# NOINLINE initialize #-}

-- | update a context with a bytestring
update :: ByteArrayAccess ba => State -> ba -> State
update (State prevCtx) d = State $ B.copyAndFreeze prevCtx $ \ctxPtr ->
    B.withByteArray d $ \dataPtr ->
        c_poly1305_update (castPtr ctxPtr) dataPtr (fromIntegral $ B.length d)
{-# NOINLINE update #-}

-- | updates a context with multiples bytestring
updates :: ByteArrayAccess ba => State -> [ba] -> State
updates (State prevCtx) d = State $ B.copyAndFreeze prevCtx (loop d)
  where loop []     _      = return ()
        loop (x:xs) ctxPtr = do
            B.withByteArray x $ \dataPtr -> c_poly1305_update ctxPtr dataPtr (fromIntegral $ B.length x)
            loop xs ctxPtr
{-# NOINLINE updates #-}

-- | finalize the context into a digest bytestring
finalize :: State -> Auth
finalize (State prevCtx) = Auth $ B.allocAndFreeze 16 $ \dst -> do
    _ <- B.copy prevCtx (\ctxPtr -> c_poly1305_finalize dst (castPtr ctxPtr)) :: IO ScrubbedBytes
    return ()
{-# NOINLINE finalize #-}

-- | One-pass authorization creation
auth :: (ByteArrayAccess key, ByteArrayAccess ba) => key -> ba -> Auth
auth key d
    | B.length key /= 32 = error "Poly1305: key length expected 32 bytes"
    | otherwise          = Auth $ B.allocAndFreeze 16 $ \dst -> do
        _ <- B.alloc 84 (onCtx dst) :: IO ScrubbedBytes
        return ()
  where
        onCtx dst ctxPtr =
            B.withByteArray key $ \keyPtr -> do
                c_poly1305_init (castPtr ctxPtr) keyPtr
                B.withByteArray d $ \dataPtr ->
                    c_poly1305_update (castPtr ctxPtr) dataPtr (fromIntegral $ B.length d)
                c_poly1305_finalize dst (castPtr ctxPtr)
