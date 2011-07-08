{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , ForeignFunctionInterface
           , EmptyDataDecls
  #-}
-- ----------------------------------------------------------------------------
-- 
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- ----------------------------------------------------------------------------

module GHC.Fingerprint (
        Fingerprint(..), fingerprint0, 
        fingerprintData,
        fingerprintString,
        fingerprintFingerprints
   ) where

import GHC.IO
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Real
import Foreign
import Foreign.C
import GHC.IO.Encoding
import GHC.Foreign

import GHC.Fingerprint.Type

-- for SIZEOF_STRUCT_MD5CONTEXT:
#include "HsBaseConfig.h"

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

instance Storable Fingerprint where
  sizeOf _ = 16
  alignment _ = 8
  peek = peekFingerprint
  poke = pokeFingerprint

-- peek/poke in fixed BIG-endian 128-bit format
peekFingerprint :: Ptr Fingerprint -> IO Fingerprint
peekFingerprint p = do
      let peekW64 :: Ptr Word8 -> Int -> Word64 -> IO Word64
          peekW64 _  0  !i = return i
          peekW64 !p !n !i = do
                w8 <- peek p
                peekW64 (p `plusPtr` 1) (n-1) 
                    ((i `shiftL` 8) .|. fromIntegral w8)

      high <- peekW64 (castPtr p) 8 0
      low  <- peekW64 (castPtr p `plusPtr` 8) 8 0
      return (Fingerprint high low)

pokeFingerprint :: Ptr Fingerprint -> Fingerprint -> IO ()
pokeFingerprint p (Fingerprint high low) = do
      let pokeW64 :: Ptr Word8 -> Int -> Word64 -> IO ()
          pokeW64 p 0  !i = return ()
          pokeW64 p !n !i = do
                pokeElemOff p (n-1) (fromIntegral i)
                pokeW64 p (n-1) (i `shiftR` 8)

      pokeW64 (castPtr p) 8 high
      pokeW64 (castPtr p `plusPtr` 8) 8 low

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints fs = unsafeDupablePerformIO $
  withArrayLen fs $ \len p -> do
    fingerprintData (castPtr p) (len * sizeOf (head fs))

fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData buf len = do
  allocaBytes SIZEOF_STRUCT_MD5CONTEXT $ \pctxt -> do
    c_MD5Init pctxt
    c_MD5Update pctxt buf (fromIntegral len)
    allocaBytes 16 $ \pdigest -> do
      c_MD5Final pdigest pctxt
      peekFingerprint (castPtr pdigest)

fingerprintString :: String -> Fingerprint
fingerprintString str = unsafeDupablePerformIO $
  GHC.Foreign.withCStringLen utf8 str $ \(p,len) ->
     fingerprintData (castPtr p) len

data MD5Context

foreign import ccall unsafe "MD5Init"
   c_MD5Init   :: Ptr MD5Context -> IO ()
foreign import ccall unsafe "MD5Update"
   c_MD5Update :: Ptr MD5Context -> Ptr Word8 -> CInt -> IO ()
foreign import ccall unsafe "MD5Final"
   c_MD5Final  :: Ptr Word8 -> Ptr MD5Context -> IO ()
