{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy       #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- ----------------------------------------------------------------------------

module GHC.Internal.Fingerprint (
        Fingerprint(..), fingerprint0,
        fingerprintData,
        fingerprintString,
        fingerprintFingerprints,
        fingerprintBufferedStream
   ) where

import GHC.Internal.IO
import GHC.Internal.Base
import GHC.Internal.Bits
import GHC.Internal.Num
import GHC.Internal.Data.Maybe
import GHC.Internal.List
import GHC.Internal.Real
import GHC.Internal.Word
import GHC.Internal.Ptr
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.Marshal.Alloc
import GHC.Internal.Foreign.Marshal.Array
import GHC.Internal.Foreign.Storable

import GHC.Internal.Fingerprint.Type

-- for SIZEOF_STRUCT_MD5CONTEXT:
#include "HsBaseConfig.h"

-- XXX instance Storable Fingerprint
-- defined in GHC.Internal.Foreign.Storable to avoid orphan instance

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints fs = unsafeDupablePerformIO $
  withArrayLen fs $ \len p ->
    fingerprintData (castPtr p) (len * sizeOf (undefined :: Fingerprint))

fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData buf len =
  allocaBytes SIZEOF_STRUCT_MD5CONTEXT $ \pctxt -> do
    c_MD5Init pctxt
    c_MD5Update pctxt buf (fromIntegral len)
    allocaBytes 16 $ \pdigest -> do
      c_MD5Final pdigest pctxt
      peek (castPtr pdigest :: Ptr Fingerprint)

fingerprintString :: String -> Fingerprint
fingerprintString str = unsafeDupablePerformIO $
  withArrayLen word8s $ \len p ->
     fingerprintData p len
    where word8s = concatMap f str
          f c = let w32 :: Word32
                    w32 = fromIntegral (ord c)
                in [fromIntegral (w32 `shiftR` 24),
                    fromIntegral (w32 `shiftR` 16),
                    fromIntegral (w32 `shiftR` 8),
                    fromIntegral w32]

-- | Reads data in chunks and computes its hash.
-- This function runs in constant memory.
fingerprintBufferedStream :: (Ptr Word8 -> Int -> IO (Maybe Int))
                          -> IO Fingerprint
fingerprintBufferedStream readChunk =
  allocaBytes SIZEOF_STRUCT_MD5CONTEXT $ \pctxt -> do
    c_MD5Init pctxt
    allocaBytes _BUFSIZE $ \arrPtr ->
      let loop = do
            maybeRemainderSize <- readChunk arrPtr _BUFSIZE
            c_MD5Update pctxt
                        arrPtr
                        (fromIntegral (fromMaybe _BUFSIZE maybeRemainderSize))
            when (isNothing maybeRemainderSize) loop
      in loop
    allocaBytes 16 $ \pdigest -> do
      c_MD5Final pdigest pctxt
      peek (castPtr pdigest :: Ptr Fingerprint)
  where
    _BUFSIZE = 4096

data MD5Context

foreign import ccall unsafe "__hsbase_MD5Init"
   c_MD5Init   :: Ptr MD5Context -> IO ()
foreign import ccall unsafe "__hsbase_MD5Update"
   c_MD5Update :: Ptr MD5Context -> Ptr Word8 -> CInt -> IO ()
foreign import ccall unsafe "__hsbase_MD5Final"
   c_MD5Final  :: Ptr Word8 -> Ptr MD5Context -> IO ()
