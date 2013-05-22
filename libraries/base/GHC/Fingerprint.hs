{-# LANGUAGE Trustworthy #-}
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

import GHC.Fingerprint.Type

-- for SIZEOF_STRUCT_MD5CONTEXT:
#include "HsBaseConfig.h"

-- XXX instance Storable Fingerprint
-- defined in Foreign.Storable to avoid orphan instance

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

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
      peek (castPtr pdigest :: Ptr Fingerprint)

-- This is duplicated in compiler/utils/Fingerprint.hsc
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

data MD5Context

foreign import ccall unsafe "__hsbase_MD5Init"
   c_MD5Init   :: Ptr MD5Context -> IO ()
foreign import ccall unsafe "__hsbase_MD5Update"
   c_MD5Update :: Ptr MD5Context -> Ptr Word8 -> CInt -> IO ()
foreign import ccall unsafe "__hsbase_MD5Final"
   c_MD5Final  :: Ptr Word8 -> Ptr MD5Context -> IO ()

