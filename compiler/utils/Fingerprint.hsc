-- ----------------------------------------------------------------------------
-- 
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- ----------------------------------------------------------------------------

module Fingerprint (
        Fingerprint(..), fingerprint0,
        readHexFingerprint,
        fingerprintData,
        fingerprintString
   ) where

#include "md5.h"
##include "HsVersions.h"

import Outputable

import Text.Printf
import Numeric          ( readHex )

##if __GLASGOW_HASKELL__ >= 701
-- The MD5 implementation is now in base, to support Typeable
import GHC.Fingerprint
##endif

##if __GLASGOW_HASKELL__ < 701
import Data.Char
import Foreign
import Foreign.C
import GHC.IO (unsafeDupablePerformIO)

-- Using 128-bit MD5 fingerprints for now.

data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Eq, Ord)
        -- or ByteString?

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

peekFingerprint :: Ptr Word8 -> IO Fingerprint
peekFingerprint p = do
      let peekW64 :: Ptr Word8 -> Int -> Word64 -> IO Word64
          STRICT3(peekW64)
          peekW64 _ 0 i = return i
          peekW64 p n i = do 
                w8 <- peek p
                peekW64 (p `plusPtr` 1) (n-1) 
                    ((i `shiftL` 8) .|. fromIntegral w8)

      high <- peekW64 p 8 0
      low  <- peekW64 (p `plusPtr` 8) 8 0
      return (Fingerprint high low)

fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData buf len = do
  allocaBytes (#const sizeof(struct MD5Context)) $ \pctxt -> do
    c_MD5Init pctxt
    c_MD5Update pctxt buf (fromIntegral len)
    allocaBytes 16 $ \pdigest -> do
      c_MD5Final pdigest pctxt
      peekFingerprint (castPtr pdigest)

-- This is duplicated in libraries/base/GHC/Fingerprint.hs
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

foreign import ccall unsafe "MD5Init"
   c_MD5Init   :: Ptr MD5Context -> IO ()
foreign import ccall unsafe "MD5Update"
   c_MD5Update :: Ptr MD5Context -> Ptr Word8 -> CInt -> IO ()
foreign import ccall unsafe "MD5Final"
   c_MD5Final  :: Ptr Word8 -> Ptr MD5Context -> IO ()
##endif

instance Outputable Fingerprint where
  ppr (Fingerprint w1 w2) = text (printf "%016x%016x" i1 i2)
    where i1 = fromIntegral w1 :: Integer
          i2 = fromIntegral w2 :: Integer
          -- printf in GHC 6.4.2 didn't have Word64 instances

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)

