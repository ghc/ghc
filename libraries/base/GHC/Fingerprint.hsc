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

module GHC.Fingerprint (
        Fingerprint(..), fingerprint0,
        fingerprintData,
        fingerprintString,
        fingerprintFingerprints,
        getFileHash
   ) where

import GHC.IO
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Real
import GHC.Show
import Foreign
import Foreign.C
import System.IO

import GHC.Fingerprint.Type

#include "HsHash.h"

-- XXX instance Storable Fingerprint
-- defined in Foreign.Storable to avoid orphan instance

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints fs = unsafeDupablePerformIO $
  withArrayLen fs $ \len p ->
    fingerprintData (castPtr p) (len * sizeOf (head fs))

fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData buf len =
  allocaBytes (#const sizeof(XXH3_state_t)) $ \pctxt -> do
    c_HashInit pctxt
    c_HashUpdate pctxt buf (fromIntegral len)
    allocaBytes 16 $ \pdigest -> do
      c_HashFinal pctxt pdigest
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

-- | Computes the hash of a given file.
-- This function loops over the handle, running in constant memory.
--
-- @since 4.7.0.0
getFileHash :: FilePath -> IO Fingerprint
getFileHash path = withBinaryFile path ReadMode $ \h ->
  allocaBytes  (#const sizeof(XXH3_state_t)) $ \pctxt -> do
    c_HashInit pctxt

    processChunks h (\buf size -> c_HashUpdate pctxt buf (fromIntegral size))

    allocaBytes 16 $ \pdigest -> do
      c_HashFinal pctxt pdigest
      peek (castPtr pdigest :: Ptr Fingerprint)

  where
    _BUFSIZE = 4096

    -- | Loop over _BUFSIZE sized chunks read from the handle,
    -- passing the callback a block of bytes and its size.
    processChunks :: Handle -> (Ptr Word8 -> Int -> IO ()) -> IO ()
    processChunks h f = allocaBytes _BUFSIZE $ \arrPtr ->

      let loop = do
            count <- hGetBuf h arrPtr _BUFSIZE
            eof <- hIsEOF h
            when (count /= _BUFSIZE && not eof) $ errorWithoutStackTrace $
              "GHC.Fingerprint.getFileHash: only read " ++ show count ++ " bytes"

            f arrPtr count

            when (not eof) loop

      in loop

data HashContext

foreign import ccall unsafe "__hsbase_hash_init"
   c_HashInit   :: Ptr HashContext -> IO ()
foreign import ccall unsafe "__hsbase_hash_update"
   c_HashUpdate :: Ptr HashContext -> Ptr Word8 -> CInt -> IO ()
foreign import ccall unsafe "__hsbase_hash_final"
   c_HashFinal  :: Ptr HashContext -> Ptr Word64 -> IO ()
