{-# LANGUAGE Trustworthy #-}

module GHC.Fingerprint (
        Fingerprint(..), fingerprint0,
        fingerprintData,
        fingerprintString,
        fingerprintFingerprints,
        getFileHash
   ) where

import GHC.Internal.Fingerprint

import GHC.Internal.Base
       (
           ($),
           return,
           when,
           not,
           (&&),
           (/=),
           (++),
           Int,
           errorWithoutStackTrace
       )
import GHC.Internal.Show (show)
import GHC.Internal.Data.Maybe (Maybe (Nothing, Just))
import GHC.Internal.Word (Word8)
import GHC.Internal.System.IO
       (
           IO,
           FilePath,
           IOMode (ReadMode),
           withBinaryFile,
           hGetBuf,
           hIsEOF
       )
import GHC.Internal.Ptr (Ptr)

-- | Computes the hash of a given file.
-- This function runs in constant memory.
--
-- @since base-4.7.0.0
getFileHash :: FilePath -> IO Fingerprint
getFileHash path = withBinaryFile path ReadMode $ \ hdl ->
    let
        readChunk :: Ptr Word8 -> Int -> IO (Maybe Int)
        readChunk bufferPtr bufferSize = do
            chunkSize <- hGetBuf hdl bufferPtr bufferSize
            isFinished <- hIsEOF hdl
            when (chunkSize /= bufferSize && not isFinished)
                 (
                     errorWithoutStackTrace $
                     "GHC.Fingerprint.getFileHash: could only read " ++
                     show chunkSize                                  ++
                     " bytes, but more are available"
                 )
            return (if isFinished then Just chunkSize else Nothing)
    in fingerprintBufferedStream readChunk
