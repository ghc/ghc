-- |
-- Module      : Crypto.Random.SystemDRG
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE BangPatterns #-}
module Crypto.Random.SystemDRG
    ( SystemDRG
    , getSystemDRG
    ) where

import           Crypto.Random.Types
import           Crypto.Random.Entropy.Unsafe
import           Crypto.Internal.Compat
import           Data.ByteArray (ScrubbedBytes, ByteArray)
import           Data.Memory.PtrMethods as B (memCopy)
import           Data.Maybe (catMaybes)
import           Data.Tuple (swap)
import           Foreign.Ptr
import qualified Data.ByteArray as B
import           System.IO.Unsafe (unsafeInterleaveIO)

-- | A referentially transparent System representation of
-- the random evaluated out of the system.
--
-- Holding onto a specific DRG means that all the already
-- evaluated bytes will be consistently replayed.
--
-- There's no need to reseed this DRG, as only pure
-- entropy is represented here.
data SystemDRG = SystemDRG !Int [ScrubbedBytes]

instance DRG SystemDRG where
    randomBytesGenerate = generate

systemChunkSize :: Int
systemChunkSize = 256

-- | Grab one instance of the System DRG
getSystemDRG :: IO SystemDRG
getSystemDRG = do
    backends <- catMaybes `fmap` sequence supportedBackends
    let getNext = unsafeInterleaveIO $ do
            bs   <- B.alloc systemChunkSize (replenish systemChunkSize backends)
            more <- getNext
            return (bs:more)
    SystemDRG 0 <$> getNext

generate :: ByteArray output => Int -> SystemDRG -> (output, SystemDRG)
generate nbBytes (SystemDRG ofs sysChunks) = swap $ unsafeDoIO $ B.allocRet nbBytes $ loop ofs sysChunks nbBytes
  where loop currentOfs chunks 0 _ = return $! SystemDRG currentOfs chunks
        loop _          []     _ _ = error "SystemDRG: the impossible happened: empty chunk"
        loop currentOfs oChunks@(c:cs) n d = do
            let currentLeft = B.length c - currentOfs
                toCopy      = min n currentLeft
                nextOfs     = currentOfs + toCopy
                n'          = n - toCopy
            B.withByteArray c $ \src -> B.memCopy d (src `plusPtr` currentOfs) toCopy
            if nextOfs == B.length c
                then loop 0 cs n' (d `plusPtr` toCopy)
                else loop nextOfs oChunks n' (d `plusPtr` toCopy)
