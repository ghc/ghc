-- |
-- Module      : Crypto.Data.AFIS
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Haskell implementation of the Anti-forensic information splitter
-- available in LUKS. <http://clemens.endorphin.org/AFsplitter>
--
-- The algorithm bloats an arbitrary secret with many bits that are necessary for
-- the recovery of the key (merge), and allow greater way to permanently
-- destroy a key stored on disk.
--
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Data.AFIS
    ( split
    , merge
    ) where

import           Crypto.Hash
import           Crypto.Random.Types
import           Crypto.Internal.Compat
import           Control.Monad (forM_, foldM)
import           Data.Word
import           Data.Bits
import           Foreign.Storable
import           Foreign.Ptr

import           Crypto.Internal.ByteArray (ByteArray, Bytes, MemView(..))
import qualified Crypto.Internal.ByteArray as B

import           Data.Memory.PtrMethods (memSet, memCopy)

-- | Split data to diffused data, using a random generator and
-- an hash algorithm.
--
-- the diffused data will consist of random data for (expandTimes-1)
-- then the last block will be xor of the accumulated random data diffused by
-- the hash algorithm.
--
-- ----------
-- -  orig  -
-- ----------
--
-- ---------- ---------- --------------
-- - rand1  - - rand2  - - orig ^ acc -
-- ---------- ---------- --------------
--
-- where acc is :
--   acc(n+1) = hash (n ++ rand(n)) ^ acc(n)
--
split :: (ByteArray ba, HashAlgorithm hash, DRG rng)
      => hash  -- ^ Hash algorithm to use as diffuser
      -> rng   -- ^ Random generator to use
      -> Int   -- ^ Number of times to diffuse the data.
      -> ba    -- ^ original data to diffuse.
      -> (ba, rng)         -- ^ The diffused data
{-# NOINLINE split #-}
split hashAlg rng expandTimes src
    | expandTimes <= 1 = error "invalid expandTimes value"
    | otherwise        = unsafeDoIO $ do
        (rng', bs) <- B.allocRet diffusedLen runOp
        return (bs, rng')
  where diffusedLen = blockSize * expandTimes
        blockSize   = B.length src
        runOp dstPtr = do
            let lastBlock = dstPtr `plusPtr` (blockSize * (expandTimes-1))
            memSet lastBlock 0 blockSize
            let randomBlockPtrs = map (plusPtr dstPtr . (*) blockSize) [0..(expandTimes-2)]
            rng' <- foldM fillRandomBlock rng randomBlockPtrs
            mapM_ (addRandomBlock lastBlock) randomBlockPtrs
            B.withByteArray src $ \srcPtr -> xorMem srcPtr lastBlock blockSize
            return rng'
        addRandomBlock lastBlock blockPtr = do
            xorMem blockPtr lastBlock blockSize
            diffuse hashAlg lastBlock blockSize
        fillRandomBlock g blockPtr = do
            let (rand :: Bytes, g') = randomBytesGenerate blockSize g
            B.withByteArray rand $ \randPtr -> memCopy blockPtr randPtr blockSize
            return g'

-- | Merge previously diffused data back to the original data.
merge :: (ByteArray ba, HashAlgorithm hash)
      => hash  -- ^ Hash algorithm used as diffuser
      -> Int   -- ^ Number of times to un-diffuse the data
      -> ba    -- ^ Diffused data
      -> ba    -- ^ Original data
{-# NOINLINE merge #-}
merge hashAlg expandTimes bs
    | r /= 0            = error "diffused data not a multiple of expandTimes"
    | originalSize <= 0 = error "diffused data null"
    | otherwise         = B.allocAndFreeze originalSize $ \dstPtr ->
        B.withByteArray bs $ \srcPtr -> do
            memSet dstPtr 0 originalSize
            forM_ [0..(expandTimes-2)] $ \i -> do
                xorMem (srcPtr `plusPtr` (i * originalSize)) dstPtr originalSize
                diffuse hashAlg dstPtr originalSize
            xorMem (srcPtr `plusPtr` ((expandTimes-1) * originalSize)) dstPtr originalSize
            return ()
  where (originalSize,r) = len `quotRem` expandTimes
        len              = B.length bs

-- | inplace Xor with an input
-- dst = src `xor` dst
xorMem :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
xorMem src dst sz
    | sz `mod` 64 == 0 = loop 8 (castPtr src :: Ptr Word64) (castPtr dst) sz
    | sz `mod` 32 == 0 = loop 4 (castPtr src :: Ptr Word32) (castPtr dst) sz
    | otherwise        = loop 1 (src :: Ptr Word8) dst sz
  where loop _    _ _ 0 = return ()
        loop incr s d n = do a <- peek s
                             b <- peek d
                             poke d (a `xor` b)
                             loop incr (s `plusPtr` incr) (d `plusPtr` incr) (n-incr)

diffuse :: HashAlgorithm hash
        => hash      -- ^ Hash function to use as diffuser
        -> Ptr Word8 -- ^ buffer to diffuse, modify in place
        -> Int       -- ^ length of buffer to diffuse
        -> IO ()
diffuse hashAlg src sz = loop src 0
  where (full,pad) = sz `quotRem` digestSize 
        loop s i
            | i < full = do h <- hashBlock i s digestSize
                            B.withByteArray h $ \hPtr -> memCopy s hPtr digestSize
                            loop (s `plusPtr` digestSize) (i+1)
            | pad /= 0 = do h <- hashBlock i s pad
                            B.withByteArray h $ \hPtr -> memCopy s hPtr pad
                            return ()
            | otherwise = return ()

        digestSize = hashDigestSize hashAlg

        -- Hash [ BE32(n), (p .. p+hashSz) ]
        hashBlock n p hashSz = do
            let ctx = hashInitWith hashAlg
            return $! hashFinalize $ hashUpdate (hashUpdate ctx (be32 n)) (MemView p hashSz)

        be32 :: Int -> Bytes
        be32 n = B.allocAndFreeze 4 $ \ptr -> do
            poke ptr               (f8 (n `shiftR` 24))
            poke (ptr `plusPtr` 1) (f8 (n `shiftR` 16))
            poke (ptr `plusPtr` 2) (f8 (n `shiftR` 8))
            poke (ptr `plusPtr` 3) (f8 n)
          where
                f8 :: Int -> Word8
                f8 = fromIntegral
