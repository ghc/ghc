-- |
-- Module      : Crypto.Random.EntropyPool
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.Random.EntropyPool
    ( EntropyPool
    , createEntropyPool
    , createEntropyPoolWith
    , getEntropyFrom
    ) where

import           Control.Concurrent.MVar
import           Crypto.Random.Entropy.Unsafe
import           Crypto.Internal.ByteArray (ByteArray, ScrubbedBytes)
import qualified Crypto.Internal.ByteArray as B
import           Data.Word (Word8)
import           Data.Maybe (catMaybes)
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.Ptr (plusPtr, Ptr)

-- | Pool of Entropy. Contains a self-mutating pool of entropy,
-- that is always guaranteed to contain data.
data EntropyPool = EntropyPool [EntropyBackend] (MVar Int) ScrubbedBytes

-- size of entropy pool by default
defaultPoolSize :: Int
defaultPoolSize = 4096

-- | Create a new entropy pool of a specific size
--
-- While you can create as many entropy pools as you want,
-- the pool can be shared between multiples RNGs.
createEntropyPoolWith :: Int -> [EntropyBackend] -> IO EntropyPool
createEntropyPoolWith poolSize backends = do
    m  <- newMVar 0
    sm <- B.alloc poolSize (replenish poolSize backends)
    return $ EntropyPool backends m sm

-- | Create a new entropy pool with a default size.
--
-- While you can create as many entropy pools as you want,
-- the pool can be shared between multiples RNGs.
createEntropyPool :: IO EntropyPool
createEntropyPool = do
    backends <- catMaybes `fmap` sequence supportedBackends
    createEntropyPoolWith defaultPoolSize backends

-- | Put a chunk of the entropy pool into a buffer
getEntropyPtr :: EntropyPool -> Int -> Ptr Word8 -> IO ()
getEntropyPtr (EntropyPool backends posM sm) n outPtr =
    B.withByteArray sm $ \entropyPoolPtr ->
        modifyMVar_ posM $ \pos ->
            copyLoop outPtr entropyPoolPtr pos n
  where poolSize = B.length sm
        copyLoop d s pos left
            | left == 0 = return pos
            | otherwise = do
                wrappedPos <-
                    if pos == poolSize
                        then replenish poolSize backends s >> return 0
                        else return pos
                let m = min (poolSize - wrappedPos) left
                copyBytes d (s `plusPtr` wrappedPos) m
                copyLoop (d `plusPtr` m) s (wrappedPos + m) (left - m)

-- | Grab a chunk of entropy from the entropy pool.
getEntropyFrom :: ByteArray byteArray => EntropyPool -> Int -> IO byteArray
getEntropyFrom pool n = B.alloc n (getEntropyPtr pool n)
