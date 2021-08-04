-- |
-- Module      : Crypto.Random.Entropy.Unsafe
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.Random.Entropy.Unsafe
    ( replenish
    , module Crypto.Random.Entropy.Backend
    ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Crypto.Random.Entropy.Backend

-- | Refill the entropy in a buffer
--
-- Call each entropy backend in turn until the buffer has
-- been replenished.
--
-- If the buffer cannot be refill after 3 loopings, this will raise
-- an User Error exception
replenish :: Int -> [EntropyBackend] -> Ptr Word8 -> IO ()
replenish _        []       _   = fail "cryptonite: random: cannot get any source of entropy on this system"
replenish poolSize backends ptr = loop 0 backends ptr poolSize
  where loop :: Int -> [EntropyBackend] -> Ptr Word8 -> Int -> IO ()
        loop retry [] p n | n == 0     = return ()
                          | retry == 3 = error "cryptonite: random: cannot fully replenish"
                          | otherwise  = loop (retry+1) backends p n
        loop _     (_:_)  _ 0 = return ()
        loop retry (b:bs) p n = do
            r <- gatherBackend b p n
            loop retry bs (p `plusPtr` r) (n - r)
