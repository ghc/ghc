-- |
-- Module      : Crypto.Random.Entropy.Source
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.Random.Entropy.Source where

import Foreign.Ptr
import Data.Word (Word8)

-- | A handle to an entropy maker, either a system capability
-- or a hardware generator.
class EntropySource a where
    -- | Try to open an handle for this source
    entropyOpen   :: IO (Maybe a)
    -- | Try to gather a number of entropy bytes into a buffer.
    -- Return the number of actual bytes gathered
    entropyGather :: a -> Ptr Word8 -> Int -> IO Int
    -- | Close an open handle
    entropyClose  :: a -> IO ()
