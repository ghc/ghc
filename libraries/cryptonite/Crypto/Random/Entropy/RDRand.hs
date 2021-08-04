-- |
-- Module      : Crypto.Random.Entropy.RDRand
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE ForeignFunctionInterface #-}
module Crypto.Random.Entropy.RDRand
    ( RDRand
    ) where

import Foreign.Ptr
import Foreign.C.Types
import Data.Word (Word8)
import Crypto.Random.Entropy.Source

foreign import ccall unsafe "cryptonite_cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "cryptonite_get_rand_bytes"
  c_get_rand_bytes :: Ptr Word8 -> CInt -> IO CInt

-- | Fake handle to Intel RDRand entropy CPU instruction
data RDRand = RDRand

instance EntropySource RDRand where
    entropyOpen     = rdrandGrab
    entropyGather _ = rdrandGetBytes
    entropyClose  _ = return ()

rdrandGrab :: IO (Maybe RDRand)
rdrandGrab = supported `fmap` c_cpu_has_rdrand
  where supported 0 = Nothing
        supported _ = Just RDRand

rdrandGetBytes :: Ptr Word8 -> Int -> IO Int
rdrandGetBytes ptr sz = fromIntegral `fmap` c_get_rand_bytes ptr (fromIntegral sz)
