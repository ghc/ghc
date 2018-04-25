{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign

-- Test allocate(), the easy way.
data Cap = Cap
foreign import ccall "rts_unsafeGetMyCapability" myCapability :: IO (Ptr Cap)
foreign import ccall "allocate" allocate :: Ptr Cap -> Word -> IO (Ptr ())

-- Number of words n such that n * sizeof(W_) exactly overflows a word
-- (2^30 on a 32-bit system, 2^61 on a 64-bit system)
overflowWordCount :: Word
overflowWordCount = fromInteger $
                    (fromIntegral (maxBound :: Word) + 1) `div`
                    fromIntegral (sizeOf (undefined :: Word))

main = do
  cap <- myCapability
  allocate cap (overflowWordCount - 1)
