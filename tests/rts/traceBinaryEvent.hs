{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Word
import GHC.Base
import GHC.Ptr

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

main :: IO ()
main = do
  traceBinaryEventIO "0123456789"
  traceBinaryEventIO $ B.replicate 10 0
  traceBinaryEventIO $ B.replicate (maxSize + 1) 0

maxSize :: Int
maxSize = fromIntegral (maxBound :: Word16)

traceBinaryEventIO :: B.ByteString -> IO ()
traceBinaryEventIO bytes =
  BU.unsafeUseAsCStringLen bytes $ \(Ptr p, I# n) -> IO $ \s -> do
    case traceBinaryEvent# p n s of
      s' -> (# s', () #)
