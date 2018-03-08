{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Compact
import GHC.Int
import GHC.Prim
import GHC.IO
import GHC.Exts

data BA = ByteArray ByteArray#

newByteArray :: Int -> IO BA
newByteArray (I# sz) = IO $ \s -> case newByteArray# sz s of {
  (# s', arr# #) -> case unsafeFreezeByteArray# arr# s of {
  (# s'', barr# #) -> (# s', ByteArray barr# #) }}

main :: IO ()
main = do
  ByteArray arr1# <- fmap getCompact $ newByteArray 65000 >>= compact
  ByteArray arr2# <- newByteArray 65000
  print (I# (isByteArrayPinned# arr1#))
  print (I# (isByteArrayPinned# arr2#))
  putStrLn "Finished"
