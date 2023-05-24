{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main (main) where

import Control.Monad
import Data.Array.Byte
import Data.Int
import GHC.Exts
import GHC.IO

main :: IO ()
main = do
  let szInt = 8
      sz = 101

  cur0 <- newAlignedPinnedByteArray (sz*szInt) 4096
  old0 <- newAlignedPinnedByteArray (sz*szInt) 64

  print (sizeofMutableByteArray cur0)
  print (sizeofMutableByteArray old0)

  replicateM_ 20 $ do
    forM_ [0 .. sz-1] $ \i -> do
      putStrLn $ "I: " <> show i
      writeByteArray cur0 i (2*i)

newAlignedPinnedByteArray :: Int -> Int -> IO (MutableByteArray RealWorld)
newAlignedPinnedByteArray (I# sz) (I# align) = IO $ \s0 ->
    case newAlignedPinnedByteArray# sz align s0 of
      (# s1, ba #) -> (# s1, MutableByteArray ba #)

sizeofMutableByteArray :: MutableByteArray RealWorld -> Int
sizeofMutableByteArray (MutableByteArray arr#) = I# (sizeofMutableByteArray# arr#)

writeByteArray :: MutableByteArray RealWorld -> Int -> Int -> IO ()
writeByteArray (MutableByteArray arr#) (I# i#) (I# x#) =
  IO (\s# -> case writeIntArray# arr# i# x# s# of
    s'# -> (# s'#, () #))
