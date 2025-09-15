{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import GHC.Exts
import GHC.Int
import GHC.IO
import Control.Concurrent (threadDelay)
import System.Mem (performMajorGC)
import Control.Monad (mapM_, replicateM)

data ByteArray = BA (MutableByteArray# RealWorld)

newByteArray :: Int -> IO ByteArray
newByteArray (I# n) = IO $ \s ->
  case {-# SCC suzanne #-} newPinnedByteArray# n s of
    (# s', ba# #) -> (# s', BA ba# #)

writeByteArray :: Int -> Int -> ByteArray -> IO ()
writeByteArray (I# offset) (I# n) (BA ba#) = IO $ \s ->
  case writeIntArray# ba# offset n s of
    s' -> (# s', () #)

main :: IO ()
main = do
  bas <- {-# SCC robert #-} mapM (\n -> newByteArray (100*n)) [0..1000]
  mapM_ doSomething [0..4]
  mapM_ (writeByteArray 0 42) bas

doSomething :: Int -> IO ()
doSomething n = do
  threadDelay (1000*1000)
  print n
  performMajorGC
