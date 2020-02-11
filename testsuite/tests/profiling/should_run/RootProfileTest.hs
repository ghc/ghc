{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

import Control.Monad
import Control.Exception (evaluate)

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import System.IO
import System.Mem

import GHC.Profiling
import GHC.Exts

foreign import ccall "&g_rootProfileDebugLevel" g_rootProfileDebugLevel
  :: Ptr CInt

main :: IO ()
main = do
  poke g_rootProfileDebugLevel 1
  hSetBuffering stdout NoBuffering
  putStrLn "= simple =" >> simple
  putStrLn "\n\n\n\n= sharing =" >> sharing
  poke g_rootProfileDebugLevel 0

go :: Int# -> [Int]
go x = I# x : go (x +# 1#)

simple :: IO ()
simple = do
  let xs = go 0#; xs :: [Int]
  setHeapProfilingRoots [Root "xs" xs]
  performMajorGC
  forM_ [0..10] $ \i -> do
    _ <- evaluate (xs !! i)
    performMajorGC

sharing :: IO ()
sharing = do
  let x1 = 1 : x2
      x2 = 2 : x3
      x3 = 3 : x4
      x4 = 4 : x5
      x5 = 5 : x6
      x6 = 6 : x7
      x7 = []
      x7 :: [Word]

  setHeapProfilingRoots
    [ Root "1" x1
    , Root "2" x2
    , Root "3" x3
    , Root "4" x4
    , Root "5" x5
    , Root "6" x6
    , Root "[]" x7
    ]

  performMajorGC
