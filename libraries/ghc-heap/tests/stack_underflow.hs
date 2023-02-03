{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bool (Bool (True))
import GHC.Exts.DecodeStack
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.InfoTable.Types
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack
import TestUtils

main = loop 128

{-# NOINLINE loop #-}
loop 0 = () <$ getStack
loop n = print "x" >> loop (n - 1) >> print "x"

getStack :: HasCallStack => IO ()
getStack = do
  (s, decodedStack) <- getDecodedStack
  -- Uncomment to see the frames (for debugging purposes)
  -- hPutStrLn stderr $ "Stack frames : " ++ show decodedStack
  assertStackInvariants s decodedStack
  assertThat
    "Stack contains underflow frames"
    (== True)
    (any isUnderflowFrame decodedStack)
  assertStackChunksAreDecodable decodedStack
  return ()

isUnderflowFrame (UnderflowFrame {..}) = tipe info == UNDERFLOW_FRAME
isUnderflowFrame _ = False

assertStackChunksAreDecodable :: HasCallStack => [Closure] -> IO ()
assertStackChunksAreDecodable s = do
  let underflowFrames = filter isUnderflowFrame s
  let framesOfChunks = map (stackClosures . decodeStack . nextChunk) underflowFrames
  assertThat
    "No empty stack chunks"
    (== True)
    ( not (any null framesOfChunks)
    )
