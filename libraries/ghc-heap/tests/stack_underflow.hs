{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Bool (Bool (True))
import GHC.Exts.Heap
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.InfoTable.Types
import GHC.Exts.Stack.Decode
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack
import TestUtils

main = loop 128

{-# NOINLINE loop #-}
loop 0 = Control.Monad.void getStack
loop n = print "x" >> loop (n - 1) >> print "x"

getStack :: HasCallStack => IO ()
getStack = do
  (s, decodedStack) <- getDecodedStack
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
  stackClosures <- mapM (getBoxedClosureData . nextChunk) underflowFrames
  let stackBoxes = map stack stackClosures
  framesOfChunks <- mapM (mapM getBoxedClosureData) stackBoxes
  assertThat
    "No empty stack chunks"
    (== True)
    ( not (any null framesOfChunks)
    )
