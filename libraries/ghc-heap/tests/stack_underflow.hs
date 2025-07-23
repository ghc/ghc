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

main = loop 256

{-# NOINLINE loop #-}
loop 0 = Control.Monad.void getStack
loop n = print "x" >> loop (n - 1) >> print "x"

getStack :: HasCallStack => IO ()
getStack = do
  (s, decodedStack) <- getDecodedStack
  assertStackInvariants decodedStack
  assertThat
    "Stack contains underflow frames"
    (== True)
    (any isUnderflowFrame decodedStack)
  assertStackChunksAreDecodable decodedStack
  return ()

isUnderflowFrame :: StackFrame -> Bool
isUnderflowFrame (UnderflowFrame {..}) = tipe info_tbl == UNDERFLOW_FRAME
isUnderflowFrame _ = False

assertStackChunksAreDecodable :: HasCallStack => [StackFrame] -> IO ()
assertStackChunksAreDecodable s = do
  let underflowFrames = filter isUnderflowFrame s
  assertThat
    ("Expect some underflow frames. Got " ++ show (length underflowFrames))
    (>= 2)
    (length underflowFrames)
  let stackFrames = map (ssc_stack . nextChunk) underflowFrames
  assertThat
    "No empty stack chunks"
    (== True)
    ( not (any null stackFrames)
    )
