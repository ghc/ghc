{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bool (Bool (True))
import GHC.Exts.DecodeStack
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack
import TestUtils

main = loop 128

{-# NOINLINE loop #-}
loop 0 = () <$ getStack
loop n = print "x" >> loop (n - 1) >> print "x"

getStack :: HasCallStack => IO ()
getStack = do
  !s <- cloneMyStack
  !decodedStack <- decodeStack s
  -- Uncomment to see the frames (for debugging purposes)
  -- hPutStrLn stderr $ "Stack frames : " ++ show decodedStack
  assertStackInvariants decodedStack
  assertThat
    "Stack contains underflow frames"
    (== True)
    (any isUnderflowFrame decodedStack)
  assertStackChunksAreDecodable decodedStack
  return ()

isUnderflowFrame (UnderflowFrame _) = True
isUnderflowFrame _ = False

assertStackChunksAreDecodable :: HasCallStack => [StackFrame] -> IO ()
assertStackChunksAreDecodable s = do
  let underflowFrames = filter isUnderflowFrame s
  framesOfChunks <- mapM (decodeStack . nextChunk) underflowFrames
  assertThat
    "No empty stack chunks"
    (== True)
    ( not (any null framesOfChunks)
    )
