module Main where

import Control.Concurrent.STM
import Control.Exception
import GHC.Conc
import GHC.Exts.DecodeStack
import GHC.Stack.CloneStack
import TestUtils

main :: IO ()
main = do
  (stackSnapshot, decodedStack) <-
    atomically $
      catchSTM @SomeException (unsafeIOToSTM getDecodedStack) throwSTM

  assertStackInvariants stackSnapshot decodedStack
  assertThat
    "Stack contains one catch stm frame"
    (== 1)
    (length $ filter isCatchStmFrame decodedStack)

getDecodedStack :: IO (StackSnapshot, [StackFrame])
getDecodedStack = do
  s <-cloneMyStack
  fs <- decodeStack s
  pure (s, fs)

isCatchStmFrame :: StackFrame -> Bool
isCatchStmFrame (CatchStmFrame _ _) = True
isCatchStmFrame _ = False
