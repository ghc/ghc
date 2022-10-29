module Main where

import Control.Concurrent.STM
import Control.Exception
import GHC.Conc
import GHC.Exts.DecodeStack
import GHC.Stack.CloneStack
import TestUtils

main :: IO ()
main = do
  decodedStack <-
    atomically $
      catchSTM @SomeException (unsafeIOToSTM getDecodedStack) throwSTM

  assertStackInvariants decodedStack
  assertThat
    "Stack contains one catch stm frame"
    (== 1)
    (length $ filter isCatchStmFrame decodedStack)

getDecodedStack :: IO [StackFrame]
getDecodedStack = cloneMyStack >>= decodeStack

isCatchStmFrame :: StackFrame -> Bool
isCatchStmFrame (CatchStmFrame _ _) = True
isCatchStmFrame _ = False
