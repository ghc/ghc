{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.STM
import Control.Exception
import GHC.Conc
import GHC.Exts.Heap
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.InfoTable.Types
import GHC.Exts.Stack.Decode
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
  assertThat
    "Stack contains one atomically frame"
    (== 1)
    (length $ filter isAtomicallyFrame decodedStack)

isCatchStmFrame :: Closure -> Bool
isCatchStmFrame (CatchStmFrame {..}) = tipe info == CATCH_STM_FRAME
isCatchStmFrame _ = False

isAtomicallyFrame :: Closure -> Bool
isAtomicallyFrame (AtomicallyFrame {..}) = tipe info == ATOMICALLY_FRAME
isAtomicallyFrame _ = False
