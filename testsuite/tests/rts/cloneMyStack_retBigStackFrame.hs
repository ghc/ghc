{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Control.Concurrent
import Data.IORef
import GHC.IO.Unsafe
import GHC.Exts (StackSnapshot#)
import GHC.Stack.CloneStack
import System.Mem

foreign import ccall "expectSixtyFourOnesInRetBigFrame" expectSixtyFourOnesInRetBigFrame :: StackSnapshot# -> IO ()

cloneStack_returnInt :: IORef (Maybe StackSnapshot) -> Int
cloneStack_returnInt ioRef = unsafePerformIO $ do
  stackSnapshot <- cloneMyStack

  performMajorGC

  writeIORef ioRef (Just stackSnapshot)

  performMajorGC

  return 42

-- | Clone a stack with a RET_BIG closure and check it in snapshot.
-- In the meanwhile enforce several garbage collections in different places to
-- ensure that the stack snapshot is still valid afterwards (is not gc'ed while
-- in use).
main :: IO ()
main = do
  stackRef <- newIORef Nothing

  bigFun (cloneStack_returnInt stackRef) 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

  Just (StackSnapshot stackSnapshot) <- readIORef stackRef

  performMajorGC

  expectSixtyFourOnesInRetBigFrame stackSnapshot

  return ()

bigFun !a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 =
  do
    print $ a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 + a36 + a37 + a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47 + a48 + a49 + a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 + a59 + a60 + a61 + a62 + a63 + a64 + a65

    return ()
