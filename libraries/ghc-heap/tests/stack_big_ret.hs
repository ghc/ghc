{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import GHC.Exts (StackSnapshot#)
import GHC.Exts.Heap
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.InfoTable.Types
import GHC.Exts.Stack.Decode
import GHC.IO.Unsafe
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack
import System.IO (hPutStrLn, stderr)
import System.Mem
import TestUtils

cloneStackReturnInt :: IORef (Maybe StackSnapshot) -> Int
cloneStackReturnInt ioRef = unsafePerformIO $ do
  stackSnapshot <- cloneMyStack

  writeIORef ioRef (Just stackSnapshot)

  pure 42

-- | Clone a stack with a RET_BIG closure and decode it.
main :: HasCallStack => IO ()
main = do
  stackRef <- newIORef Nothing

  bigFun (cloneStackReturnInt stackRef) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64

  mbStackSnapshot <- readIORef stackRef
  let stackSnapshot = fromJust mbStackSnapshot
  stackClosure <- decodeStack stackSnapshot
  let stackFrames = ssc_stack stackClosure

  assertStackInvariants stackFrames
  assertThat
    "Stack contains one big return frame"
    (== 1)
    (length $ filter isBigReturnFrame stackFrames)
  let cs = (stack_payload . head) $ filter isBigReturnFrame stackFrames
  let xs = zip [1 ..] cs
  mapM_ (uncurry checkArg) xs

checkArg :: Word -> StackField -> IO ()
checkArg w sf =
  case sf of
    StackWord _ -> error "Unexpected payload type from bitmap."
    StackBox b -> do
      c <- getBoxedClosureData b
      assertEqual CONSTR_0_1 $ (tipe . info) c
      assertEqual "I#" (name c)
      assertEqual "ghc-internal" (pkg c)
      assertEqual "GHC.Internal.Types" (modl c)
      assertEqual True $ (null . ptrArgs) c
      assertEqual [w] (dataArgs c)
      pure ()

isBigReturnFrame :: StackFrame -> Bool
isBigReturnFrame (RetBig info _) = tipe info == RET_BIG
isBigReturnFrame _ = False

{-# NOINLINE bigFun #-}
bigFun ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO ()
bigFun !a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 =
  do
    print $ a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 + a36 + a37 + a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47 + a48 + a49 + a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 + a59 + a60 + a61 + a62 + a63 + a64 + a65

    pure ()
