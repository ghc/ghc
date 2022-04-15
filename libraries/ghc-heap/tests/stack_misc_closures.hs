{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Data.Functor
import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap
import GHC.Exts.Heap.Closures
import GHC.Exts.Stack.Decode
import GHC.IO (IO (..))
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack (StackSnapshot (..))
import System.Info
import System.Mem
import TestUtils
import Unsafe.Coerce (unsafeCoerce)

foreign import prim "any_update_framezh" any_update_frame# :: SetupFunction

foreign import prim "any_catch_framezh" any_catch_frame# :: SetupFunction

foreign import prim "any_catch_stm_framezh" any_catch_stm_frame# :: SetupFunction

foreign import prim "any_catch_retry_framezh" any_catch_retry_frame# :: SetupFunction

foreign import prim "any_atomically_framezh" any_atomically_frame# :: SetupFunction

foreign import prim "any_ret_small_prim_framezh" any_ret_small_prim_frame# :: SetupFunction

foreign import prim "any_ret_small_prims_framezh" any_ret_small_prims_frame# :: SetupFunction

foreign import prim "any_ret_small_closure_framezh" any_ret_small_closure_frame# :: SetupFunction

foreign import prim "any_ret_small_closures_framezh" any_ret_small_closures_frame# :: SetupFunction

foreign import prim "any_ret_big_prims_min_framezh" any_ret_big_prims_min_frame# :: SetupFunction

foreign import prim "any_ret_big_closures_min_framezh" any_ret_big_closures_min_frame# :: SetupFunction

foreign import prim "any_ret_big_closures_two_words_framezh" any_ret_big_closures_two_words_frame# :: SetupFunction

foreign import prim "any_ret_fun_arg_n_prim_framezh" any_ret_fun_arg_n_prim_frame# :: SetupFunction

foreign import prim "any_ret_fun_arg_gen_framezh" any_ret_fun_arg_gen_frame# :: SetupFunction

foreign import prim "any_ret_fun_arg_gen_big_framezh" any_ret_fun_arg_gen_big_frame# :: SetupFunction

foreign import prim "any_bco_framezh" any_bco_frame# :: SetupFunction

foreign import prim "any_underflow_framezh" any_underflow_frame# :: SetupFunction

foreign import ccall "maxSmallBitmapBits" maxSmallBitmapBits_c :: Word

foreign import ccall "bitsInWord" bitsInWord :: Word

{- Test stategy
   ~~~~~~~~~~~~

- Create @StgStack@s in C that contain two frames: A stop frame and the frame
which's decoding should be tested.

- Cmm primops are used to get `StackSnapshot#` values. (This detour ensures that
the closures are referenced by `StackSnapshot#` and not garbage collected right
away.)

- These can then be decoded and checked.

This strategy may look pretty complex for a test. But, it can provide very
specific corner cases that would be hard to (reliably!) produce in Haskell.

N.B. `StackSnapshots` are managed by the garbage collector. It's important to
know that the GC may rewrite parts of the stack and that the stack must be sound
(otherwise, the GC may fail badly.) To find subtle garbage collection related
bugs, the GC is triggered several times.

The decission to make `StackSnapshots`s (and their closures) being managed by the
GC isn't accidential. It's closer to the reality of decoding stacks.

N.B. the test data stack are only meant be de decoded. They are not executable
(the result would likely be a crash or non-sense.)

- Due to the implementation details of the test framework, the Debug.Trace calls
are only shown when the test fails. They are used as markers to see where the
test fails on e.g. a segfault (where the HasCallStack constraint isn't helpful.)
-}
main :: HasCallStack => IO ()
main = do
  traceM "Test 1"
  test any_update_frame# $
    \case
      UpdateFrame {..} -> do
        assertEqual (tipe info) UPDATE_FRAME
        assertEqual 1 =<< (getWordFromBlackhole =<< getBoxedClosureData updatee)
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 2"
  testSize any_update_frame# 2
  traceM "Test 3"
  test any_catch_frame# $
    \case
      CatchFrame {..} -> do
        assertEqual (tipe info) CATCH_FRAME
        assertEqual exceptions_blocked 1
        assertConstrClosure 1 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 4"
  testSize any_catch_frame# 3
  traceM "Test 5"
  test any_catch_stm_frame# $
    \case
      CatchStmFrame {..} -> do
        assertEqual (tipe info) CATCH_STM_FRAME
        assertConstrClosure 1 =<< getBoxedClosureData catchFrameCode
        assertConstrClosure 2 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 6"
  testSize any_catch_stm_frame# 3
  traceM "Test 7"
  test any_catch_retry_frame# $
    \case
      CatchRetryFrame {..} -> do
        assertEqual (tipe info) CATCH_RETRY_FRAME
        assertEqual running_alt_code 1
        assertConstrClosure 2 =<< getBoxedClosureData first_code
        assertConstrClosure 3 =<< getBoxedClosureData alt_code
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 8"
  testSize any_catch_retry_frame# 4
  traceM "Test 9"
  test any_atomically_frame# $
    \case
      AtomicallyFrame {..} -> do
        assertEqual (tipe info) ATOMICALLY_FRAME
        assertConstrClosure 1 =<< getBoxedClosureData atomicallyFrameCode
        assertConstrClosure 2 =<< getBoxedClosureData result
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 10"
  testSize any_atomically_frame# 3
  traceM "Test 11"
  test any_ret_small_prim_frame# $
    \case
      RetSmall {..} -> do
        assertEqual (tipe info) RET_SMALL
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertUnknownTypeWordSizedPrimitive 1 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 12"
  testSize any_ret_small_prim_frame# 2
  traceM "Test 13"
  test any_ret_small_closure_frame# $
    \case
      RetSmall {..} -> do
        assertEqual (tipe info) RET_SMALL
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertConstrClosure 1 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 14"
  testSize any_ret_small_closure_frame# 2
  traceM "Test 15"
  test any_ret_small_closures_frame# $
    \case
      RetSmall {..} -> do
        assertEqual (tipe info) RET_SMALL
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) maxSmallBitmapBits
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1 .. maxSmallBitmapBits]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 16"
  testSize any_ret_small_closures_frame# (1 + fromIntegral maxSmallBitmapBits_c)
  traceM "Test 17"
  test any_ret_small_prims_frame# $
    \case
      RetSmall {..} -> do
        assertEqual (tipe info) RET_SMALL
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) maxSmallBitmapBits
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1 .. maxSmallBitmapBits]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 18"
  testSize any_ret_small_prims_frame# (1 + fromIntegral maxSmallBitmapBits_c)
  traceM "Test 19"
  test any_ret_big_prims_min_frame# $
    \case
      RetBig {..} -> do
        assertEqual (tipe info) RET_BIG
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) minBigBitmapBits
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1 .. minBigBitmapBits]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 20"
  testSize any_ret_big_prims_min_frame# (minBigBitmapBits + 1)
  traceM "Test 21"
  test any_ret_big_closures_min_frame# $
    \case
      RetBig {..} -> do
        assertEqual (tipe info) RET_BIG
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) minBigBitmapBits
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1 .. minBigBitmapBits]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 22"
  testSize any_ret_big_closures_min_frame# (minBigBitmapBits + 1)
  traceM "Test 23"
  test any_ret_big_closures_two_words_frame# $
    \case
      RetBig {..} -> do
        assertEqual (tipe info) RET_BIG
        pCs <- mapM getBoxedClosureData payload
        let closureCount = fromIntegral $ bitsInWord + 1
        assertEqual (length pCs) closureCount
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1 .. (fromIntegral closureCount)]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 24"
  testSize any_ret_big_closures_two_words_frame# (fromIntegral bitsInWord + 1 + 1)
  traceM "Test 25"
  test any_ret_fun_arg_n_prim_frame# $
    \case
      RetFun {..} -> do
        assertEqual (tipe info) RET_FUN
        assertEqual retFunType ARG_N
        assertEqual retFunSize 1
        assertFun01Closure 1 =<< getBoxedClosureData retFunFun
        pCs <- mapM getBoxedClosureData retFunPayload
        assertEqual (length pCs) 1
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 26"
  test any_ret_fun_arg_gen_frame# $
    \case
      RetFun {..} -> do
        assertEqual (tipe info) RET_FUN
        assertEqual retFunType ARG_GEN
        assertEqual retFunSize 9
        fc <- getBoxedClosureData retFunFun
        case fc of
          FunClosure {..} -> do
            assertEqual (tipe info) FUN_STATIC
            assertEqual (null dataArgs) True
            -- Darwin seems to have a slightly different layout regarding
            -- function `argGenFun`
            assertEqual (null ptrArgs) (os /= "darwin")
          e -> error $ "Wrong closure type: " ++ show e
        pCs <- mapM getBoxedClosureData retFunPayload
        assertEqual (length pCs) 9
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1 .. 9]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 27"
  testSize any_ret_fun_arg_gen_frame# (3 + 9)
  traceM "Test 28"
  test any_ret_fun_arg_gen_big_frame# $
    \case
      RetFun {..} -> do
        assertEqual (tipe info) RET_FUN
        assertEqual retFunType ARG_GEN_BIG
        assertEqual retFunSize 59
        fc <- getBoxedClosureData retFunFun
        case fc of
          FunClosure {..} -> do
            assertEqual (tipe info) FUN_STATIC
            assertEqual (null dataArgs) True
            assertEqual (null ptrArgs) True
          e -> error $ "Wrong closure type: " ++ show e
        pCs <- mapM getBoxedClosureData retFunPayload
        assertEqual (length pCs) 59
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1 .. 59]
  traceM "Test 29"
  testSize any_ret_fun_arg_gen_big_frame# (3 + 59)
  traceM "Test 30"
  test any_bco_frame# $
    \case
      RetBCO {..} -> do
        assertEqual (tipe info) RET_BCO
        pCs <- mapM getBoxedClosureData bcoArgs
        assertEqual (length pCs) 1
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [3]
        bco <- getBoxedClosureData bco
        case bco of
          BCOClosure {..} -> do
            assertEqual (tipe info) BCO
            assertEqual arity 3
            assertEqual size 7
            assertArrWordsClosure [1] =<< getBoxedClosureData instrs
            assertArrWordsClosure [2] =<< getBoxedClosureData literals
            assertMutArrClosure [3] =<< getBoxedClosureData bcoptrs
            assertEqual
              [ 1, -- StgLargeBitmap size in words
                0 -- StgLargeBitmap first words
              ]
              bitmap
          e -> error $ "Wrong closure type: " ++ show e
      e -> error $ "Wrong closure type: " ++ show e
  traceM "Test 31"
  testSize any_bco_frame# 3
  traceM "Test 32"
  test any_underflow_frame# $
    \case
      UnderflowFrame {..} -> do
        assertEqual (tipe info) UNDERFLOW_FRAME
        nextStack <- getBoxedClosureData nextChunk
        case nextStack of
          StackClosure {..} -> do
            assertEqual (tipe info) STACK
            assertEqual stack_size 27
            assertEqual stack_dirty 0
            assertEqual stack_marking 0
            nextStackClosures <- mapM getBoxedClosureData stack
            assertEqual (length nextStackClosures) 2
            case head nextStackClosures of
              RetSmall {..} ->
                assertEqual (tipe info) RET_SMALL
              e -> error $ "Wrong closure type: " ++ show e
            case last nextStackClosures of
              StopFrame {..} ->
                assertEqual (tipe info) STOP_FRAME
              e -> error $ "Wrong closure type: " ++ show e
          e -> error $ "Wrong closure type: " ++ show e
      e -> error $ "Wrong closure type: " ++ show e
  testSize any_underflow_frame# 2

type SetupFunction = State# RealWorld -> (# State# RealWorld, StackSnapshot# #)

test :: HasCallStack => SetupFunction -> (Closure -> IO ()) -> IO ()
test setup assertion = do
  sn@(StackSnapshot sn#) <- getStackSnapshot setup
  performGC
  traceM $ "entertainGC - " ++ entertainGC 100
  -- Run garbage collection now, to prevent later surprises: It's hard to debug
  -- when the GC suddenly does it's work and there were bad closures or pointers.
  -- Better fail early, here.
  performGC
  stackClosure <- getClosureData sn#
  performGC
  let boxedFrames = stack stackClosure
  stack <- mapM getBoxedClosureData boxedFrames
  performGC
  assert sn stack
  -- The result of HasHeapRep should be similar (wrapped in the closure for
  -- StgStack itself.)
  let (StackSnapshot sn#) = sn
  stack' <- getClosureData sn#
  case stack' of
    StackClosure {..} -> do
      !cs <- mapM getBoxedClosureData stack
      assert sn cs
    _ -> error $ "Unexpected closure type : " ++ show stack'
  where
    assert :: StackSnapshot -> [Closure] -> IO ()
    assert sn stack = do
      assertStackInvariants sn stack
      assertEqual (length stack) 2
      assertion $ head stack

entertainGC :: Int -> String
entertainGC 0 = "0"
entertainGC x = show x ++ entertainGC (x - 1)

testSize :: HasCallStack => SetupFunction -> Int -> IO ()
testSize setup expectedSize = do
  (StackSnapshot sn#) <- getStackSnapshot setup
  stackClosure <- getClosureData sn#
  assertEqual expectedSize =<< (closureSize . head . stack) stackClosure

-- | Get a `StackSnapshot` from test setup
--
-- This function mostly resembles `cloneStack`. Though, it doesn't clone, but
-- just pulls a @StgStack@ from RTS to Haskell land.
getStackSnapshot :: SetupFunction -> IO StackSnapshot
getStackSnapshot action# = IO $ \s ->
  case action# s of (# s1, stack #) -> (# s1, StackSnapshot stack #)

assertConstrClosure :: HasCallStack => Word -> Closure -> IO ()
assertConstrClosure w c = case c of
  ConstrClosure {..} -> do
    assertEqual (tipe info) CONSTR_0_1
    assertEqual dataArgs [w]
    assertEqual (null ptrArgs) True
  e -> error $ "Wrong closure type: " ++ show e

assertArrWordsClosure :: HasCallStack => [Word] -> Closure -> IO ()
assertArrWordsClosure wds c = case c of
  ArrWordsClosure {..} -> do
    assertEqual (tipe info) ARR_WORDS
    assertEqual arrWords wds
  e -> error $ "Wrong closure type: " ++ show e

assertMutArrClosure :: HasCallStack => [Word] -> Closure -> IO ()
assertMutArrClosure wds c = case c of
  MutArrClosure {..} -> do
    assertEqual (tipe info) MUT_ARR_PTRS_FROZEN_CLEAN
    xs <- mapM getBoxedClosureData mccPayload
    assertEqual wds $ map getWordFromConstr01 xs
  e -> error $ "Wrong closure type: " ++ show e

assertFun01Closure :: HasCallStack => Word -> Closure -> IO ()
assertFun01Closure w c = case c of
  FunClosure {..} -> do
    assertEqual (tipe info) FUN_0_1
    assertEqual dataArgs [w]
    assertEqual (null ptrArgs) True
  e -> error $ "Wrong closure type: " ++ show e

getWordFromConstr01 :: HasCallStack => Closure -> Word
getWordFromConstr01 c = case c of
  ConstrClosure {..} -> head dataArgs
  e -> error $ "Wrong closure type: " ++ show e

getWordFromBlackhole :: HasCallStack => Closure -> IO Word
getWordFromBlackhole c = case c of
  BlackholeClosure {..} -> getWordFromConstr01 <$> getBoxedClosureData indirectee
  -- For test stability reasons: Expect that the blackhole might have been
  -- resolved.
  ConstrClosure {..} -> pure $ head dataArgs
  e -> error $ "Wrong closure type: " ++ show e

getWordFromUnknownTypeWordSizedPrimitive :: HasCallStack => Closure -> Word
getWordFromUnknownTypeWordSizedPrimitive c = case c of
  UnknownTypeWordSizedPrimitive {..} -> wordVal
  e -> error $ "Wrong closure type: " ++ show e

assertUnknownTypeWordSizedPrimitive :: HasCallStack => Word -> Closure -> IO ()
assertUnknownTypeWordSizedPrimitive w c = case c of
  UnknownTypeWordSizedPrimitive {..} -> do
    assertEqual wordVal w
  e -> error $ "Wrong closure type: " ++ show e

unboxSingletonTuple :: (# StackSnapshot# #) -> StackSnapshot#
unboxSingletonTuple (# s# #) = s#

minBigBitmapBits :: Num a => a
minBigBitmapBits = 1 + maxSmallBitmapBits

maxSmallBitmapBits :: Num a => a
maxSmallBitmapBits = fromIntegral maxSmallBitmapBits_c

-- | A function with 59 arguments
--
-- A small bitmap has @64 - 6 = 58@ entries on 64bit machines. On 32bit machines
-- it's less (for obvious reasons.) I.e. this function's bitmap a large one;
-- function type is @ARG_GEN_BIG@.
{-# NOINLINE argGenBigFun #-}
argGenBigFun ::
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word
argGenBigFun a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 =
  a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 + a36 + a37 + a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47 + a48 + a49 + a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 + a59

-- | A function with more arguments than the pre-generated (@ARG_PPPPPPPP -> 8@) ones
-- have
--
-- This results in a @ARG_GEN@ function (the number of arguments still fits in a
-- small bitmap).
{-# NOINLINE argGenFun #-}
argGenFun ::
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word ->
  Word
argGenFun a1 a2 a3 a4 a5 a6 a7 a8 a9 = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9
