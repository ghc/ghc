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

import GHC.Exts
import GHC.Exts.DecodeStack
import GHC.Exts.Heap
import GHC.Exts.Heap.Closures
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack (StackSnapshot (..))
import TestUtils
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts.Heap (GenClosure(wordVal), HasHeapRep (getClosureData))
import System.Mem
--TODO: Remove later
import Debug.Trace
import GHC.IO (IO (..))

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

foreign import prim "any_ret_fun_arg_n_prim_framezh" any_ret_fun_arg_n_prim_framezh# :: SetupFunction

foreign import ccall "maxSmallBitmapBits" maxSmallBitmapBits_c :: Word

foreign import ccall "belchStack" belchStack# :: StackSnapshot# -> IO ()

{-
__Test stategy:__

- Create @StgStack@s in C that contain two closures (as they are on stack they
may also be called "frames"). A stop frame and the frame which's decoding should
be tested.

- Cmm primops are used to get `StackSnapshot#` values. (This detour ensures that
the closures are referenced by `StackSnapshot#` and not garbage collected right
away.)

- These can then be decoded and checked.

This strategy may look pretty complex for a test. But, it can provide very
specific corner cases that would be hard to (reliably!) produce in Haskell.

N.B. `StackSnapshots` are managed by the garbage collector. This isn't much of
an issue regarding the test data, as it's already very terse. However, it's
important to know that the GC may rewrite parts of the stack and that the stack
must be sound (otherwise, the GC may fail badly.)

The decission to make `StackSnapshots`s (and their closures) being managed by the
GC isn't accidential. It's closer to the reality of decoding stacks.

N.B. the test data stack are only meant be de decoded. They are not executable
(the result would likely be a crash or non-sense.)
-}
main :: HasCallStack => IO ()
main = do
  test any_update_frame# $
    \case
      UpdateFrame {..} -> do
        assertEqual knownUpdateFrameType NormalUpdateFrame
        assertEqual 1 =<< (getWordFromBlackhole =<< getBoxedClosureData updatee)
      e -> error $ "Wrong closure type: " ++ show e
  test any_catch_frame# $
    \case
      CatchFrame {..} -> do
        assertEqual exceptions_blocked 1
        assertConstrClosure 1 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  test any_catch_stm_frame# $
    \case
      CatchStmFrame {..} -> do
        assertConstrClosure 1 =<< getBoxedClosureData catchFrameCode
        assertConstrClosure 2 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  test any_catch_retry_frame# $
    \case
      CatchRetryFrame {..} -> do
        assertEqual running_alt_code 1
        assertConstrClosure 1 =<< getBoxedClosureData first_code
        assertConstrClosure 2 =<< getBoxedClosureData alt_code
      e -> error $ "Wrong closure type: " ++ show e
  test any_atomically_frame# $
    \case
      AtomicallyFrame {..} -> do
        assertConstrClosure 1 =<< getBoxedClosureData atomicallyFrameCode
        assertConstrClosure 2 =<< getBoxedClosureData result
      e -> error $ "Wrong closure type: " ++ show e
  -- TODO: Test for UnderflowFrame once it points to a Box payload
  test any_ret_small_prim_frame# $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType RetN
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertUnknownTypeWordSizedPrimitive 1 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_small_closure_frame# $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType RetP
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertConstrClosure 1 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_small_closures_frame# $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType None
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) (fromIntegral maxSmallBitmapBits_c)
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1..58]
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_small_prims_frame# $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType None
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) (fromIntegral maxSmallBitmapBits_c)
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1..58]
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_big_prims_min_frame# $
    \case
      RetBig {..} -> do
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 59
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1..59]
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_big_prims_min_frame# $
    \case
      RetBig {..} -> do
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 59
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1..59]
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_big_closures_min_frame# $
    \case
      RetBig {..} -> do
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 59
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1..59]
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_big_closures_two_words_frame# $
    \case
      RetBig {..} -> do
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 65
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1..65]
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_fun_arg_n_prim_framezh# $
    \case
      RetFun {..} -> do
        assertEqual retFunType ARG_N
        assertEqual retFunSize 1
        assertFun01Closure 1 =<< getBoxedClosureData retFunFun
        pCs <- mapM getBoxedClosureData retFunPayload
        assertEqual (length pCs) 1
        let wds = map  getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1]
      e -> error $ "Wrong closure type: " ++ show e

type SetupFunction = State# RealWorld -> (# State# RealWorld, StackSnapshot# #)

test :: HasCallStack => SetupFunction -> (Closure -> IO ()) -> IO ()
test setup assertion = do
    sn <- getStackSnapshot setup
    -- Run garbage collection now, to prevent later surprises: It's hard to debug
    -- when the GC suddenly does it's work and there were bad closures or pointers.
    -- Better fail early, here.
    performGC
    stack <- decodeStack' sn
    assert sn stack
    -- The result of HasHeapRep should be similar (wrapped in the closure for
    -- StgStack itself.)
    let (StackSnapshot sn#) = sn
    stack' <- getClosureData sn#
    case stack' of
      SimpleStack {..} -> do
        !cs <- mapM getBoxedClosureData stackClosures
        assert sn cs
      _ -> error $ "Unexpected closure type : " ++ show stack'
  where
    assert :: StackSnapshot -> [Closure] -> IO ()
    assert sn stack = do
      assertStackInvariants sn stack
      assertEqual (length stack) 2
      assertThat
        "Last frame is stop frame"
        ( \case
            StopFrame -> True
            _ -> False
        )
        (last stack)
      assertion $ head stack

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
