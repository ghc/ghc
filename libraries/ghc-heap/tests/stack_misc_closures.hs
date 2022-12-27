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
import GHC.Exts.Heap (GenClosure(wordVal))
import System.Mem
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

foreign import prim "any_ret_big_prims_framezh" any_ret_big_prims_frame# :: SetupFunction

foreign import prim "any_ret_big_prim_framezh" any_ret_big_prim_frame# :: SetupFunction

foreign import prim "any_ret_big_closures_framezh" any_ret_big_closures_frame# :: SetupFunction

foreign import ccall "maxSmallBitmapBits" maxSmallBitmapBits_c :: Word

foreign import ccall "belchStack" belchStack# :: StackSnapshot# -> IO ()

main :: HasCallStack => IO ()
main = do
  traceM "test any_update_frame#"
  test any_update_frame# 42## $
    \case
      UpdateFrame {..} -> do
        assertEqual knownUpdateFrameType NormalUpdateFrame
        assertEqual 42 =<< (getWordFromBlackhole =<< getBoxedClosureData updatee)
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_catch_frame#"
  test any_catch_frame# 43## $
    \case
      CatchFrame {..} -> do
        assertEqual exceptions_blocked 1
        assertConstrClosure 43 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_catch_stm_frame#"
  test any_catch_stm_frame# 44## $
    \case
      CatchStmFrame {..} -> do
        assertConstrClosure 44 =<< getBoxedClosureData catchFrameCode
        assertConstrClosure 45 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_catch_retry_frame#"
  test any_catch_retry_frame# 46## $
    \case
      CatchRetryFrame {..} -> do
        assertEqual running_alt_code 1
        assertConstrClosure 46 =<< getBoxedClosureData first_code
        assertConstrClosure 47 =<< getBoxedClosureData alt_code
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_atomically_frame#"
  test any_atomically_frame# 48## $
    \case
      AtomicallyFrame {..} -> do
        assertConstrClosure 48 =<< getBoxedClosureData atomicallyFrameCode
        assertConstrClosure 49 =<< getBoxedClosureData result
      e -> error $ "Wrong closure type: " ++ show e
  -- TODO: Test for UnderflowFrame once it points to a Box payload
  traceM "test any_ret_small_prim_frame#"
  test any_ret_small_prim_frame# 50## $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType RetN
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertUnknownTypeWordSizedPrimitive 50 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_ret_small_closure_frame#"
  test any_ret_small_closure_frame# 51## $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType RetP
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertConstrClosure 51 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_ret_small_closures_frame#"
  test any_ret_small_closures_frame# 1## $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType None
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) (fromIntegral maxSmallBitmapBits_c)
        assertConstrClosure 1 (head pCs)
        assertConstrClosure 58 (last pCs)
        let wds = map getWordFromConstr01 pCs
        assertEqual wds [1..58]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_ret_small_prims_frame#"
  test any_ret_small_prims_frame# 1## $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType None
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) (fromIntegral maxSmallBitmapBits_c)
        assertUnknownTypeWordSizedPrimitive 1 (head pCs)
        assertUnknownTypeWordSizedPrimitive 58 (last pCs)
        let wds = map getWordFromUnknownTypeWordSizedPrimitive pCs
        assertEqual wds [1..58]
      e -> error $ "Wrong closure type: " ++ show e
  traceM "test any_ret_big_prim_frame#"
  test any_ret_big_prim_frame# 52## $
    \case
      RetBig {..} -> do
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 59
        assertUnknownTypeWordSizedPrimitive 52 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e

type SetupFunction = Word# -> State# RealWorld -> (# State# RealWorld, StackSnapshot# #)

test :: HasCallStack => SetupFunction -> Word# -> (Closure -> IO ()) -> IO ()
test setup w assertion = do
  sn <- getStackSnapshot setup w
  performGC
  stack <- decodeStack' sn
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

getStackSnapshot :: SetupFunction -> Word# -> IO StackSnapshot
getStackSnapshot action# w# = IO $ \s ->
   case action# w# s of (# s1, stack #) -> (# s1, StackSnapshot stack #)

assertConstrClosure :: HasCallStack => Word -> Closure -> IO ()
assertConstrClosure w c = case c of
  ConstrClosure {..} -> do
    assertEqual (tipe info) CONSTR_0_1
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
