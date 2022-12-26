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

foreign import prim "any_update_framezh" any_update_frame# :: Word# -> (# StackSnapshot# #)

foreign import prim "any_catch_framezh" any_catch_frame# :: Word# -> (# StackSnapshot# #)

foreign import prim "any_catch_stm_framezh" any_catch_stm_frame# :: Word# -> (# StackSnapshot# #)

foreign import prim "any_catch_retry_framezh" any_catch_retry_frame# :: Word# -> (# StackSnapshot# #)

foreign import prim "any_atomically_framezh" any_atomically_frame# :: Word# -> (# StackSnapshot# #)

foreign import prim "any_ret_small_prim_framezh" any_ret_small_prim_frame# :: Word# -> (# StackSnapshot# #)

foreign import prim "any_ret_small_closure_framezh" any_ret_small_closure_frame# :: Word# -> (# StackSnapshot# #)

main :: HasCallStack => IO ()
main = do
  test any_update_frame# 42## $
    \case
      UpdateFrame {..} -> do
        assertEqual knownUpdateFrameType NormalUpdateFrame
        assertConstrClosure 42 =<< getBoxedClosureData updatee
      e -> error $ "Wrong closure type: " ++ show e
  test any_catch_frame# 43## $
    \case
      CatchFrame {..} -> do
        assertEqual exceptions_blocked 1
        assertConstrClosure 43 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  test any_catch_stm_frame# 44## $
    \case
      CatchStmFrame {..} -> do
        assertConstrClosure 44 =<< getBoxedClosureData catchFrameCode
        assertConstrClosure 45 =<< getBoxedClosureData handler
      e -> error $ "Wrong closure type: " ++ show e
  test any_catch_retry_frame# 46## $
    \case
      CatchRetryFrame {..} -> do
        assertEqual running_alt_code 1
        assertConstrClosure 46 =<< getBoxedClosureData first_code
        assertConstrClosure 47 =<< getBoxedClosureData alt_code
      e -> error $ "Wrong closure type: " ++ show e
  test any_atomically_frame# 48## $
    \case
      AtomicallyFrame {..} -> do
        assertConstrClosure 48 =<< getBoxedClosureData atomicallyFrameCode
        assertConstrClosure 49 =<< getBoxedClosureData result
      e -> error $ "Wrong closure type: " ++ show e
  -- TODO: Test for UnderflowFrame once it points to a Box payload
  test any_ret_small_prim_frame# 50## $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType RetN
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertUnknownTypeWordSizedPrimitive 50 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e
  test any_ret_small_closure_frame# 51## $
    \case
      RetSmall {..} -> do
        assertEqual knownRetSmallType RetP
        pCs <- mapM getBoxedClosureData payload
        assertEqual (length pCs) 1
        assertConstrClosure 51 (head pCs)
      e -> error $ "Wrong closure type: " ++ show e


test :: HasCallStack => (Word# -> (# StackSnapshot# #)) -> Word# -> (Closure -> IO ()) -> IO ()
test setup w assertion = do
  let sn = StackSnapshot (unboxSingletonTuple (setup w))
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

assertConstrClosure :: HasCallStack => Word -> Closure -> IO ()
assertConstrClosure w c = case c of
  ConstrClosure {..} -> do
    assertEqual (tipe info) CONSTR_0_1
    assertEqual dataArgs [w]
    assertEqual (null ptrArgs) True
  e -> error $ "Wrong closure type: " ++ show e

assertUnknownTypeWordSizedPrimitive :: HasCallStack => Word -> Closure -> IO ()
assertUnknownTypeWordSizedPrimitive w c = case c of
  UnknownTypeWordSizedPrimitive {..} -> do
    assertEqual wordVal w
  e -> error $ "Wrong closure type: " ++ show e

unboxSingletonTuple :: (# StackSnapshot# #) -> StackSnapshot#
unboxSingletonTuple (# s# #) = s#
