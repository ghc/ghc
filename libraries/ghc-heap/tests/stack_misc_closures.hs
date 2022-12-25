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
import GHC.Stack.CloneStack (StackSnapshot (..))
import TestUtils
import Unsafe.Coerce (unsafeCoerce)
import GHC.Stack (HasCallStack)

foreign import prim "any_update_framezh" any_update_frame# :: Word# -> (# StackSnapshot# #)

main :: HasCallStack => IO ()
main = do
  let sn = StackSnapshot (unboxSingletonTuple (any_update_frame# 42##))
  stack <- decodeStack' sn
  assertStackInvariants sn stack
  assertEqual (length stack) 2

  let updateFrame = head stack
  print $ "updateFrame : " ++ show updateFrame
  case updateFrame of
    UpdateFrame {..} -> do
      assertEqual knownUpdateFrameType NormalUpdateFrame
      u <- getBoxedClosureData updatee
      case u of
        ConstrClosure {..} -> do
          assertEqual (tipe info) CONSTR_0_1
          assertEqual dataArgs [42]
          assertEqual (null ptrArgs) True
        _ -> error $ "Wrong closure type: " ++ show u
    _ -> error $ "Wrong closure type: " ++ show updateFrame
  assertThat
    "Last frame is stop frame"
    ( \case
        StopFrame -> True
        _ -> False
    )
    (last stack)

unboxSingletonTuple :: (# StackSnapshot# #) -> StackSnapshot#
unboxSingletonTuple (# s# #) = s#
