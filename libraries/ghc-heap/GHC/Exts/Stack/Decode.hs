{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,7,0)
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.Exts.Stack.Decode
  ( decodeStack,
  )
where

import Control.Monad
import Data.Bits
import Data.Maybe
import Foreign
import GHC.Exts
import GHC.Exts.Heap (Box (..), getBoxedClosureData)
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
  ( Closure,
    GenClosure (UnknownTypeWordSizedPrimitive),
    RetFunType (..),
    StackFrame (..),
    StgStackClosure (..),
  )
import GHC.Exts.Heap.Constants (wORD_SIZE_IN_BITS)
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Stack.Constants
import GHC.IO (IO (..))
import GHC.Stack.CloneStack
import GHC.Word
import Prelude

{- Note [Decoding the stack]
   ~~~~~~~~~~~~~~~~~~~~~~~~~

The stack is represented by a chain of StgStack closures. Each of these closures
is subject to garbage collection. I.e. they can be moved in memory (in a
simplified perspective) at any time.

The array of closures inside an StgStack (that makeup the execution stack; the
stack frames) is moved as bare memory by the garbage collector. References
(pointers) to stack frames are not updated by the garbage collector.

As the StgStack closure is moved as whole, the relative offsets inside it stay
the same. (Though, the absolute addresses change!)

Decoding
====================

Stack frames are defined by their `StackSnapshot#` (`StgStack*` in RTS) and
their relative offset. This tuple is described by `StackFrameLocation`.

`StackFrame` is an ADT for decoded stack frames. Where it points to heap located
closures or primitive Words (in bitmap encoded payloads), `Closure` is used to
describe the referenced payload.

The decoding happens in two phases:

1. The whole stack is decoded into `StackFrameLocation`s.

2. All `StackFrameLocation`s are decoded into `StackFrame`s which have
`Closure`s as fields/references.

`StackSnapshot#` parameters are updated by the garbage collector and thus safe
to hand around.

The head of the stack frame array has offset (index) 0. To traverse the stack
frames the latest stack frame's offset is incremented by the closure size. The
unit of the offset is machine words (32bit or 64bit.)

Boxes
=====

`Closure` makes extensive usage of `Box`es. Unfortunately, we cannot simply apply the
same here:

- Bitmap encoded payloads can be either words or closure pointers.

- Underflow frames point to `StgStack` closures.

These three cases are hard to encode in boxes. Additionally, introducing new box
types would break existing box usages. Thus, the stack is decoded unboxed, while
the referenced `Closure`s use boxes. This seems to be a good compromise between
optimization (with boxes) and simplicity (by leaving out the mentioned special
cases.)

IO
==

Unfortunately, ghc-heap decodes `Closure`s in `IO`. This leads to `StackFrames`
also being decoded in `IO`, due to references to `Closure`s.

Technical details
=================

- All access to StgStack/StackSnapshot# closures is made through Cmm code. This
  keeps the closure from being moved by the garbage collector during the
  operation.

- As StgStacks are mainly used in Cmm and C code, much of the decoding logic is
  implemented in Cmm and C. It's just easier to reuse existing helper macros and
  functions, than reinventing them in Haskell.

- Offsets and sizes of closures are imported from DerivedConstants.h via HSC.
  This keeps the code very portable.
-}

foreign import prim "getUnderflowFrameNextChunkzh"
  getUnderflowFrameNextChunk# ::
    StackSnapshot# -> Word# -> StackSnapshot#

getUnderflowFrameNextChunk :: StackSnapshot# -> WordOffset -> StackSnapshot
getUnderflowFrameNextChunk stackSnapshot# index =
  StackSnapshot (getUnderflowFrameNextChunk# stackSnapshot# (wordOffsetToWord# index))

foreign import prim "getWordzh"
  getWord# ::
    StackSnapshot# -> Word# -> Word#

getWord :: StackSnapshot# -> WordOffset -> Word
getWord stackSnapshot# index =
  W# (getWord# stackSnapshot# (wordOffsetToWord# index))

foreign import prim "getRetFunTypezh" getRetFunType# :: StackSnapshot# -> Word# -> Word#

getRetFunType :: StackSnapshot# -> WordOffset -> RetFunType
getRetFunType stackSnapshot# index =
  toEnum . fromInteger . toInteger $
    W# (getRetFunType# stackSnapshot# (wordOffsetToWord# index))

-- | Gets contents of a `LargeBitmap` (@StgLargeBitmap@)
--
-- The first two arguments identify the location of the frame on the stack.
-- Returned is the `Addr#` of the @StgWord[]@ (bitmap) and it's size.
type LargeBitmapGetter = StackSnapshot# -> Word# -> (# Addr#, Word# #)

foreign import prim "getLargeBitmapzh" getLargeBitmap# :: LargeBitmapGetter

foreign import prim "getBCOLargeBitmapzh" getBCOLargeBitmap# :: LargeBitmapGetter

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: LargeBitmapGetter

-- | Gets contents of a small bitmap (fitting in one @StgWord@)
--
-- The first two arguments identify the location of the frame on the stack.
-- Returned is the bitmap and it's size.
type SmallBitmapGetter = StackSnapshot# -> Word# -> (# Word#, Word# #)

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: SmallBitmapGetter

foreign import prim "getRetFunSmallBitmapzh" getRetFunSmallBitmap# :: SmallBitmapGetter

foreign import prim "getInfoTableAddrzh" getInfoTableAddr# :: StackSnapshot# -> Word# -> Addr#

foreign import prim "getStackInfoTableAddrzh" getStackInfoTableAddr# :: StackSnapshot# -> Addr#

getInfoTableOnStack :: StackSnapshot# -> WordOffset -> IO StgInfoTable
getInfoTableOnStack stackSnapshot# index =
  let infoTablePtr = Ptr (getInfoTableAddr# stackSnapshot# (wordOffsetToWord# index))
   in peekItbl infoTablePtr

getInfoTableForStack :: StackSnapshot# -> IO StgInfoTable
getInfoTableForStack stackSnapshot# =
  peekItbl $
    Ptr (getStackInfoTableAddr# stackSnapshot#)

foreign import prim "getStackClosurezh"
  getStackClosure# ::
    StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Any #)

foreign import prim "getStackFieldszh"
  getStackFields# ::
    StackSnapshot# -> (# Word32#, Word8#, Word8# #)

getStackFields :: StackSnapshot# -> (Word32, Word8, Word8)
getStackFields stackSnapshot# =
  case getStackFields# stackSnapshot# of
    (# sSize#, sDirty#, sMarking# #) ->
      (W32# sSize#, W8# sDirty#, W8# sMarking#)

-- | `StackFrameLocation` of the top-most stack frame
stackHead :: StackSnapshot# -> StackFrameLocation
stackHead s# = (StackSnapshot s#, 0) -- GHC stacks are never empty

-- | Advance to the next stack frame (if any)
--
-- The last `Int#` in the result tuple is meant to be treated as bool
-- (has_next).
foreign import prim "advanceStackFrameLocationzh"
  advanceStackFrameLocation# ::
    StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

-- | Advance to the next stack frame (if any)
advanceStackFrameLocation :: StackFrameLocation -> Maybe StackFrameLocation
advanceStackFrameLocation ((StackSnapshot stackSnapshot#), index) =
  let !(# s', i', hasNext #) = advanceStackFrameLocation# stackSnapshot# (wordOffsetToWord# index)
   in if I# hasNext > 0
        then Just (StackSnapshot s', primWordToWordOffset i')
        else Nothing
  where
    primWordToWordOffset :: Word# -> WordOffset
    primWordToWordOffset w# = fromIntegral (W# w#)

getClosure :: StackSnapshot# -> WordOffset -> IO Closure
getClosure stackSnapshot# index =
  -- Beware! We have to put ptr into a Box immediately. Otherwise, the garbage
  -- collector might move the referenced closure, without updating our reference
  -- (pointer) to it.
  ( IO $ \s ->
      case getStackClosure#
        stackSnapshot#
        (wordOffsetToWord# index)
        s of
        (# s1, ptr #) ->
          (# s1, Box ptr #)
  )
    >>= getBoxedClosureData

-- | Representation of @StgLargeBitmap@ (RTS)
data LargeBitmap = LargeBitmap
  { largeBitmapSize :: Word,
    largebitmapWords :: Ptr Word
  }

-- | Is a bitmap entry a closure pointer or a primitive non-pointer?
data Pointerness = Pointer | NonPointer
  deriving (Show)

decodeLargeBitmap :: LargeBitmapGetter -> StackSnapshot# -> WordOffset -> WordOffset -> IO [Closure]
decodeLargeBitmap getterFun# stackSnapshot# index relativePayloadOffset = do
  let largeBitmap = case getterFun# stackSnapshot# (wordOffsetToWord# index) of
        (# wordsAddr#, size# #) -> LargeBitmap (W# size#) (Ptr wordsAddr#)
  bitmapWords <- largeBitmapToList largeBitmap
  decodeBitmaps
    stackSnapshot#
    (index + relativePayloadOffset)
    (bitmapWordsPointerness (largeBitmapSize largeBitmap) bitmapWords)
  where
    largeBitmapToList :: LargeBitmap -> IO [Word]
    largeBitmapToList LargeBitmap {..} =
      cWordArrayToList largebitmapWords $
        (usedBitmapWords . fromIntegral) largeBitmapSize

    cWordArrayToList :: Ptr Word -> Int -> IO [Word]
    cWordArrayToList ptr size = mapM (peekElemOff ptr) [0 .. (size - 1)]

    usedBitmapWords :: Int -> Int
    usedBitmapWords 0 = error "Invalid large bitmap size 0."
    usedBitmapWords size = (size `div` fromIntegral wORD_SIZE_IN_BITS) + 1

    bitmapWordsPointerness :: Word -> [Word] -> [Pointerness]
    bitmapWordsPointerness size _ | size <= 0 = []
    bitmapWordsPointerness _ [] = []
    bitmapWordsPointerness size (w : wds) =
      bitmapWordPointerness (min size (fromIntegral wORD_SIZE_IN_BITS)) w
        ++ bitmapWordsPointerness (size - fromIntegral wORD_SIZE_IN_BITS) wds

bitmapWordPointerness :: Word -> Word -> [Pointerness]
bitmapWordPointerness 0 _ = []
bitmapWordPointerness bSize bitmapWord =
  ( if (bitmapWord .&. 1) /= 0
      then NonPointer
      else Pointer
  )
    : bitmapWordPointerness
      (bSize - 1)
      (bitmapWord `shiftR` 1)

decodeBitmaps :: StackSnapshot# -> WordOffset -> [Pointerness] -> IO [Closure]
decodeBitmaps stack# index ps =
  zipWithM toPayload ps [index ..]
  where
    toPayload :: Pointerness -> WordOffset -> IO Closure
    toPayload p i = case p of
      NonPointer ->
        pure $ UnknownTypeWordSizedPrimitive (getWord stack# i)
      Pointer -> getClosure stack# i

decodeSmallBitmap :: SmallBitmapGetter -> StackSnapshot# -> WordOffset -> WordOffset -> IO [Closure]
decodeSmallBitmap getterFun# stackSnapshot# index relativePayloadOffset =
  let (bitmap, size) = case getterFun# stackSnapshot# (wordOffsetToWord# index) of
        (# b#, s# #) -> (W# b#, W# s#)
   in decodeBitmaps
        stackSnapshot#
        (index + relativePayloadOffset)
        (bitmapWordPointerness size bitmap)

unpackStackFrame :: StackFrameLocation -> IO StackFrame
unpackStackFrame (StackSnapshot stackSnapshot#, index) = do
  info <- getInfoTableOnStack stackSnapshot# index
  unpackStackFrame' info
  where
    unpackStackFrame' :: StgInfoTable -> IO StackFrame
    unpackStackFrame' info =
      case tipe info of
        RET_BCO -> do
          bco' <- getClosure stackSnapshot# (index + offsetStgClosurePayload)
          -- The arguments begin directly after the payload's one element
          bcoArgs' <- decodeLargeBitmap getBCOLargeBitmap# stackSnapshot# index (offsetStgClosurePayload + 1)
          pure
            RetBCO
              { info_tbl = info,
                bco = bco',
                bcoArgs = bcoArgs'
              }
        RET_SMALL -> do
          payload' <- decodeSmallBitmap getSmallBitmap# stackSnapshot# index offsetStgClosurePayload
          pure $
            RetSmall
              { info_tbl = info,
                stack_payload = payload'
              }
        RET_BIG -> do
          payload' <- decodeLargeBitmap getLargeBitmap# stackSnapshot# index offsetStgClosurePayload
          pure $
            RetBig
              { info_tbl = info,
                stack_payload = payload'
              }
        RET_FUN -> do
          let retFunType' = getRetFunType stackSnapshot# index
              retFunSize' = getWord stackSnapshot# (index + offsetStgRetFunFrameSize)
          retFunFun' <- getClosure stackSnapshot# (index + offsetStgRetFunFrameFun)
          retFunPayload' <-
            if retFunType' == ARG_GEN_BIG
              then decodeLargeBitmap getRetFunLargeBitmap# stackSnapshot# index offsetStgRetFunFramePayload
              else decodeSmallBitmap getRetFunSmallBitmap# stackSnapshot# index offsetStgRetFunFramePayload
          pure $
            RetFun
              { info_tbl = info,
                retFunType = retFunType',
                retFunSize = retFunSize',
                retFunFun = retFunFun',
                retFunPayload = retFunPayload'
              }
        UPDATE_FRAME -> do
          updatee' <- getClosure stackSnapshot# (index + offsetStgUpdateFrameUpdatee)
          pure $
            UpdateFrame
              { info_tbl = info,
                updatee = updatee'
              }
        CATCH_FRAME -> do
          let exceptions_blocked' = getWord stackSnapshot# (index + offsetStgCatchFrameExceptionsBlocked)
          handler' <- getClosure stackSnapshot# (index + offsetStgCatchFrameHandler)
          pure $
            CatchFrame
              { info_tbl = info,
                exceptions_blocked = exceptions_blocked',
                handler = handler'
              }
        UNDERFLOW_FRAME -> do
          let nextChunk' = getUnderflowFrameNextChunk stackSnapshot# index
          stackClosure <- decodeStack nextChunk'
          pure $
            UnderflowFrame
              { info_tbl = info,
                nextChunk = stackClosure
              }
        STOP_FRAME -> pure $ StopFrame {info_tbl = info}
        ATOMICALLY_FRAME -> do
          atomicallyFrameCode' <- getClosure stackSnapshot# (index + offsetStgAtomicallyFrameCode)
          result' <- getClosure stackSnapshot# (index + offsetStgAtomicallyFrameResult)
          pure $
            AtomicallyFrame
              { info_tbl = info,
                atomicallyFrameCode = atomicallyFrameCode',
                result = result'
              }
        CATCH_RETRY_FRAME -> do
          let running_alt_code' = getWord stackSnapshot# (index + offsetStgCatchRetryFrameRunningAltCode)
          first_code' <- getClosure stackSnapshot# (index + offsetStgCatchRetryFrameRunningFirstCode)
          alt_code' <- getClosure stackSnapshot# (index + offsetStgCatchRetryFrameAltCode)
          pure $
            CatchRetryFrame
              { info_tbl = info,
                running_alt_code = running_alt_code',
                first_code = first_code',
                alt_code = alt_code'
              }
        CATCH_STM_FRAME -> do
          catchFrameCode' <- getClosure stackSnapshot# (index + offsetStgCatchSTMFrameCode)
          handler' <- getClosure stackSnapshot# (index + offsetStgCatchSTMFrameHandler)
          pure $
            CatchStmFrame
              { info_tbl = info,
                catchFrameCode = catchFrameCode',
                handler = handler'
              }
        x -> error $ "Unexpected closure type on stack: " ++ show x

-- | Unbox 'Int#' from 'Int'
toInt# :: Int -> Int#
toInt# (I# i) = i

-- | Convert `Int` to `Word#`
intToWord# :: Int -> Word#
intToWord# i = int2Word# (toInt# i)

wordOffsetToWord# :: WordOffset -> Word#
wordOffsetToWord# wo = intToWord# (fromIntegral wo)

-- | Location of a stackframe on the stack
--
-- It's defined by the `StackSnapshot` (@StgStack@) and the offset to the bottom
-- of the stack.
type StackFrameLocation = (StackSnapshot, WordOffset)

-- | Decode `StackSnapshot` to a `StgStackClosure`
--
-- The return value is the representation of the @StgStack@ itself.
--
-- See /Note [Decoding the stack]/.
decodeStack :: StackSnapshot -> IO StgStackClosure
decodeStack (StackSnapshot stack#) = do
  info <- getInfoTableForStack stack#
  case tipe info of
    STACK -> do
      let (stack_size', stack_dirty', stack_marking') = getStackFields stack#
          sfls = stackFrameLocations stack#
      stack' <- mapM unpackStackFrame sfls
      pure $
        StgStackClosure
          { ssc_info = info,
            ssc_stack_size = stack_size',
            ssc_stack_dirty = stack_dirty',
            ssc_stack_marking = stack_marking',
            ssc_stack = stack'
          }
    _ -> error $ "Expected STACK closure, got " ++ show info
  where
    stackFrameLocations :: StackSnapshot# -> [StackFrameLocation]
    stackFrameLocations s# =
      stackHead s#
        : go (advanceStackFrameLocation (stackHead s#))
      where
        go :: Maybe StackFrameLocation -> [StackFrameLocation]
        go Nothing = []
        go (Just r) = r : go (advanceStackFrameLocation r)

#else
module GHC.Exts.Stack.Decode where
#endif
