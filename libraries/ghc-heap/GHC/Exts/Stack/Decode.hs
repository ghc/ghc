{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,13,0)
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
import GHC.Exts.Heap (Box (..))
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
  ( StackFrame,
    GenStackFrame (..),
    StgStackClosure,
    GenStgStackClosure (..),
    StackField,
    GenStackField(..)
  )
import GHC.Exts.Heap.Constants (wORD_SIZE_IN_BITS)
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Stack.Constants
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
========

Stack frames are defined by their `StackSnapshot#` (`StgStack*` in RTS) and
their relative offset. This tuple is described by `StackFrameLocation`.

`StackFrame` is an ADT for decoded stack frames. Regarding payload and fields we
have to deal with three cases:

- If the payload can only be a closure, we put it in a `Box` for later decoding
  by the heap closure functions.

- If the payload can either be a closure or a word-sized value (this happens for
  bitmap-encoded payloads), we use a `StackField` which is a sum type to
  represent either a `Word` or a `Box`.

- Fields that are just simple (i.e. non-closure) values are decoded as such.

The decoding happens in two phases:

1. The whole stack is decoded into `StackFrameLocation`s.

2. All `StackFrameLocation`s are decoded into `StackFrame`s.

`StackSnapshot#` parameters are updated by the garbage collector and thus safe
to hand around.

The head of the stack frame array has offset (index) 0. To traverse the stack
frames the latest stack frame's offset is incremented by the closure size. The
unit of the offset is machine words (32bit or 64bit.)

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

foreign import prim "isArgGenBigRetFunTypezh" isArgGenBigRetFunType# :: StackSnapshot# -> Word# -> Int#

isArgGenBigRetFunType :: StackSnapshot# -> WordOffset -> Bool
isArgGenBigRetFunType stackSnapshot# index =
  I# (isArgGenBigRetFunType# stackSnapshot# (wordOffsetToWord# index)) > 0

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
    StackSnapshot# -> Word# ->  Any

foreign import prim "getStackFieldszh"
  getStackFields# ::
    StackSnapshot# -> Word32#

getStackFields :: StackSnapshot# -> Word32
getStackFields stackSnapshot# =
  case getStackFields# stackSnapshot# of
    sSize# -> W32# sSize#

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

getClosureBox :: StackSnapshot# -> WordOffset -> Box
getClosureBox stackSnapshot# index =
        case getStackClosure# stackSnapshot# (wordOffsetToWord# index) of
          -- c needs to be strictly evaluated, otherwise a thunk gets boxed (and
          -- will later be decoded as such)
          !c -> Box c

-- | Representation of @StgLargeBitmap@ (RTS)
data LargeBitmap = LargeBitmap
  { largeBitmapSize :: Word,
    largebitmapWords :: Ptr Word
  }

-- | Is a bitmap entry a closure pointer or a primitive non-pointer?
data Pointerness = Pointer | NonPointer
  deriving (Show)

decodeLargeBitmap :: LargeBitmapGetter -> StackSnapshot# -> WordOffset -> WordOffset -> IO [StackField]
decodeLargeBitmap getterFun# stackSnapshot# index relativePayloadOffset = do
  let largeBitmap = case getterFun# stackSnapshot# (wordOffsetToWord# index) of
        (# wordsAddr#, size# #) -> LargeBitmap (W# size#) (Ptr wordsAddr#)
  bitmapWords <- largeBitmapToList largeBitmap
  pure $ decodeBitmaps
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

decodeBitmaps :: StackSnapshot# -> WordOffset -> [Pointerness] -> [StackField]
decodeBitmaps stack# index ps =
  zipWith toPayload ps [index ..]
  where
    toPayload :: Pointerness -> WordOffset -> StackField
    toPayload p i = case p of
      NonPointer -> StackWord (getWord stack# i)
      Pointer -> StackBox (getClosureBox stack# i)

decodeSmallBitmap :: SmallBitmapGetter -> StackSnapshot# -> WordOffset -> WordOffset -> [StackField]
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
          let bco' = getClosureBox stackSnapshot# (index + offsetStgClosurePayload)
          -- The arguments begin directly after the payload's one element
          bcoArgs' <- decodeLargeBitmap getBCOLargeBitmap# stackSnapshot# index (offsetStgClosurePayload + 1)
          pure
            RetBCO
              { info_tbl = info,
                bco = bco',
                bcoArgs = bcoArgs'
              }
        RET_SMALL ->
          let payload' = decodeSmallBitmap getSmallBitmap# stackSnapshot# index offsetStgClosurePayload
          in
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
          let retFunSize' = getWord stackSnapshot# (index + offsetStgRetFunFrameSize)
              retFunFun' = getClosureBox stackSnapshot# (index + offsetStgRetFunFrameFun)
          retFunPayload' <-
            if isArgGenBigRetFunType stackSnapshot# index == True
              then decodeLargeBitmap getRetFunLargeBitmap# stackSnapshot# index offsetStgRetFunFramePayload
              else pure $ decodeSmallBitmap getRetFunSmallBitmap# stackSnapshot# index offsetStgRetFunFramePayload
          pure $
            RetFun
              { info_tbl = info,
                retFunSize = retFunSize',
                retFunFun = retFunFun',
                retFunPayload = retFunPayload'
              }
        UPDATE_FRAME ->
          let updatee' = getClosureBox stackSnapshot# (index + offsetStgUpdateFrameUpdatee)
          in
            pure $
              UpdateFrame
                { info_tbl = info,
                  updatee = updatee'
                }
        CATCH_FRAME -> do
          let handler' = getClosureBox stackSnapshot# (index + offsetStgCatchFrameHandler)
          pure $
            CatchFrame
              { info_tbl = info,
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
          let atomicallyFrameCode' = getClosureBox stackSnapshot# (index + offsetStgAtomicallyFrameCode)
              result' = getClosureBox stackSnapshot# (index + offsetStgAtomicallyFrameResult)
          pure $
            AtomicallyFrame
              { info_tbl = info,
                atomicallyFrameCode = atomicallyFrameCode',
                result = result'
              }
        CATCH_RETRY_FRAME ->
          let running_alt_code' = getWord stackSnapshot# (index + offsetStgCatchRetryFrameRunningAltCode)
              first_code' = getClosureBox stackSnapshot# (index + offsetStgCatchRetryFrameRunningFirstCode)
              alt_code' = getClosureBox stackSnapshot# (index + offsetStgCatchRetryFrameAltCode)
          in
            pure $
              CatchRetryFrame
                { info_tbl = info,
                  running_alt_code = running_alt_code',
                  first_code = first_code',
                  alt_code = alt_code'
                }
        CATCH_STM_FRAME ->
          let catchFrameCode' = getClosureBox stackSnapshot# (index + offsetStgCatchSTMFrameCode)
              handler' = getClosureBox stackSnapshot# (index + offsetStgCatchSTMFrameHandler)
          in
            pure $
              CatchStmFrame
                { info_tbl = info,
                  catchFrameCode = catchFrameCode',
                  handler = handler'
                }
        ANN_FRAME ->
          let annotation = getClosureBox stackSnapshot# (index + offsetStgAnnFrameAnn)
           in
             pure $
               AnnFrame
                { info_tbl = info,
                  annotation = annotation
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
      let stack_size' = getStackFields stack#
          sfls = stackFrameLocations stack#
      stack' <- mapM unpackStackFrame sfls
      pure $
        GenStgStackClosure
          { ssc_info = info,
            ssc_stack_size = stack_size',
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
