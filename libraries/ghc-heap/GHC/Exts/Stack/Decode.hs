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
    unpackStackFrameIter,
  )
where

import Data.Array.Byte
import Data.Bits
import Data.Maybe
import Foreign
import GHC.Exts
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
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
(pointers) to stack frames are not updated.

As the StgStack closure is moved as whole, the relative offsets inside it stay
the same. (Though, the absolute addresses change!)

Stack frame iterator
====================

A stack frame iterator (StackFrameIter) deals with the mentioned challenges
regarding garbage collected memory. It consists of the StgStack itself and the
mentioned offset (or index) where needed.

It has three constructors:

- SfiStackClosure: Represents the StgStack closure itself. As stacks are chained
  by underflow frames, there can be multiple StgStack closures per logical
  stack.

- SfiClosure: Represents a closure on the stack. The location on the stack is
  defined by the StgStack itself and an index into it.

- SfiPrimitive: Is structurally equivalent to SfiClosure, but represents a data
  Word on the stack. These appear as payloads to closures with bitmap layout.
  From the RTS-perspective, there's no information about the concrete type of
  the Word. So, it's just handled as Word in further processing.

The `stackSnapshot# :: !StackSnapshot#` field represents a StgStack closure. It
is updated by the garbage collector when the stack closure is moved.

The relative offset (index) describes the location of a stack frame on the
stack. As stack frames come in various sizes, one cannot simply step over the
stack array with a constant offset.

The head of the stack frame array has offset (index) 0. To traverse the stack
frames the latest stack frame's offset is incremented by the closure size. The
unit of the offset is machine words (32bit or 64bit.)

Boxes
=====

As references into the stack frame array aren't updated by the garbage collector,
creating a Box with a pointer (address) to a stack frame would break as soon as
the StgStack closure is moved.

To deal with this another kind of Box is introduced: A StackFrameBox contains a
stack frame iterator (StackFrameIter).

Heap-represented closures referenced by stack frames are boxed the usual way,
with a Box that contains a pointer to the closure as it's payload. In
Haskell-land this means: A Box which contains the closure.

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

foreign import prim "getUnderflowFrameNextChunkzh" getUnderflowFrameNextChunk# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, StackSnapshot# #)

getUnderflowFrameNextChunk :: StackFrameIter -> IO StackSnapshot
getUnderflowFrameNextChunk (SfiClosure {..}) = IO $ \s ->
  case getUnderflowFrameNextChunk# stackSnapshot# (wordOffsetToWord# index) s of
    (# s1, stack# #) -> (# s1, StackSnapshot stack# #)
getUnderflowFrameNextChunk sfi = error $ "Unexpected StackFrameIter type: " ++ show sfi

foreign import prim "getWordzh" getWord# :: StackSnapshot# -> Word# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

getWord :: StackFrameIter -> WordOffset -> IO Word
getWord (SfiPrimitive {..}) relativeOffset = IO $ \s ->
  case getWord#
    stackSnapshot#
    (wordOffsetToWord# index)
    (wordOffsetToWord# relativeOffset)
    s of
    (# s1, w# #) -> (# s1, W# w# #)
getWord (SfiClosure {..}) relativeOffset = IO $ \s ->
  case getWord#
    stackSnapshot#
    (wordOffsetToWord# index)
    (wordOffsetToWord# relativeOffset)
    s of
    (# s1, w# #) -> (# s1, W# w# #)
getWord sfi _ = error $ "Unexpected StackFrameIter type: " ++ show sfi

type WordGetter = StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

foreign import prim "getRetFunTypezh" getRetFunType# :: WordGetter

getRetFunType :: StackFrameIter -> IO RetFunType
getRetFunType (SfiClosure {..}) =
  toEnum . fromInteger . toInteger
    <$> IO
      ( \s ->
          case getRetFunType#
            stackSnapshot#
            (wordOffsetToWord# index)
            s of
            (# s1, rft# #) -> (# s1, W# rft# #)
      )
getRetFunType sfi = error $ "Unexpected StackFrameIter type: " ++ show sfi

type LargeBitmapGetter = StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, ByteArray#, Word# #)

foreign import prim "getLargeBitmapzh" getLargeBitmap# :: LargeBitmapGetter

foreign import prim "getBCOLargeBitmapzh" getBCOLargeBitmap# :: LargeBitmapGetter

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: LargeBitmapGetter

type SmallBitmapGetter = StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word#, Word# #)

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: SmallBitmapGetter

foreign import prim "getRetFunSmallBitmapzh" getRetFunSmallBitmap# :: SmallBitmapGetter

foreign import prim "getInfoTableAddrzh" getInfoTableAddr# :: StackSnapshot# -> Word# -> Addr#

foreign import prim "getStackInfoTableAddrzh" getStackInfoTableAddr# :: StackSnapshot# -> Addr#

getInfoTable :: StackFrameIter -> IO StgInfoTable
getInfoTable SfiClosure {..} =
  let infoTablePtr = Ptr (getInfoTableAddr# stackSnapshot# (wordOffsetToWord# index))
   in peekItbl infoTablePtr
getInfoTable SfiStackClosure {..} =
  peekItbl $
    Ptr (getStackInfoTableAddr# stackSnapshot#)
getInfoTable _ = error "Primitives have no info table!"

foreign import prim "getBoxedClosurezh" getBoxedClosure# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Any #)

foreign import prim "getStackFieldszh" getStackFields# :: StackSnapshot# -> State# RealWorld -> (# State# RealWorld, Word32#, Word8#, Word8# #)

getStackFields :: StackFrameIter -> IO (Word32, Word8, Word8)
getStackFields SfiStackClosure {..} = IO $ \s ->
  case getStackFields# stackSnapshot# s of
    (# s1, sSize#, sDirty#, sMarking# #) ->
      (# s1, (W32# sSize#, W8# sDirty#, W8# sMarking#) #)
getStackFields sfi = error $ "Unexpected StackFrameIter type: " ++ show sfi

-- | Get an interator starting with the top-most stack frame
stackHead :: StackSnapshot -> StackFrameIter
stackHead (StackSnapshot s) = SfiClosure s 0 -- GHC stacks are never empty

-- | Advance to the next stack frame (if any)
--
-- The last `Int#` in the result tuple is meant to be treated as bool
-- (has_next).
foreign import prim "advanceStackFrameIterzh" advanceStackFrameIter# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

-- | Advance iterator to the next stack frame (if any)
advanceStackFrameIter :: StackFrameIter -> Maybe StackFrameIter
advanceStackFrameIter (SfiClosure {..}) =
  let !(# s', i', hasNext #) = advanceStackFrameIter# stackSnapshot# (wordOffsetToWord# index)
   in if I# hasNext > 0
        then Just $ SfiClosure s' (primWordToWordOffset i')
        else Nothing
  where
    primWordToWordOffset :: Word# -> WordOffset
    primWordToWordOffset w# = fromIntegral (W# w#)
advanceStackFrameIter sfi = error $ "Unexpected StackFrameIter type: " ++ show sfi

getClosure :: StackFrameIter -> WordOffset -> IO Box
getClosure SfiClosure {..} relativeOffset =
  IO $ \s ->
    case getBoxedClosure#
      stackSnapshot#
      (wordOffsetToWord# (index + relativeOffset))
      s of
      (# s1, ptr #) ->
        (# s1, Box ptr #)
getClosure sfi _ = error $ "Unexpected StackFrameIter type: " ++ show sfi

decodeLargeBitmap :: LargeBitmapGetter -> StackFrameIter -> WordOffset -> IO [Box]
decodeLargeBitmap getterFun# sfi@(SfiClosure {..}) relativePayloadOffset = do
  (bitmapArray, size) <- IO $ \s ->
    case getterFun# stackSnapshot# (wordOffsetToWord# index) s of
      (# s1, ba#, s# #) -> (# s1, (ByteArray ba#, W# s#) #)
  let bitmapWords :: [Word] = byteArrayToList bitmapArray
  decodeBitmaps sfi relativePayloadOffset bitmapWords size
  where
    byteArrayToList :: ByteArray -> [Word]
    byteArrayToList (ByteArray bArray) = go 0
      where
        go i
          | i < maxIndex = W# (indexWordArray# bArray (toInt# i)) : go (i + 1)
          | otherwise = []
        maxIndex = sizeofByteArray bArray `quot` sizeOf (undefined :: Word)

    sizeofByteArray :: ByteArray# -> Int
    sizeofByteArray arr# = I# (sizeofByteArray# arr#)
decodeLargeBitmap _ sfi _ = error $ "Unexpected StackFrameIter type: " ++ show sfi

decodeBitmaps :: StackFrameIter -> WordOffset -> [Word] -> Word -> IO [Box]
decodeBitmaps (SfiClosure {..}) relativePayloadOffset bitmapWords size =
  let bes = wordsToBitmapEntries (index + relativePayloadOffset) bitmapWords size
   in mapM toBitmapPayload bes
  where
    toBitmapPayload :: StackFrameIter -> IO Box
    toBitmapPayload sfi@SfiPrimitive {} = pure (StackFrameBox sfi)
    toBitmapPayload sfi@SfiClosure {} = getClosure sfi 0
    toBitmapPayload sfi = error $ "Unexpected StackFrameIter type: " ++ show sfi

    wordsToBitmapEntries :: WordOffset -> [Word] -> Word -> [StackFrameIter]
    wordsToBitmapEntries _ [] 0 = []
    wordsToBitmapEntries _ [] i = error $ "Invalid state: Empty list, size " ++ show i
    wordsToBitmapEntries _ l 0 = error $ "Invalid state: Size 0, list " ++ show l
    wordsToBitmapEntries index' (b : bs) bitmapSize =
      let entries = toBitmapEntries index' b (min bitmapSize (fromIntegral wORD_SIZE_IN_BITS))
          mbLastFrame = (listToMaybe . reverse) entries
       in case mbLastFrame of
            Just sfi' ->
              entries
                ++ wordsToBitmapEntries
                  ((getIndex sfi') + 1)
                  bs
                  subtractDecodedBitmapWord
            _ -> error "This should never happen! Recursion ended not in base case."
      where
        subtractDecodedBitmapWord :: Word
        subtractDecodedBitmapWord =
          fromIntegral $
            max 0 (fromIntegral bitmapSize - wORD_SIZE_IN_BITS)

        toBitmapEntries :: WordOffset -> Word -> Word -> [StackFrameIter]
        toBitmapEntries _ _ 0 = []
        toBitmapEntries i bitmapWord bSize =
          ( if (bitmapWord .&. 1) /= 0
              then SfiPrimitive stackSnapshot# i
              else SfiClosure stackSnapshot# i
          )
            : toBitmapEntries
              (i + 1)
              (bitmapWord `shiftR` 1)
              (bSize - 1)

        getIndex :: StackFrameIter -> WordOffset
        getIndex (SfiClosure _ i) = i
        getIndex (SfiPrimitive _ i) = i
        getIndex sfi' = error $ "Has no index : " ++ show sfi'
decodeBitmaps sfi _ _ _ = error $ "Unexpected StackFrameIter type: " ++ show sfi

decodeSmallBitmap :: SmallBitmapGetter -> StackFrameIter -> WordOffset -> IO [Box]
decodeSmallBitmap getterFun# sfi@(SfiClosure {..}) relativePayloadOffset =
  do
    (bitmap, size) <- IO $ \s ->
      case getterFun# stackSnapshot# (wordOffsetToWord# index) s of
        (# s1, b#, s# #) -> (# s1, (W# b#, W# s#) #)
    let bitmapWords = [bitmap | size > 0]
    decodeBitmaps sfi relativePayloadOffset bitmapWords size
decodeSmallBitmap _ sfi _ =
  error $
    "Unexpected StackFrameIter type: " ++ show sfi

-- | Decode `StackFrameIter` to `Closure`
unpackStackFrameIter :: StackFrameIter -> IO Closure
unpackStackFrameIter sfi@(SfiPrimitive {}) =
  UnknownTypeWordSizedPrimitive
    <$> getWord sfi 0
unpackStackFrameIter sfi@(SfiStackClosure {..}) = do
  info <- getInfoTable sfi
  (stack_size', stack_dirty', stack_marking') <- getStackFields sfi
  case tipe info of
    STACK -> do
      let stack' = decodeStackToBoxes (StackSnapshot stackSnapshot#)
      pure $
        StackClosure
          { info = info,
            stack_size = stack_size',
            stack_dirty = stack_dirty',
            stack_marking = stack_marking',
            stack = stack'
          }
    _ -> error $ "Expected STACK closure, got " ++ show info
  where
    decodeStackToBoxes :: StackSnapshot -> [Box]
    decodeStackToBoxes s =
      StackFrameBox (stackHead s)
        : go (advanceStackFrameIter (stackHead s))
      where
        go :: Maybe StackFrameIter -> [Box]
        go Nothing = []
        go (Just sfi') = StackFrameBox sfi' : go (advanceStackFrameIter sfi')
unpackStackFrameIter sfi@(SfiClosure {}) = do
  info <- getInfoTable sfi
  unpackStackFrameIter' info
  where
    unpackStackFrameIter' :: StgInfoTable -> IO Closure
    unpackStackFrameIter' info =
      case tipe info of
        RET_BCO -> do
          bco' <- getClosure sfi offsetStgClosurePayload
          -- The arguments begin directly after the payload's one element
          bcoArgs' <- decodeLargeBitmap getBCOLargeBitmap# sfi (offsetStgClosurePayload + 1)
          pure
            RetBCO
              { info = info,
                bco = bco',
                bcoArgs = bcoArgs'
              }
        RET_SMALL -> do
          payload' <- decodeSmallBitmap getSmallBitmap# sfi offsetStgClosurePayload
          pure $
            RetSmall
              { info = info,
                payload = payload'
              }
        RET_BIG -> do
          payload' <- decodeLargeBitmap getLargeBitmap# sfi offsetStgClosurePayload
          pure $
            RetBig
              { info = info,
                payload = payload'
              }
        RET_FUN -> do
          retFunType' <- getRetFunType sfi
          retFunSize' <- getWord sfi offsetStgRetFunFrameSize
          retFunFun' <- getClosure sfi offsetStgRetFunFrameFun
          retFunPayload' <-
            if retFunType' == ARG_GEN_BIG
              then decodeLargeBitmap getRetFunLargeBitmap# sfi offsetStgRetFunFramePayload
              else decodeSmallBitmap getRetFunSmallBitmap# sfi offsetStgRetFunFramePayload
          pure $
            RetFun
              { info = info,
                retFunType = retFunType',
                retFunSize = retFunSize',
                retFunFun = retFunFun',
                retFunPayload = retFunPayload'
              }
        UPDATE_FRAME -> do
          updatee' <- getClosure sfi offsetStgUpdateFrameUpdatee
          pure $
            UpdateFrame
              { info = info,
                updatee = updatee'
              }
        CATCH_FRAME -> do
          exceptions_blocked' <- getWord sfi offsetStgCatchFrameExceptionsBlocked
          handler' <- getClosure sfi offsetStgCatchFrameHandler
          pure $
            CatchFrame
              { info = info,
                exceptions_blocked = exceptions_blocked',
                handler = handler'
              }
        UNDERFLOW_FRAME -> do
          (StackSnapshot nextChunk') <- getUnderflowFrameNextChunk sfi
          pure $
            UnderflowFrame
              { info = info,
                nextChunk = StackFrameBox $ SfiStackClosure nextChunk'
              }
        STOP_FRAME -> pure $ StopFrame {info = info}
        ATOMICALLY_FRAME -> do
          atomicallyFrameCode' <- getClosure sfi offsetStgAtomicallyFrameCode
          result' <- getClosure sfi offsetStgAtomicallyFrameResult
          pure $
            AtomicallyFrame
              { info = info,
                atomicallyFrameCode = atomicallyFrameCode',
                result = result'
              }
        CATCH_RETRY_FRAME -> do
          running_alt_code' <- getWord sfi offsetStgCatchRetryFrameRunningAltCode
          first_code' <- getClosure sfi offsetStgCatchRetryFrameRunningFirstCode
          alt_code' <- getClosure sfi offsetStgCatchRetryFrameAltCode
          pure $
            CatchRetryFrame
              { info = info,
                running_alt_code = running_alt_code',
                first_code = first_code',
                alt_code = alt_code'
              }
        CATCH_STM_FRAME -> do
          catchFrameCode' <- getClosure sfi offsetStgCatchSTMFrameCode
          handler' <- getClosure sfi offsetStgCatchSTMFrameHandler
          pure $
            CatchStmFrame
              { info = info,
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

-- | Decode `StackSnapshot` to a Closure
--
-- Due to the use of `Box` this decoding is lazy. The first decoded closure is
-- the representation of the @StgStack@ itself.
decodeStack :: StackSnapshot -> IO Closure
decodeStack (StackSnapshot stack#) =
  unpackStackFrameIter $
    SfiStackClosure stack#

#else
module GHC.Exts.Stack.Decode where
#endif
