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
import GHC.Exts.Heap.Decode
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

getUnderflowFrameNextChunk :: StackSnapshot# -> WordOffset -> IO StackSnapshot
getUnderflowFrameNextChunk stackSnapshot# index = IO $ \s ->
  case getUnderflowFrameNextChunk# stackSnapshot# (wordOffsetToWord# index) s of
    (# s1, stack# #) -> (# s1, StackSnapshot stack# #)

foreign import prim "getWordzh" getWord# :: StackSnapshot# -> Word# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

getWord :: StackSnapshot# -> WordOffset -> WordOffset -> IO Word
getWord stackSnapshot# index relativeOffset = IO $ \s ->
  case getWord#
    stackSnapshot#
    (wordOffsetToWord# index)
    (wordOffsetToWord# relativeOffset)
    s of
    (# s1, w# #) -> (# s1, W# w# #)

type WordGetter = StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

foreign import prim "getRetFunTypezh" getRetFunType# :: WordGetter

getRetFunType :: StackSnapshot# -> WordOffset -> IO RetFunType
getRetFunType stackSnapshot# index =
  toEnum . fromInteger . toInteger
    <$> IO
      ( \s ->
          case getRetFunType#
            stackSnapshot#
            (wordOffsetToWord# index)
            s of
            (# s1, rft# #) -> (# s1, W# rft# #)
      )

type LargeBitmapGetter = StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, ByteArray#, Word# #)

foreign import prim "getLargeBitmapzh" getLargeBitmap# :: LargeBitmapGetter

foreign import prim "getBCOLargeBitmapzh" getBCOLargeBitmap# :: LargeBitmapGetter

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: LargeBitmapGetter

type SmallBitmapGetter = StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word#, Word# #)

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

foreign import prim "getBoxedClosurezh" getBoxedClosure# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Any #)

foreign import prim "getStackFieldszh" getStackFields# :: StackSnapshot# -> State# RealWorld -> (# State# RealWorld, Word32#, Word8#, Word8# #)

getStackFields :: StackSnapshot# -> IO (Word32, Word8, Word8)
getStackFields stackSnapshot# = IO $ \s ->
  case getStackFields# stackSnapshot# s of
    (# s1, sSize#, sDirty#, sMarking# #) ->
      (# s1, (W32# sSize#, W8# sDirty#, W8# sMarking#) #)

-- | Get an interator starting with the top-most stack frame
stackHead :: StackSnapshot -> (StackSnapshot, WordOffset)
stackHead (StackSnapshot s#) = (StackSnapshot s#, 0 ) -- GHC stacks are never empty

-- | Advance to the next stack frame (if any)
--
-- The last `Int#` in the result tuple is meant to be treated as bool
-- (has_next).
foreign import prim "advanceStackFrameIterzh" advanceStackFrameIter# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

-- | Advance iterator to the next stack frame (if any)
advanceStackFrameIter :: StackSnapshot -> WordOffset -> Maybe (StackSnapshot, WordOffset)
advanceStackFrameIter (StackSnapshot stackSnapshot#) index =
  let !(# s', i', hasNext #) = advanceStackFrameIter# stackSnapshot# (wordOffsetToWord# index)
   in if I# hasNext > 0
        then Just $ (StackSnapshot s', (primWordToWordOffset i'))
        else Nothing
  where
    primWordToWordOffset :: Word# -> WordOffset
    primWordToWordOffset w# = fromIntegral (W# w#)

getClosure :: StackSnapshot# -> WordOffset -> WordOffset -> IO Box
getClosure stackSnapshot# index relativeOffset =
  IO $ \s ->
    case getBoxedClosure#
      stackSnapshot#
      (wordOffsetToWord# (index + relativeOffset))
      s of
      (# s1, ptr #) ->
        (# s1, Box ptr #)

-- | Iterator state for stack decoding
data StackFrameIter =
  -- | Represents a closure on the stack
  SfiClosure !StackSnapshot# !WordOffset
  -- | Represents a primitive word on the stack
  | SfiPrimitive !StackSnapshot# !WordOffset

decodeLargeBitmap :: LargeBitmapGetter -> StackSnapshot# -> WordOffset -> WordOffset -> IO [Closure]
decodeLargeBitmap getterFun# stackSnapshot# index relativePayloadOffset = do
  (bitmapArray, size) <- IO $ \s ->
    case getterFun# stackSnapshot# (wordOffsetToWord# index) s of
      (# s1, ba#, s# #) -> (# s1, (ByteArray ba#, W# s#) #)
  let bitmapWords :: [Word] = byteArrayToList bitmapArray
  decodeBitmaps stackSnapshot# index relativePayloadOffset bitmapWords size
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

decodeBitmaps :: StackSnapshot# -> WordOffset -> WordOffset -> [Word] -> Word -> IO [Closure]
decodeBitmaps stackSnapshot# index relativePayloadOffset bitmapWords size =
  let bes = wordsToBitmapEntries (index + relativePayloadOffset) bitmapWords size
   in mapM toBitmapPayload bes
  where
    toBitmapPayload :: StackFrameIter -> IO Closure
    toBitmapPayload (SfiPrimitive stack# i)  = do
      w <- getWord stack# i 0
      pure $ UnknownTypeWordSizedPrimitive w
    toBitmapPayload (SfiClosure stack# i) = getBoxedClosureData =<< getClosure stack# i 0

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

-- TODO: (auto-) format the code
-- TODO: Check all functions with two WordOffsets? Can't it be one?
decodeSmallBitmap :: SmallBitmapGetter -> StackSnapshot# -> WordOffset -> WordOffset -> IO [Closure]
decodeSmallBitmap getterFun# stackSnapshot# index relativePayloadOffset =
  do
    (bitmap, size) <- IO $ \s ->
      case getterFun# stackSnapshot# (wordOffsetToWord# index) s of
        (# s1, b#, s# #) -> (# s1, (W# b#, W# s#) #)
    let bitmapWords = [bitmap | size > 0]
    decodeBitmaps stackSnapshot# index relativePayloadOffset bitmapWords size

unpackStackFrame :: StackFrameLocation -> IO StackFrame
unpackStackFrame ((StackSnapshot stackSnapshot#), index) = do
  info <- getInfoTableOnStack stackSnapshot# index
  unpackStackFrame' info
  where
    unpackStackFrame' :: StgInfoTable -> IO StackFrame
    unpackStackFrame' info =
      case tipe info of
        RET_BCO -> do
          bco' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgClosurePayload
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
          retFunType' <- getRetFunType stackSnapshot# index
          retFunSize' <- getWord stackSnapshot# index offsetStgRetFunFrameSize
          retFunFun' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgRetFunFrameFun
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
          updatee' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgUpdateFrameUpdatee
          pure $
            UpdateFrame
              { info_tbl = info,
                updatee = updatee'
              }
        CATCH_FRAME -> do
          exceptions_blocked' <- getWord stackSnapshot# index offsetStgCatchFrameExceptionsBlocked
          handler' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgCatchFrameHandler
          pure $
            CatchFrame
              { info_tbl = info,
                exceptions_blocked = exceptions_blocked',
                handler = handler'
              }
        UNDERFLOW_FRAME -> do
          nextChunk' <- getUnderflowFrameNextChunk stackSnapshot# index
          stackClosure <- decodeStack nextChunk'
          pure $
            UnderflowFrame
              { info_tbl = info,
                nextChunk = stackClosure
              }
        STOP_FRAME -> pure $ StopFrame {info_tbl = info}
        ATOMICALLY_FRAME -> do
          atomicallyFrameCode' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgAtomicallyFrameCode
          result' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgAtomicallyFrameResult
          pure $
            AtomicallyFrame
              { info_tbl = info,
                atomicallyFrameCode = atomicallyFrameCode',
                result = result'
              }
        CATCH_RETRY_FRAME -> do
          running_alt_code' <- getWord stackSnapshot# index offsetStgCatchRetryFrameRunningAltCode
          first_code' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgCatchRetryFrameRunningFirstCode
          alt_code' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgCatchRetryFrameAltCode
          pure $
            CatchRetryFrame
              { info_tbl = info,
                running_alt_code = running_alt_code',
                first_code = first_code',
                alt_code = alt_code'
              }
        CATCH_STM_FRAME -> do
          -- TODO: Move `getBoxedClosureData =<<` to `getClosure`
          catchFrameCode' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgCatchSTMFrameCode
          handler' <- getBoxedClosureData =<< getClosure stackSnapshot# index offsetStgCatchSTMFrameHandler
          pure $
            CatchStmFrame
              { info_tbl = info,
                catchFrameCode = catchFrameCode',
                handler = handler'
              }
        x -> error $ "Unexpected closure type on stack: " ++ show x

-- TODO: Duplicate
getClosureDataFromHeapObject
    :: a
    -- ^ Heap object to decode.
    -> IO Closure
    -- ^ Heap representation of the closure.
getClosureDataFromHeapObject x = do
    case unpackClosure# x of
        (# infoTableAddr, heapRep, pointersArray #) -> do
            let infoTablePtr = Ptr infoTableAddr
                ptrList = [case indexArray# pointersArray i of
                                (# ptr #) -> Box ptr
                            | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
                            ]

            infoTable <- peekItbl infoTablePtr
            case tipe infoTable of
                TSO   -> pure $ UnsupportedClosure infoTable
                STACK -> pure $ UnsupportedClosure infoTable
                _ -> getClosureDataFromHeapRep heapRep infoTablePtr ptrList

-- | Like 'getClosureData', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureDataFromHeapObject a

-- | Unbox 'Int#' from 'Int'
toInt# :: Int -> Int#
toInt# (I# i) = i

-- | Convert `Int` to `Word#`
intToWord# :: Int -> Word#
intToWord# i = int2Word# (toInt# i)

wordOffsetToWord# :: WordOffset -> Word#
wordOffsetToWord# wo = intToWord# (fromIntegral wo)

type StackFrameLocation = (StackSnapshot, WordOffset)

-- | Decode `StackSnapshot` to a `StgStackClosure`
--
-- The return value is the representation of the @StgStack@ itself.
decodeStack :: StackSnapshot -> IO StgStackClosure
decodeStack (StackSnapshot stack#) = do
  info <- getInfoTableForStack stack#
  (stack_size', stack_dirty', stack_marking') <- getStackFields stack#
  case tipe info of
    STACK -> do
      let sfis = stackFrameLocations (StackSnapshot stack#)
      stack' <- mapM unpackStackFrame sfis
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
     stackFrameLocations :: StackSnapshot -> [StackFrameLocation]
     stackFrameLocations s =
      (stackHead s)
        : go (advanceStackFrameIter (fst (stackHead s)) (snd (stackHead s)))
      where
        go :: Maybe StackFrameLocation -> [StackFrameLocation]
        go Nothing = []
        go (Just r) = r : go (advanceStackFrameIter (fst r) (snd r))

#else
module GHC.Exts.Stack.Decode where
#endif
