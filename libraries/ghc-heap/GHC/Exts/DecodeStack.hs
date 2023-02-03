{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,5,0)
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- TODO: Find better place than top level. Re-export from top-level?
module GHC.Exts.DecodeStack
  ( decodeStack,
    unpackStackFrameIter
  )
where

import Data.Bits
import Data.Maybe
-- TODO: Remove before releasing
import Debug.Trace
import Foreign
import GHC.Exts
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.Constants (wORD_SIZE_IN_BITS)
import GHC.Exts.Heap.InfoTable
import GHC.Exts.StackConstants
import GHC.Stack.CloneStack
import Prelude
import GHC.IO (IO (..))
import Data.Array.Byte

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

A stack frame interator (StackFrameIter) consists of a StackSnapshot# and a
relative offset into the the array of stack frames (StgStack->stack). The
StackSnapshot# represents a StgStack closure. It is updated by the garbage
collector when the stack closure is moved.

The relative offset describes the location of a stack frame. As stack frames
come in various sizes, one cannot simply step over the stack array with a
constant offset.

The head of the stack frame array has offset 0. To traverse the stack frames the
latest stack frame's offset is incremented by the closure size. The unit of the
offset is machine words (32bit or 64bit).

Additionally, StackFrameIter contains a flag (isPrimitive) to indicate if a
location on the stack should be interpreted as plain data word (in contrast to
being a closure or a pointer to a closure.) It's used when bitmap encoded
arguments are interpreted.

Boxes
=====

As references into the stack frame array aren't updated by the garbage collector,
creating a Box with a pointer (address) to a stack frame would break as soon as
the StgStack closure is moved.

To deal with this another kind of Box is introduced: A StackFrameBox contains a
stack frame iterator for a decoded stack frame or it's payload.

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

foreign import prim "derefStackWordzh" derefStackWord# :: StackSnapshot# -> Word# -> Word#

derefStackWord :: StackFrameIter -> Word
derefStackWord (StackFrameIter {..}) = W# (derefStackWord# stackSnapshot# (wordOffsetToWord# index))

foreign import prim "getUpdateFrameTypezh" getUpdateFrameType# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

getUpdateFrameType :: StackFrameIter -> IO UpdateFrameType
getUpdateFrameType (StackFrameIter {..}) = (toEnum . fromInteger . toInteger) <$> IO (\s ->
   case (getUpdateFrameType# stackSnapshot# (wordOffsetToWord# index) s) of (# s1, uft# #) -> (# s1, W# uft# #))

foreign import prim "getUnderflowFrameNextChunkzh" getUnderflowFrameNextChunk# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, StackSnapshot# #)

getUnderflowFrameNextChunk :: StackFrameIter -> IO StackSnapshot
getUnderflowFrameNextChunk (StackFrameIter {..}) = IO $ \s ->
  case getUnderflowFrameNextChunk# stackSnapshot# (wordOffsetToWord# index) s of
    (# s1, stack# #) -> (# s1, StackSnapshot stack# #)

foreign import prim "getWordzh" getWord# :: StackSnapshot# -> Word# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

foreign import prim "getAddrzh" getAddr# :: StackSnapshot# -> Word# -> Addr#

getWord :: StackFrameIter -> WordOffset -> IO Word
getWord (StackFrameIter {..}) relativeOffset = IO $ \s ->
  case getWord# stackSnapshot# (wordOffsetToWord# index) (wordOffsetToWord# relativeOffset) s of
    (# s1, w# #) -> (# s1, W# w# #)

foreign import prim "getRetFunTypezh" getRetFunType# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

-- TODO: Could use getWord
getRetFunType :: StackFrameIter -> IO RetFunType
getRetFunType (StackFrameIter {..}) = (toEnum . fromInteger . toInteger) <$> IO (\s ->
   case (getRetFunType# stackSnapshot# (wordOffsetToWord# index) s) of (# s1, rft# #) -> (# s1, W# rft# #))

foreign import prim "getLargeBitmapzh" getLargeBitmap# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, ByteArray#, Word# #)

foreign import prim "getBCOLargeBitmapzh" getBCOLargeBitmap# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, ByteArray#, Word# #)

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, ByteArray#, Word# #)

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word#, Word# #)

foreign import prim "getRetSmallSpecialTypezh" getRetSmallSpecialType# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)

getRetSmallSpecialType :: StackFrameIter -> IO SpecialRetSmall
getRetSmallSpecialType (StackFrameIter {..}) = (toEnum . fromInteger . toInteger) <$> IO (\s ->
   case (getRetSmallSpecialType# stackSnapshot# (wordOffsetToWord# index) s) of (# s1, rft# #) -> (# s1, W# rft# #))

foreign import prim "getRetFunSmallBitmapzh" getRetFunSmallBitmap# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word#, Word# #)

foreign import prim "advanceStackFrameIterzh" advanceStackFrameIter# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

foreign import prim "getInfoTableAddrzh" getInfoTableAddr# :: StackSnapshot# -> Word# -> Addr#

getInfoTable :: StackFrameIter -> IO StgInfoTable
getInfoTable StackFrameIter {..} =
  let infoTablePtr = Ptr (getInfoTableAddr# stackSnapshot# (wordOffsetToWord# index))
   in peekItbl infoTablePtr

foreign import prim "getBoxedClosurezh" getBoxedClosure# :: StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Addr# #)

-- -- TODO: Remove this instance (debug only)
-- instance Show StackFrameIter where
--   show (StackFrameIter {..}) = "StackFrameIter " ++ "(StackSnapshot _" ++ " " ++ show index

-- | Get an interator starting with the top-most stack frame
stackHead :: StackSnapshot -> StackFrameIter
stackHead (StackSnapshot s) = StackFrameIter s 0 False -- GHC stacks are never empty

-- | Advance iterator to the next stack frame (if any)
advanceStackFrameIter :: StackFrameIter -> Maybe StackFrameIter
advanceStackFrameIter (StackFrameIter {..}) =
  let !(# s', i', hasNext #) = advanceStackFrameIter# stackSnapshot# (wordOffsetToWord# index)
   in if (I# hasNext) > 0
        then Just $ StackFrameIter s' (primWordToWordOffset i') False
        else Nothing

primWordToWordOffset :: Word# -> WordOffset
primWordToWordOffset w# = fromIntegral (W# w#)

wordsToBitmapEntries :: StackFrameIter -> [Word] -> Word -> [StackFrameIter]
wordsToBitmapEntries _ [] 0 = []
wordsToBitmapEntries _ [] i = error $ "Invalid state: Empty list, size " ++ show i
wordsToBitmapEntries _ l 0 = error $ "Invalid state: Size 0, list " ++ show l
wordsToBitmapEntries sfi (b : bs) bitmapSize =
  let entries = toBitmapEntries sfi b (min bitmapSize (fromIntegral wORD_SIZE_IN_BITS))
      mbLastFrame = (listToMaybe . reverse) entries
   in case mbLastFrame of
        Just (StackFrameIter {..}) ->
          entries ++ wordsToBitmapEntries (StackFrameIter stackSnapshot# (index + 1) False) bs (subtractDecodedBitmapWord bitmapSize)
        Nothing -> error "This should never happen! Recursion ended not in base case."
  where
    subtractDecodedBitmapWord :: Word -> Word
    subtractDecodedBitmapWord bSize = fromIntegral $ max 0 ((fromIntegral bSize) - wORD_SIZE_IN_BITS)

toBitmapEntries :: StackFrameIter -> Word -> Word -> [StackFrameIter]
toBitmapEntries _ _ 0 = []
toBitmapEntries sfi@(StackFrameIter {..}) bitmapWord bSize =
  -- TODO: overriding isPrimitive field is a bit weird. Could be calculated before
    sfi {
        isPrimitive = (bitmapWord .&. 1) /= 0
        }
    : toBitmapEntries (StackFrameIter stackSnapshot# (index + 1) False) (bitmapWord `shiftR` 1) (bSize - 1)

toBitmapPayload :: StackFrameIter -> IO Box
toBitmapPayload sfi
  | isPrimitive sfi = trace "PRIM" $ pure . StackFrameBox $ sfi {
                                      isPrimitive = True
                                     }
toBitmapPayload sfi = getClosure sfi 0

getClosure :: StackFrameIter -> WordOffset -> IO Box
getClosure sfi@StackFrameIter {..} relativeOffset = trace ("getClosure " ++ show sfi ++ "  " ++ show relativeOffset) $
   IO $ \s ->
      case (getBoxedClosure# stackSnapshot# (wordOffsetToWord# (index + relativeOffset)) s) of (# s1, ptr #) ->
                                                                                                 (# s1, Box (unsafeCoerce# ptr) #)

decodeLargeBitmap :: (StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, ByteArray#, Word# #)) -> StackFrameIter -> WordOffset -> IO [Box]
decodeLargeBitmap getterFun# sfi@(StackFrameIter {..}) relativePayloadOffset = do
  (bitmapArray, size) <- IO $ \s ->
    case getterFun# stackSnapshot# (wordOffsetToWord# index) s of
      (# s1, ba#, s# #) -> (# s1, (ByteArray ba#, W# s#) #)
  let bitmapWords :: [Word] = byteArrayToList bitmapArray
  decodeBitmaps sfi relativePayloadOffset bitmapWords size

decodeBitmaps :: StackFrameIter -> WordOffset -> [Word] -> Word -> IO [Box]
decodeBitmaps (StackFrameIter {..}) relativePayloadOffset bitmapWords size =
  let bes = wordsToBitmapEntries (StackFrameIter stackSnapshot# (index + relativePayloadOffset) False) bitmapWords size
   in mapM toBitmapPayload bes

decodeSmallBitmap :: (StackSnapshot# -> Word# -> State# RealWorld -> (# State# RealWorld, Word#, Word# #)) -> StackFrameIter -> WordOffset -> IO [Box]
decodeSmallBitmap getterFun# sfi@(StackFrameIter {..}) relativePayloadOffset = do
   (bitmap, size) <- IO $ \s ->
     case getterFun# stackSnapshot# (wordOffsetToWord# index) s of
       (# s1, b# , s# #) -> (# s1, (W# b# , W# s#) #)
   let bitmapWords = if size > 0 then [bitmap] else []
   decodeBitmaps sfi relativePayloadOffset bitmapWords size

byteArrayToList :: ByteArray -> [Word]
byteArrayToList (ByteArray bArray) = go 0
  where
    go i
      | i < maxIndex = (W# (indexWordArray# bArray (toInt# i))) : (go (i + 1))
      | otherwise = []
    maxIndex = sizeofByteArray bArray `quot` sizeOf (undefined :: Word)

wordOffsetToWord# :: WordOffset -> Word#
wordOffsetToWord# wo = intToWord# (fromIntegral wo)

unpackStackFrameIter :: StackFrameIter -> IO Closure
unpackStackFrameIter sfi | isPrimitive sfi = UnknownTypeWordSizedPrimitive <$> (getWord sfi 0)
unpackStackFrameIter sfi = do
  traceM $ "unpackStackFrameIter - sfi " ++ show sfi
  info <- getInfoTable sfi
  res <- unpackStackFrameIter' info
  traceM $ "unpackStackFrameIter - unpacked " ++ show res
  pure res
  where
    unpackStackFrameIter' :: StgInfoTable -> IO Closure
    unpackStackFrameIter' info =
      case tipe info of
        RET_BCO -> do
          bco' <- getClosure sfi offsetStgClosurePayload
          -- The arguments begin directly after the payload's one element
          bcoArgs' <- decodeLargeBitmap getBCOLargeBitmap# sfi (offsetStgClosurePayload + 1)
          pure $ RetBCO
            { info = info,
              bco = bco',
              bcoArgs = bcoArgs'
            }
        RET_SMALL ->
          trace "RET_SMALL" $ do
          payload' <- decodeSmallBitmap getSmallBitmap# sfi offsetStgClosurePayload
          knownRetSmallType' <- getRetSmallSpecialType sfi
          pure $ RetSmall
            { info = info,
              knownRetSmallType = knownRetSmallType',
              payload = payload'
            }
        RET_BIG -> do
          payload' <- decodeLargeBitmap getLargeBitmap# sfi offsetStgClosurePayload
          pure $ RetBig
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
          pure $ RetFun
            { info = info,
              retFunType = retFunType',
              retFunSize = retFunSize',
              retFunFun = retFunFun',
              retFunPayload = retFunPayload'
            }
        UPDATE_FRAME -> do
          updatee' <- getClosure sfi offsetStgUpdateFrameUpdatee
          knownUpdateFrameType' <- getUpdateFrameType sfi
          pure $ UpdateFrame
            { info = info,
              knownUpdateFrameType = knownUpdateFrameType',
              updatee = updatee'
            }
        CATCH_FRAME -> do
          exceptions_blocked' <- getWord sfi offsetStgCatchFrameExceptionsBlocked
          handler' <- getClosure sfi offsetStgCatchFrameHandler
          pure $ CatchFrame
            { info = info,
              exceptions_blocked = exceptions_blocked',
              handler = handler'
            }
        UNDERFLOW_FRAME -> do
          nextChunk' <- getUnderflowFrameNextChunk sfi
          pure $ UnderflowFrame
            { info = info,
              nextChunk = nextChunk'
            }
        STOP_FRAME -> pure $ StopFrame {info = info}
        ATOMICALLY_FRAME -> do
          atomicallyFrameCode' <- getClosure sfi offsetStgAtomicallyFrameCode
          result' <- getClosure sfi offsetStgAtomicallyFrameResult
          pure $ AtomicallyFrame
            { info = info,
              atomicallyFrameCode = atomicallyFrameCode',
              result = result'
            }
        CATCH_RETRY_FRAME -> do
          running_alt_code' <- getWord sfi offsetStgCatchRetryFrameRunningAltCode
          first_code' <- getClosure sfi offsetStgCatchRetryFrameRunningFirstCode
          alt_code' <- getClosure sfi offsetStgCatchRetryFrameAltCode
          pure $ CatchRetryFrame
            { info = info,
              running_alt_code = running_alt_code',
              first_code = first_code',
              alt_code = alt_code'
            }
        CATCH_STM_FRAME -> do
          catchFrameCode' <- getClosure sfi offsetStgCatchSTMFrameCode
          handler' <- getClosure sfi offsetStgCatchSTMFrameHandler
          pure $ CatchStmFrame
            { info = info,
              catchFrameCode = catchFrameCode',
              handler = handler'
            }
        x -> error $ "Unexpected closure type on stack: " ++ show x

-- | Size of the byte array in bytes.
-- Copied from `primitive`
sizeofByteArray :: ByteArray# -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray arr# = I# (sizeofByteArray# arr#)

-- | Unbox 'Int#' from 'Int'
toInt# :: Int -> Int#
toInt# (I# i) = i

intToWord# :: Int -> Word#
intToWord# i = int2Word# (toInt# i)

decodeStack :: StackSnapshot -> Closure
decodeStack = SimpleStack . decodeStack'

decodeStack' :: StackSnapshot -> [Box]
decodeStack' s = StackFrameBox (stackHead s) : go (advanceStackFrameIter (stackHead s))
  where
    go :: Maybe StackFrameIter -> [Box]
    go Nothing = []
    go (Just sfi) = (StackFrameBox sfi) : go (advanceStackFrameIter sfi)
#else
module GHC.Exts.DecodeStack where
#endif
