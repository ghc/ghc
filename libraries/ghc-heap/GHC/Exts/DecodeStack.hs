{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,5,0)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: Find better place than top level. Re-export from top-level?
module GHC.Exts.DecodeStack (
  decodeStack,
  decodeStack'
                            ) where

import GHC.Exts.StackConstants
import GHC.Exts.Heap.Constants (wORD_SIZE_IN_BITS)
import Data.Maybe
import Data.Bits
import Foreign
import Prelude
import GHC.Stack.CloneStack
-- TODO: Remove before releasing
import Debug.Trace
import GHC.Exts
import GHC.Exts.Heap.Closures as CL
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.DecodeHeap

foreign import prim "derefStackWordzh" derefStackWord# :: StackSnapshot# -> Word# -> Word#

derefStackWord :: StackFrameIter -> Word
derefStackWord (StackFrameIter {..}) = W# (derefStackWord# stackSnapshot# (wordOffsetToWord# index))

foreign import prim "getUpdateFrameTypezh" getUpdateFrameType# :: StackSnapshot# -> Word# -> Word#

-- TODO: This can be simplified if the offset is always full words
foreign import prim "unpackClosureReferencedByFramezh" unpackClosureReferencedByFrame# :: Word# -> StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# b #)

unpackClosureReferencedByFrame :: ByteOffset -> StackSnapshot# -> WordOffset -> (# Addr#, ByteArray#, Array# b #)
unpackClosureReferencedByFrame bo ss# wo = unpackClosureReferencedByFrame# (byteOffsetToWord# bo) ss# (wordOffsetToWord# wo)

foreign import prim "getCatchFrameExceptionsBlockedzh" getCatchFrameExceptionsBlocked#  :: StackSnapshot# -> Word# -> Word#

foreign import prim "getUnderflowFrameNextChunkzh" getUnderflowFrameNextChunk# :: StackSnapshot# -> Word# -> StackSnapshot#

foreign import prim "getWordzh" getWord# :: StackSnapshot# -> Word# -> Word# -> Word#

-- Use WordOffset - The access will likely be aligned to words
-- TODO: Negative offsets won't work! Consider using Word
getWord :: StackFrameIter -> ByteOffset -> Word
getWord (StackFrameIter {..}) relativeOffset = W# (getWord# stackSnapshot# (wordOffsetToWord# index) (byteOffsetToWord# relativeOffset))

foreign import prim "getRetFunTypezh" getRetFunType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getInfoTableTypezh" getInfoTableType# :: StackSnapshot# -> Word# -> Word#

getInfoTableType :: StackFrameIter -> ClosureType
getInfoTableType (StackFrameIter {..}) = (toEnum . fromIntegral) (W# (getInfoTableType# stackSnapshot# (wordOffsetToWord# index)))

foreign import prim "getLargeBitmapzh" getLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getBCOLargeBitmapzh" getBCOLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: StackSnapshot# -> Word# -> (# Word#, Word# #)

foreign import prim "getRetSmallSpecialTypezh" getRetSmallSpecialType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getRetFunSmallBitmapzh" getRetFunSmallBitmap# :: StackSnapshot# -> Word# -> (# Word#, Word# #)

foreign import prim "advanceStackFrameIterzh" advanceStackFrameIter# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

type StackFrameIter# = (#
                          -- | StgStack
                          StackSnapshot#,
                          -- | offset in machine words
                          Word#
                        #)

data StackFrameIter = StackFrameIter {
  stackSnapshot# :: StackSnapshot#,
  index :: WordOffset
                                     }
-- TODO: Remove this instance (debug only)
instance Show StackFrameIter where
  show (StackFrameIter { .. }) = "StackFrameIter " ++ "(StackSnapshot _" ++ " " ++ show index

-- | Get an interator starting with the top-most stack frame
stackHead :: StackSnapshot -> StackFrameIter
stackHead (StackSnapshot s) = StackFrameIter s 0 -- GHC stacks are never empty

-- | Advance iterator to the next stack frame (if any)
advanceStackFrameIter :: StackFrameIter -> Maybe StackFrameIter
advanceStackFrameIter (StackFrameIter {..}) = let !(# s', i', hasNext #) = advanceStackFrameIter# stackSnapshot# (wordOffsetToWord# index) in
  if (I# hasNext) > 0 then Just $ StackFrameIter s' (primWordToWordOffset i')
  else Nothing

primWordToWordOffset :: Word# -> WordOffset
primWordToWordOffset w# = fromIntegral (W# w#)

data BitmapEntry = BitmapEntry {
    closureFrame :: StackFrameIter,
    isPrimitive :: Bool
  } deriving (Show)

wordsToBitmapEntries :: StackFrameIter -> [Word] -> Word -> [BitmapEntry]
wordsToBitmapEntries _ [] 0 = []
wordsToBitmapEntries _ [] i = error $ "Invalid state: Empty list, size " ++ show i
wordsToBitmapEntries _ l 0 = error $ "Invalid state: Size 0, list " ++ show l
wordsToBitmapEntries sfi (b:bs) bitmapSize =
    let  entries = toBitmapEntries sfi b (min bitmapSize (fromIntegral wORD_SIZE_IN_BITS))
         mbLastEntry = (listToMaybe . reverse) entries
         mbLastFrame = fmap closureFrame mbLastEntry
      in
        case mbLastFrame of
          Just (StackFrameIter {..} ) ->
            entries ++ wordsToBitmapEntries (StackFrameIter stackSnapshot# (index + 1)) bs (subtractDecodedBitmapWord bitmapSize)
          Nothing -> error "This should never happen! Recursion ended not in base case."
  where
    subtractDecodedBitmapWord :: Word -> Word
    subtractDecodedBitmapWord bSize = fromIntegral $ max 0 ((fromIntegral bSize) - wORD_SIZE_IN_BITS)

toBitmapEntries :: StackFrameIter -> Word -> Word -> [BitmapEntry]
toBitmapEntries _ _ 0 = []
toBitmapEntries sfi@(StackFrameIter {..}) bitmapWord bSize = BitmapEntry {
    closureFrame = sfi,
    isPrimitive = (bitmapWord .&. 1) /= 0
  } : toBitmapEntries (StackFrameIter stackSnapshot# (index + 1)) (bitmapWord `shiftR` 1) (bSize - 1)

toBitmapPayload :: BitmapEntry -> IO Box
toBitmapPayload e | isPrimitive e = pure $ DecodedClosureBox. CL.UnknownTypeWordSizedPrimitive . derefStackWord . closureFrame $ e
toBitmapPayload e = toClosure (unpackClosureReferencedByFrame 0) (closureFrame e)

-- TODO: Offset should be in Words. That's the smallest reasonable unit.
-- TODO: Negative offsets won't work! Consider using Word
getClosure :: StackFrameIter ->  ByteOffset-> IO Box
getClosure sfi relativeOffset = toClosure (unpackClosureReferencedByFrame relativeOffset) sfi

toClosure :: (StackSnapshot# -> WordOffset -> (# Addr#, ByteArray#, Array# Any #)) -> StackFrameIter -> IO Box
toClosure f# (StackFrameIter {..}) =
  case f# stackSnapshot# index of
      (# infoTableAddr, heapRep, pointersArray #) ->
          let infoTablePtr = Ptr infoTableAddr
              ptrList = [case indexArray# pointersArray i of
                              (# ptr #) -> CL.Box ptr
                          | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
                          ]
          in
            DecodedClosureBox <$> (getClosureDataFromHeapRep heapRep infoTablePtr ptrList)

-- TODO: Make function more readable: No IO in let bindings
decodeLargeBitmap :: (StackSnapshot# -> Word# -> (# ByteArray#, Word# #)) -> StackFrameIter -> WordOffset -> IO [Box]
decodeLargeBitmap getterFun# (StackFrameIter {..}) relativePayloadOffset =
      let !(# bitmapArray#, size# #) = getterFun# stackSnapshot# (wordOffsetToWord# index)
          bitmapWords :: [Word] = foldrByteArray (\w acc -> W# w : acc) [] bitmapArray#
          bes = wordsToBitmapEntries (StackFrameIter stackSnapshot# (index + relativePayloadOffset)) bitmapWords (W# size#)
          payloads = mapM toBitmapPayload bes
      in
        payloads

-- TODO: Make function more readable: No IO in let bindings
decodeSmallBitmap :: (StackSnapshot# -> Word# -> (# Word#, Word# #)) -> StackFrameIter -> WordOffset -> IO [Box]
decodeSmallBitmap getterFun# (StackFrameIter {..}) relativePayloadOffset =
      let !(# bitmap#, size# #) = getterFun# stackSnapshot# (wordOffsetToWord# index)
          bes = toBitmapEntries (StackFrameIter stackSnapshot# (index + relativePayloadOffset)) (W# bitmap#) (W# size#)
          payloads = mapM toBitmapPayload bes
      in
        payloads

byteOffsetToWord# :: ByteOffset -> Word#
byteOffsetToWord# bo = intToWord# (fromIntegral bo)

wordOffsetToWord# :: WordOffset -> Word#
wordOffsetToWord# wo = intToWord# (fromIntegral wo)

getRetSmallSpecialType :: StackFrameIter -> SpecialRetSmall
getRetSmallSpecialType (StackFrameIter {..}) = let special# = getRetSmallSpecialType# stackSnapshot# (wordOffsetToWord# index)
                         in
                           (toEnum . fromInteger . toInteger) (W# special#)

getRetFunType :: StackFrameIter -> RetFunType
getRetFunType (StackFrameIter {..}) = (toEnum . fromInteger . toInteger) (W# (getRetFunType# stackSnapshot# (wordOffsetToWord# index)))

getUpdateFrameType :: StackFrameIter -> UpdateFrameType
getUpdateFrameType (StackFrameIter {..}) = (toEnum . fromInteger . toInteger) (W# (getUpdateFrameType# stackSnapshot# (wordOffsetToWord# index)))

getCatchFrameExceptionsBlocked :: StackFrameIter -> Word
getCatchFrameExceptionsBlocked (StackFrameIter {..}) = W# (getCatchFrameExceptionsBlocked# stackSnapshot# (wordOffsetToWord# index))

getUnderflowFrameNextChunk :: StackFrameIter -> StackSnapshot
getUnderflowFrameNextChunk (StackFrameIter {..}) = StackSnapshot s#
  where
   s# = getUnderflowFrameNextChunk# stackSnapshot# (wordOffsetToWord# index)

unpackStackFrameIter :: StackFrameIter -> IO CL.Closure
unpackStackFrameIter sfi =
  case getInfoTableType sfi of
     RET_BCO -> do
        bco' <- getClosure sfi offsetStgClosurePayload
        args' <- decodeLargeBitmap getBCOLargeBitmap# sfi 2
        pure $ CL.RetBCO bco' args'
     RET_SMALL -> do
                    payloads <- decodeSmallBitmap getSmallBitmap# sfi 1
                    let special = getRetSmallSpecialType sfi
                    pure $ CL.RetSmall special payloads
     RET_BIG -> CL.RetBig <$> decodeLargeBitmap getLargeBitmap# sfi 1
     RET_FUN -> do
        let t = getRetFunType sfi
            size' = getWord sfi offsetStgRetFunFrameSize
        fun' <- getClosure sfi offsetStgRetFunFrameFun
        payload' <-
          if t == CL.ARG_GEN_BIG then
            decodeLargeBitmap getRetFunLargeBitmap# sfi 3
          else
            -- TODO: The offsets should be based on DerivedConstants.h
            decodeSmallBitmap getRetFunSmallBitmap# sfi 3
        pure $ CL.RetFun t size' fun' payload'
     -- TODO: Decode update frame type
     UPDATE_FRAME -> let
        !t = getUpdateFrameType sfi
        c = getClosure sfi offsetStgUpdateFrameUpdatee
      in
        (CL.UpdateFrame t ) <$> c
     CATCH_FRAME -> do
        -- TODO: Replace with getWord# expression
        let exceptionsBlocked = getCatchFrameExceptionsBlocked sfi
        c <- getClosure sfi offsetStgCatchFrameHandler
        pure $ CL.CatchFrame exceptionsBlocked c
     UNDERFLOW_FRAME -> let
          nextChunk = getUnderflowFrameNextChunk sfi
        in
          pure $ CL.UnderflowFrame nextChunk
     STOP_FRAME -> pure CL.StopFrame
     ATOMICALLY_FRAME -> CL.AtomicallyFrame
            <$> getClosure sfi offsetStgAtomicallyFrameCode
            <*> getClosure sfi offsetStgAtomicallyFrameResult
     CATCH_RETRY_FRAME -> do
        let running_alt_code' = getWord sfi offsetStgCatchRetryFrameRunningAltCode
        first_code' <- getClosure sfi offsetStgCatchRetryFrameRunningFirstCode
        alt_code' <- getClosure sfi offsetStgCatchRetryFrameAltCode
        pure $ CL.CatchRetryFrame running_alt_code' first_code' alt_code'
     CATCH_STM_FRAME -> CL.CatchStmFrame
          <$> getClosure sfi offsetStgCatchSTMFrameCode
          <*> getClosure sfi offsetStgCatchSTMFrameHandler
     x -> error $ "Unexpected closure type on stack: " ++ show x

-- | Right-fold over the elements of a 'ByteArray'.
-- Copied from `primitive`
foldrByteArray :: forall b. (Word# -> b -> b) -> b -> ByteArray# -> b
{-# INLINE foldrByteArray #-}
foldrByteArray f z arr = go 0
  where
    go i
      | i < maxI  = f (indexWordArray# arr (toInt# i)) (go (i + 1))
      | otherwise = z
    maxI = sizeofByteArray arr `quot` sizeOf (undefined :: Word)

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

decodeStack :: StackSnapshot -> IO CL.Closure
decodeStack s = do
  stack <- decodeStack' s
  let boxed = map DecodedClosureBox stack
  pure $ SimpleStack boxed

decodeStack' :: StackSnapshot -> IO [CL.Closure]
decodeStack' s = unpackStackFrameIter (stackHead s) >>= \frame -> (frame :) <$> go (advanceStackFrameIter (stackHead s))
  where
    go :: Maybe StackFrameIter -> IO [CL.Closure]
    go Nothing = pure []
    go (Just sfi) = (trace "decode\n" (unpackStackFrameIter sfi)) >>= \frame -> (frame :) <$> go (advanceStackFrameIter sfi)

#else
module GHC.Exts.DecodeStack where
#endif
