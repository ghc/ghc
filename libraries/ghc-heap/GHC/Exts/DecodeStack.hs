{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,17,0)
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

type StackFrameIter# = (#
                          -- | StgStack
                          StackSnapshot#,
                          -- | offset in machine words
                          Word#
                        #)

data StackFrameIter = StackFrameIter StackFrameIter#

-- TODO: Remove this instance (debug only)
instance Show StackFrameIter where
  show (StackFrameIter (# _, i# #)) = "StackFrameIter " ++ "(StackSnapshot _" ++ " " ++ show (W# i#)

-- | Get an interator starting with the top-most stack frame
stackHead :: StackSnapshot -> StackFrameIter
stackHead (StackSnapshot s) = StackFrameIter (# s , 0## #) -- GHC stacks are never empty

foreign import prim "advanceStackFrameIterzh" advanceStackFrameIter# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

-- | Advance iterator to the next stack frame (if any)
advanceStackFrameIter :: StackFrameIter -> Maybe StackFrameIter
advanceStackFrameIter (StackFrameIter (# s, i #)) = let !(# s', i', hasNext #) = advanceStackFrameIter# s i in
  if (I# hasNext) > 0 then Just $ StackFrameIter (# s', i' #)
  else Nothing

foreign import prim "getInfoTableTypezh" getInfoTableType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getLargeBitmapzh" getLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getBCOLargeBitmapzh" getBCOLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: StackSnapshot# -> Word# -> (# Word#, Word# #)

foreign import prim "getRetSmallSpecialTypezh" getRetSmallSpecialType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getRetFunSmallBitmapzh" getRetFunSmallBitmap# :: StackSnapshot# -> Word# -> (# Word#, Word# #)

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
          Just (StackFrameIter (# s'#, i'# #)) ->
            entries ++ wordsToBitmapEntries (StackFrameIter (# s'#, plusWord# i'# 1## #)) bs (subtractDecodedBitmapWord bitmapSize)
          Nothing -> error "This should never happen! Recursion ended not in base case."
  where
    subtractDecodedBitmapWord :: Word -> Word
    subtractDecodedBitmapWord bSize = fromIntegral $ max 0 ((fromIntegral bSize) - wORD_SIZE_IN_BITS)

toBitmapEntries :: StackFrameIter -> Word -> Word -> [BitmapEntry]
toBitmapEntries _ _ 0 = []
toBitmapEntries sfi@(StackFrameIter(# s, i #)) bitmap bSize = BitmapEntry {
    closureFrame = sfi,
    isPrimitive = (bitmap .&. 1) /= 0
  } : toBitmapEntries (StackFrameIter (# s , plusWord# i 1## #)) (bitmap `shiftR` 1) (bSize - 1)

toBitmapPayload :: BitmapEntry -> IO Box
toBitmapPayload e | isPrimitive e = pure $ asBox . CL.UnknownTypeWordSizedPrimitive . toWord . closureFrame $ e
      where
        toWord (StackFrameIter (# s#, i# #)) = W# (derefStackWord# s# i#)
toBitmapPayload e = toClosure unpackClosureFromStackFrame# (closureFrame e)

-- TODO: Negative offsets won't work! Consider using Word
getClosure :: StackFrameIter -> Int -> IO Box
getClosure sfi relativeOffset = toClosure (unpackClosureReferencedByFrame# (intToWord# relativeOffset)) sfi

toClosure :: (StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# Any #)) -> StackFrameIter -> IO Box
toClosure f# (StackFrameIter (# s#, i# #)) =
  case f# s# i# of
      (# infoTableAddr, heapRep, pointersArray #) ->
          let infoTablePtr = Ptr infoTableAddr
              ptrList = [case indexArray# pointersArray i of
                              (# ptr #) -> CL.Box ptr
                          | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
                          ]
          in
            DecodedClosureBox <$> (getClosureDataFromHeapRep heapRep infoTablePtr ptrList)

-- TODO: Make function more readable: No IO in let bindings
decodeLargeBitmap :: (StackSnapshot# -> Word# -> (# ByteArray#, Word# #)) -> StackFrameIter -> Word# -> IO [Box]
decodeLargeBitmap getterFun# (StackFrameIter (# stackFrame#, closureOffset# #)) relativePayloadOffset# =
      let !(# bitmapArray#, size# #) = getterFun# stackFrame# closureOffset#
          bitmapWords :: [Word] = foldrByteArray (\w acc -> W# w : acc) [] bitmapArray#
          bes = wordsToBitmapEntries (StackFrameIter (# stackFrame#, plusWord# closureOffset# relativePayloadOffset# #)) bitmapWords (W# size#)
          payloads = mapM toBitmapPayload bes
      in
        payloads

-- TODO: Make function more readable: No IO in let bindings
decodeSmallBitmap :: (StackSnapshot# -> Word# -> (# Word#, Word# #)) -> StackFrameIter -> Word# -> IO [Box]
decodeSmallBitmap getterFun# (StackFrameIter (# stackFrame#, closureOffset# #)) relativePayloadOffset# =
      let !(# bitmap#, size# #) = getterFun# stackFrame# closureOffset#
          bes = toBitmapEntries (StackFrameIter (# stackFrame#, plusWord# closureOffset# relativePayloadOffset# #))(W# bitmap#) (W# size#)
          payloads = mapM toBitmapPayload bes
      in
        payloads

-- TODO: Negative offsets won't work! Consider using Word
getHalfWord :: StackFrameIter -> Int -> Word
getHalfWord (StackFrameIter (# s#, i# #)) relativeOffset = W# (getHalfWord# s# i# (intToWord# relativeOffset))

-- TODO: Negative offsets won't work! Consider using Word
getWord :: StackFrameIter -> Int -> Word
getWord (StackFrameIter (# s#, i# #)) relativeOffset = W# (getWord# s# i# (intToWord# relativeOffset))

unpackStackFrameIter :: StackFrameIter -> IO CL.Closure
unpackStackFrameIter sfi@(StackFrameIter (# s#, i# #)) = trace ("decoding ... " ++ show @ClosureType ((toEnum . fromIntegral) (W# (getInfoTableType# s# i#))) ++ "\n") $
  case (toEnum . fromIntegral) (W# (getInfoTableType# s# i#)) of
     RET_BCO -> do
        instrs' <- getClosure sfi offsetStgRetBCOFrameInstrs
        literals'<- getClosure sfi offsetStgRetBCOFrameLiterals
        ptrs' <- getClosure sfi offsetStgRetBCOFramePtrs
        let arity' = getHalfWord sfi offsetStgRetBCOFrameArity
            size' = getHalfWord sfi offsetStgRetBCOFrameSize
        payload' <- decodeLargeBitmap getBCOLargeBitmap# sfi 2##
        pure $ CL.RetBCO {
                bcoInstrs = instrs',
                bcoLiterals  = literals',
                bcoPtrs = ptrs',
                bcoArity = arity',
                bcoSize = size',
                bcoPayload = payload'
              }
     RET_SMALL -> do
                    payloads <- decodeSmallBitmap getSmallBitmap# sfi 1##
                    let special# = getRetSmallSpecialType# s# i#
                        special = (toEnum . fromInteger . toInteger) (W# special#)
                    pure $ CL.RetSmall special payloads
     RET_BIG -> CL.RetBig <$> decodeLargeBitmap getLargeBitmap# sfi 1##
     RET_FUN -> do
        let t = (toEnum . fromInteger . toInteger) (W# (getRetFunType# s# i#))
            size' = getWord sfi offsetStgRetFunFrameSize
        fun' <- getClosure sfi offsetStgRetFunFrameFun
        payload' <-
          if t == CL.ARG_GEN_BIG then
            decodeLargeBitmap getRetFunLargeBitmap# sfi 2##
          else
            decodeSmallBitmap getRetFunSmallBitmap# sfi 2##
        pure $ CL.RetFun t size' fun' payload'
     -- TODO: Decode update frame type
     UPDATE_FRAME -> let
        !t = (toEnum . fromInteger . toInteger) (W# (getUpdateFrameType# s# i#))
        c = getClosure sfi offsetStgUpdateFrameUpdatee
      in
        (CL.UpdateFrame t ) <$> c
     CATCH_FRAME -> do
        -- TODO: Replace with getWord# expression
        let exceptionsBlocked = W# (getCatchFrameExceptionsBlocked# s# i#)
        c <- getClosure sfi offsetStgCatchFrameHandler
        pure $ CL.CatchFrame exceptionsBlocked c
     UNDERFLOW_FRAME -> let
          nextChunk# = getUnderflowFrameNextChunk# s# i#
        in
          pure $ CL.UnderflowFrame (StackSnapshot nextChunk#)
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

-- TODO: Is the function type below needed? (Was proposed by Ben)
-- derefStackPtr :: StackSnapshot# -> Int# -> a

foreign import prim "unpackClosureFromStackFramezh" unpackClosureFromStackFrame# :: StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# b #)

foreign import prim "derefStackWordzh" derefStackWord# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getUpdateFrameTypezh" getUpdateFrameType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "unpackClosureReferencedByFramezh" unpackClosureReferencedByFrame# :: Word# -> StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# b #)

foreign import prim "getCatchFrameExceptionsBlockedzh" getCatchFrameExceptionsBlocked#  :: StackSnapshot# -> Word# -> Word#

foreign import prim "getUnderflowFrameNextChunkzh" getUnderflowFrameNextChunk# :: StackSnapshot# -> Word# -> StackSnapshot#

foreign import prim "getWordzh" getWord# ::  StackSnapshot# -> Word# -> Word# -> Word#

foreign import prim "getHalfWordzh" getHalfWord# ::  StackSnapshot# -> Word# -> Word# -> Word#

foreign import prim "getRetFunTypezh" getRetFunType# :: StackSnapshot# -> Word# -> Word#

#if defined(DEBUG)
foreign import ccall "belchStack" belchStack# :: StackSnapshot# -> IO ()

belchStack :: StackSnapshot -> IO ()
belchStack (StackSnapshot s#) = belchStack# s#
#endif

decodeStack :: StackSnapshot -> IO CL.Closure
decodeStack s = do
#if defined(DEBUG)
  belchStack s
#endif
  SimpleStack . (map asBox) <$> decodeStack' s

decodeStack' :: StackSnapshot -> IO [CL.Closure]
decodeStack' s = unpackStackFrameIter (stackHead s) >>= \frame -> (frame :) <$> go (advanceStackFrameIter (stackHead s))
  where
    go :: Maybe StackFrameIter -> IO [CL.Closure]
    go Nothing = pure []
    go (Just sfi) = (trace "decode\n" (unpackStackFrameIter sfi)) >>= \frame -> (frame :) <$> go (advanceStackFrameIter sfi)

#else
module GHC.Exts.DecodeStack where
#endif
