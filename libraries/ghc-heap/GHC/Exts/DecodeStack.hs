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

-- TODO: Find better place than top level. Re-export from top-level?
module GHC.Exts.DecodeStack (
  StackFrame(..),
  BitmapPayload(..),
  decodeStack
                            ) where

import GHC.Exts.StackConstants
import GHC.Exts.Heap.Constants (wORD_SIZE_IN_BITS)
import Data.Maybe
import Data.Bits
import Foreign
import System.IO.Unsafe
import Prelude
import GHC.Stack.CloneStack
import GHC.Exts.Heap hiding (bitmap, size)
import Debug.Trace
import GHC.Exts
import qualified GHC.Exts.Heap.Closures as CL


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

instance Show StackSnapshot where
  show _ = "StackSnapshot _"

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
wordsToBitmapEntries sfi (b:bs) size =
    let  entries = toBitmapEntries sfi b (min size (fromIntegral wORD_SIZE_IN_BITS))
         mbLastEntry = (listToMaybe . reverse) entries
         mbLastFrame = fmap closureFrame mbLastEntry
      in
        case mbLastFrame of
          Just (StackFrameIter (# s'#, i'# #)) ->
            entries ++ wordsToBitmapEntries (StackFrameIter (# s'#, plusWord# i'# 1## #)) bs (subtractDecodedBitmapWord size)
          Nothing -> error "This should never happen! Recursion ended not in base case."
  where
    subtractDecodedBitmapWord :: Word -> Word
    subtractDecodedBitmapWord size = fromIntegral $ max 0 ((fromIntegral size) - wORD_SIZE_IN_BITS)

toBitmapEntries :: StackFrameIter -> Word -> Word -> [BitmapEntry]
toBitmapEntries _ _ 0 = []
toBitmapEntries sfi@(StackFrameIter(# s, i #)) bitmap size = BitmapEntry {
    closureFrame = sfi,
    isPrimitive = (bitmap .&. 1) /= 0
  } : toBitmapEntries (StackFrameIter (# s , plusWord# i 1## #)) (bitmap `shiftR` 1) (size - 1)

toBitmapPayload :: BitmapEntry -> BitmapPayload
toBitmapPayload e | isPrimitive e = Primitive . toWord . closureFrame $ e
      where
        toWord (StackFrameIter (# s#, i# #)) = W# (derefStackWord# s# i#)
toBitmapPayload e = Closure . toClosure unpackClosureFromStackFrame# . closureFrame $ e

-- TODO: Get rid of unsafePerformIO
toClosure :: (StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# Any #)) -> StackFrameIter -> CL.Closure
toClosure f# (StackFrameIter (# s#, i# #)) = unsafePerformIO $
  case f# s# i# of
      (# infoTableAddr, heapRep, pointersArray #) -> do
          let infoTablePtr = Ptr infoTableAddr
              ptrList = [case indexArray# pointersArray i of
                              (# ptr #) -> Box ptr
                          | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
                          ]

          getClosureDataFromHeapRep heapRep infoTablePtr ptrList

decodeLargeBitmap :: (StackSnapshot# -> Word# -> (# ByteArray#, Word# #)) -> StackFrameIter -> Word# -> [BitmapPayload]
decodeLargeBitmap getterFun# (StackFrameIter (# stackFrame#, closureOffset# #)) relativePayloadOffset# =
      let !(# bitmapArray#, size# #) = getterFun# stackFrame# closureOffset#
          bitmapWords :: [Word] = foldrByteArray (\w acc -> W# w : acc) [] bitmapArray#
          bes = wordsToBitmapEntries (StackFrameIter (# stackFrame#, plusWord# closureOffset# relativePayloadOffset# #)) (trace ("bitmapWords" ++ show bitmapWords) bitmapWords) (trace ("XXX size " ++ show (W# size#))(W# size#))
          payloads = map toBitmapPayload bes
      in
        payloads

decodeSmallBitmap :: (StackSnapshot# -> Word# -> (# Word#, Word# #)) -> StackFrameIter -> Word# -> [BitmapPayload]
decodeSmallBitmap getterFun# (StackFrameIter (# stackFrame#, closureOffset# #)) relativePayloadOffset# =
      let !(# bitmap#, size# #) = getterFun# stackFrame# closureOffset#
          bes = toBitmapEntries (StackFrameIter (# stackFrame#, plusWord# closureOffset# relativePayloadOffset# #))(W# bitmap#) (W# size#)
          payloads = map toBitmapPayload bes
      in
        payloads

-- TODO: Negative offsets won't work! Consider using Word
getClosure :: StackFrameIter -> Int -> CL.Closure
getClosure sfi relativeOffset = toClosure (unpackClosureReferencedByFrame# (intToWord# relativeOffset)) sfi

-- TODO: Negative offsets won't work! Consider using Word
getHalfWord :: StackFrameIter -> Int -> Word
getHalfWord (StackFrameIter (# s#, i# #)) relativeOffset = W# (getHalfWord# s# i# (intToWord# relativeOffset))

-- TODO: Negative offsets won't work! Consider using Word
getWord :: StackFrameIter -> Int -> Word
getWord (StackFrameIter (# s#, i# #)) relativeOffset = W# (getWord# s# i# (intToWord# relativeOffset))

unpackStackFrameIter :: StackFrameIter -> StackFrame
unpackStackFrameIter sfi@(StackFrameIter (# s#, i# #)) =
  case (toEnum . fromIntegral) (W# (getInfoTableType# s# i#)) of
     RET_BCO -> let
        instrs' = getClosure sfi offsetStgRetBCOFrameInstrs
        literals' = getClosure sfi offsetStgRetBCOFrameLiterals
        ptrs' = getClosure sfi offsetStgRetBCOFramePtrs
        arity' = getHalfWord sfi offsetStgRetBCOFrameArity
        size' = getHalfWord sfi offsetStgRetBCOFrameSize
        payload' = decodeLargeBitmap getBCOLargeBitmap# sfi 2##
        in
       RetBCO {
        instrs = instrs',
          literals  = literals',
          ptrs = ptrs',
          arity = arity',
          size = size',
          payload = payload'
              }
     RET_SMALL -> let payloads = decodeSmallBitmap getSmallBitmap# sfi 1##
                      special# = getRetSmallSpecialType# s# i#
                      special = (toEnum . fromInteger . toInteger) (W# special#)
                  in
                    RetSmall special payloads
     RET_BIG -> let payloads = decodeLargeBitmap getLargeBitmap# sfi 1##
                in
                  RetBig payloads
     RET_FUN -> let
        t = (toEnum . fromInteger . toInteger) (W# (getRetFunType# s# i#))
        size = getWord sfi offsetStgRetFunFrameSize
        fun = getClosure sfi offsetStgRetFunFrameFun
        payload =
          case t of
              ARG_GEN_BIG ->
                let
                  payloads = decodeLargeBitmap getRetFunLargeBitmap# sfi 2##
                in
                  payloads
              _ ->
                let
                  payloads = decodeSmallBitmap getRetFunSmallBitmap# sfi 2##
                in
                  payloads
       in
        RetFun t size fun payload
     -- TODO: Decode update frame type
     UPDATE_FRAME -> let
        c = getClosure sfi offsetStgUpdateFrameUpdatee
        !t = (toEnum . fromInteger . toInteger) (W# (getUpdateFrameType# s# i#))
       in
        UpdateFrame t c
     CATCH_FRAME -> let
        c = getClosure sfi offsetStgCatchFrameHandler
        -- TODO: Replace with getWord# expression
        exceptionsBlocked = W# (getCatchFrameExceptionsBlocked# s# i#)
       in
        CatchFrame exceptionsBlocked c
     UNDERFLOW_FRAME -> let
          nextChunk# = getUnderflowFrameNextChunk# s# i#
        in
          UnderflowFrame (StackSnapshot nextChunk#)
     STOP_FRAME ->  StopFrame
     ATOMICALLY_FRAME -> let
          c = getClosure sfi offsetStgAtomicallyFrameCode
          r = getClosure sfi offsetStgAtomicallyFrameResult
       in
         AtomicallyFrame c r
     CATCH_RETRY_FRAME ->  let
        running_alt_code = getWord sfi offsetStgCatchRetryFrameRunningAltCode
        first_code = getClosure sfi offsetStgCatchRetryFrameRunningFirstCode
        alt_code = getClosure sfi offsetStgCatchRetryFrameRunningAltCode
       in
         CatchRetryFrame running_alt_code first_code alt_code
     CATCH_STM_FRAME -> let
          c = getClosure sfi offsetStgCatchSTMFrameCode
          h = getClosure sfi offsetStgCatchSTMFrameHandler
        in
          CatchStmFrame c h
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

data BitmapPayload = Closure CL.Closure | Primitive Word

instance Show BitmapPayload where
  show (Primitive w) = "Primitive " ++ show w
  show (Closure ptr) = "Closure " ++ show ptr -- showAddr# addr#

-- TODO There are likely more. See MiscClosures.h
data SpecialRetSmall =
  -- TODO: Shoudn't `None` be better `Maybe ...`?
  None |
  ApV |
  ApF |
  ApD |
  ApL |
  ApN |
  ApP |
  ApPP |
  ApPPP |
  ApPPPP |
  ApPPPPP |
  ApPPPPPP |
  RetV |
  RetP |
  RetN |
  RetF |
  RetD |
  RetL |
  RestoreCCCS |
  RestoreCCCSEval
  deriving (Enum, Eq, Show)

data UpdateFrameType =
  NormalUpdateFrame |
  BhUpdateFrame |
  MarkedUpdateFrame
  deriving (Enum, Eq, Show)

data StackFrame =
  UpdateFrame { knownUpdateFrameType :: UpdateFrameType, updatee :: CL.Closure } |
  CatchFrame { exceptions_blocked :: Word,  handler :: CL.Closure } |
  CatchStmFrame { code :: CL.Closure, handler :: CL.Closure  } |
  CatchRetryFrame {running_alt_code :: Word, first_code :: CL.Closure, alt_code :: CL.Closure} |
  AtomicallyFrame { code :: CL.Closure, result :: CL.Closure} |
  UnderflowFrame { nextChunk:: StackSnapshot } |
  StopFrame |
  RetSmall { knownRetSmallType :: SpecialRetSmall, payload :: [BitmapPayload]} |
  RetBig { payload :: [BitmapPayload] } |
  RetFun { retFunType :: RetFunType, size :: Word, fun :: CL.Closure, payload :: [BitmapPayload]} |
  RetBCO {
  -- TODO: Add pre-defined BCO closures (like knownUpdateFrameType)
          instrs :: CL.Closure,
          literals :: CL.Closure,
          ptrs :: CL.Closure,
          arity :: Word,
          size :: Word,
          payload :: [BitmapPayload]
         }
  deriving (Show)

data RetFunType =
      ARG_GEN     |
      ARG_GEN_BIG |
      ARG_BCO     |
      ARG_NONE    |
      ARG_N       |
      ARG_P       |
      ARG_F       |
      ARG_D       |
      ARG_L       |
      ARG_V16     |
      ARG_V32     |
      ARG_V64     |
      ARG_NN      |
      ARG_NP      |
      ARG_PN      |
      ARG_PP      |
      ARG_NNN     |
      ARG_NNP     |
      ARG_NPN     |
      ARG_NPP     |
      ARG_PNN     |
      ARG_PNP     |
      ARG_PPN     |
      ARG_PPP     |
      ARG_PPPP    |
      ARG_PPPPP   |
      ARG_PPPPPP  |
      ARG_PPPPPPP |
      ARG_PPPPPPPP
      deriving (Show, Eq, Enum)

#if defined(DEBUG)
foreign import ccall "belchStack" belchStack# :: StackSnapshot# -> IO ()

belchStack :: StackSnapshot -> IO ()
belchStack (StackSnapshot s#) = belchStack# s#
#endif

decodeStack :: StackSnapshot -> IO [StackFrame]
decodeStack s = do
#if defined(DEBUG)
  belchStack s
#endif
  pure $ decodeStack' s

decodeStack' :: StackSnapshot -> [StackFrame]
decodeStack' s = unpackStackFrameIter (stackHead s) : go (advanceStackFrameIter (stackHead s))
  where
    go :: Maybe StackFrameIter -> [StackFrame]
    go Nothing = []
    go (Just sfi) = unpackStackFrameIter sfi : go (advanceStackFrameIter sfi)

#else
module GHC.Exts.DecodeStack where
#endif
