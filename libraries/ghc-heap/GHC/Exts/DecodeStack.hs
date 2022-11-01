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

foreign import prim "getRetFunLargeBitmapzh" getRetFunLargeBitmap# :: StackSnapshot# -> Word# -> (# ByteArray#, Word# #)

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: StackSnapshot# -> Word# -> (# Word#, Word#, Word# #)

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


unpackStackFrameIter :: StackFrameIter -> StackFrame
unpackStackFrameIter sfi@(StackFrameIter (# s#, i# #)) =
  case (toEnum . fromIntegral) (W# (getInfoTableType# s# i#)) of
     RET_BCO -> RetBCO
     RET_SMALL -> let !(# bitmap#, size#, special# #) = getSmallBitmap# s# i#
                      bes = toBitmapEntries (StackFrameIter (# s#, plusWord# i# 1## #))(W# bitmap#) (W# size#)
                      payloads = map toBitmapPayload bes
                      special = (toEnum . fromInteger . toInteger) (W# special#)
                  in
                    RetSmall special payloads
     RET_BIG -> let !(# bitmapArray#, size# #) = getLargeBitmap# s# i#
                    bitmapWords :: [Word] = foldrByteArray (\w acc -> W# w : acc) [] bitmapArray#
                    bes = wordsToBitmapEntries (StackFrameIter (# s#, plusWord# i# 1## #)) (trace ("bitmapWords" ++ show bitmapWords) bitmapWords) (trace ("XXX size " ++ show (W# size#))(W# size#))
                    payloads = map toBitmapPayload bes
                in
                  RetBig payloads
     RET_FUN -> let
        t = getRetFunType# s# i#
        size =  W# (getWord# s# i# (intToWord# offsetStgRetFunFrameSize))
        fun = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgRetFunFrameFun)) sfi
        payload :: [CL.Closure]
       in
        RetFun t size fun payload
     -- TODO: Decode update frame type
     UPDATE_FRAME -> let
        c = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgUpdateFrameUpdatee)) sfi
        !t = (toEnum . fromInteger . toInteger) (W# (getUpdateFrameType# s# i#))
       in
        UpdateFrame t c
     CATCH_FRAME -> let
        c = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgCatchFrameHandler)) sfi
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
          c = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgAtomicallyFrameCode)) sfi
          r = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgAtomicallyFrameResult)) sfi
       in
         AtomicallyFrame c r
     CATCH_RETRY_FRAME ->  let
        running_alt_code = W# (getWord# s# i# (intToWord# offsetStgCatchRetryFrameRunningAltCode))
        first_code = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgCatchRetryFrameRunningFirstCode)) sfi
        alt_code = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgCatchRetryFrameRunningAltCode)) sfi
       in
         CatchRetryFrame running_alt_code first_code alt_code
     CATCH_STM_FRAME -> let
          c = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgCatchSTMFrameCode)) sfi
          h = toClosure (unpackClosureReferencedByFrame# (intToWord# offsetStgCatchSTMFrameHandler)) sfi
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

foreign import prim "unpackUpdateeFromUpdateFramezh" unpackUpdateeFromUpdateFrame# :: StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# b #)

foreign import prim "derefStackWordzh" derefStackWord# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getUpdateFrameTypezh" getUpdateFrameType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "unpackClosureReferencedByFramezh" unpackClosureReferencedByFrame# :: Word# -> StackSnapshot# -> Word# -> (# Addr#, ByteArray#, Array# b #)

foreign import prim "getCatchFrameExceptionsBlockedzh" getCatchFrameExceptionsBlocked#  :: StackSnapshot# -> Word# -> Word#

foreign import prim "getUnderflowFrameNextChunkzh" getUnderflowFrameNextChunk# :: StackSnapshot# -> Word# -> StackSnapshot#

foreign import prim "getWordzh" getWord# ::  StackSnapshot# -> Word# -> Word# -> Word#

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
  RetFun { retFunType :: RetFunType, size :: Word, fun :: CL.Closure, payload :: [CL.Closure]} |
  RetBCO
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
