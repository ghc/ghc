{-# LANGUAGE CPP #-}
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

-- TODO: Find better place than top level. Re-export from top-level?
module GHC.Exts.DecodeStack where

#if MIN_VERSION_base(4,17,0)
import Data.Bits
import Foreign

import Prelude
import GHC.Stack.CloneStack
import GHC.Exts.Heap
import GHC.Ptr
import Debug.Trace
import GHC.Exts
import qualified GHC.Exts.Heap.FFIClosures as FFIClosures
import Numeric
import qualified GHC.Exts.Heap.Closures as CL
import GHC.Exts.Heap.StackFFI as StackFFI
import Data.Bits

import GHC.Exts.Heap (Closure)
import GHC.Exts.Heap.StackFFI (bitsInWord)
import GHC.Exts.Heap.Closures (closureSize)
import System.Mem (performMajorGC)

type StackFrameIter# = (#
                          -- | StgStack
                          StackSnapshot#,
                          -- | offset in machine words
                          Word#
                        #)

data StackFrameIter = StackFrameIter StackFrameIter#

-- | Get an interator starting with the top-most stack frame
stackHead :: StackSnapshot -> StackFrameIter
stackHead (StackSnapshot s) = StackFrameIter (# s , 0## #) -- GHC stacks are never empty

foreign import prim "advanceStackFrameIterzh" advanceStackFrameIter# :: StackSnapshot# -> Word# -> (# StackSnapshot#, Word#, Int# #)

-- | Advance iterator to the next stack frame (if any)
advanceStackFrameIter :: StackFrameIter -> Maybe StackFrameIter
advanceStackFrameIter (StackFrameIter (# s, i #)) = let (# s', i', hasNext #) = advanceStackFrameIter# s i in
  if (I# hasNext) > 0 then Just $ StackFrameIter (# s', i' #)
  else Nothing

foreign import prim "getInfoTableTypezh" getInfoTableType# :: StackSnapshot# -> Word# -> Word#

foreign import prim "getSmallBitmapzh" getSmallBitmap# :: StackSnapshot# -> Word# -> (# Word#, Word# #)

data BitmapEntry = BitmapEntry {
    closureFrame :: StackFrameIter,
    isPrimitive :: Bool
  } deriving (Show)

toBitmapEntries :: StackFrameIter -> Word -> Word -> [BitmapEntry]
toBitmapEntries _ _ 0 = []
toBitmapEntries sfi@(StackFrameIter(# s, i #) bitmap size = BitmapEntry {
  closureFrame = sfi,
  isPrimitive = (bitmap .&. 1) == 0
                                            } : toBitmapEntries (StackFrameIter (# s , i + 1 #)) (bitmap `shiftR` 1) (size - 1)

unpackStackFrameIter :: StackFrameIter -> StackFrame
unpackStackFrameIter (StackFrameIter (# s, i #)) =
  case (toEnum . fromIntegral) (W# (getInfoTableType# s i)) of
     RET_BCO -> RetBCO
     RET_SMALL -> let (# bitmap#, size# #) = getSmallBitmap# s i
                  in
                    RetSmall None []
     RET_BIG ->  RetBig []
     RET_FUN ->  RetFun
     UPDATE_FRAME ->  UpdateFrame
     CATCH_FRAME ->  CatchFrame
     UNDERFLOW_FRAME ->  UnderflowFrame
     STOP_FRAME ->  StopFrame
     ATOMICALLY_FRAME ->  AtomicallyFrame
     CATCH_RETRY_FRAME ->  CatchRetryFrame
     CATCH_STM_FRAME ->  CatchStmFrame
     x -> error $ "Unexpected closure type on stack: " ++ show x

-- TODO: Is the function type below needed? (Was proposed by Ben)
-- derefStackPtr :: StackSnapshot# -> Int# -> a

foreign import prim "derefStackWordzh" derefStackWord# :: StackSnapshot# -> Int# -> Word#

data BitmapPayload = Closure CL.Closure | Primitive Word

instance Show BitmapPayload where
  show (Primitive w) = "Primitive " ++ show w
  show (Closure ptr) = "Closure " ++ show ptr -- showAddr# addr#

-- TODO There are likely more. See MiscClosures.h
data SpecialRetSmall =
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
  deriving (Enum, Eq,Show)

data StackFrame =
  UpdateFrame |
  CatchFrame |
  CatchStmFrame |
  CatchRetryFrame |
  AtomicallyFrame |
  UnderflowFrame |
  StopFrame |
  RetSmall SpecialRetSmall [BitmapPayload] |
  RetBig [BitmapPayload] |
  RetFun |
  RetBCO
  deriving (Show)

#if defined(DEBUG)
foreign import ccall "belchStack" belchStack :: StackSnapshot# -> IO ()
#endif

decodeStack :: StackSnapshot -> IO [StackFrame]
decodeStack s@(StackSnapshot s#) = do
#if defined(DEBUG)
  belchStack s#
#endif
  pure $ decodeStack' s

decodeStack' :: StackSnapshot -> [StackFrame]
decodeStack' s = unpackStackFrameIter (stackHead s) : go (advanceStackFrameIter (stackHead s))
  where
    go :: Maybe StackFrameIter -> [StackFrame]
    go Nothing = []
    go (Just sfi) = unpackStackFrameIter sfi : go (advanceStackFrameIter sfi)


-- foreign import ccall "stackFrameSizeW" stackFrameSizeW :: Addr# -> Word
-- foreign import ccall "getItbl" getItbl :: Addr# -> Ptr StgInfoTable
-- foreign import ccall "getSpecialRetSmall" getSpecialRetSmall :: Addr# -> Word
-- foreign import ccall "getBitmapSize" getBitmapSize :: Ptr StgInfoTable -> Word
-- foreign import ccall "getBitmapWord" getBitmapWord :: Ptr StgInfoTable -> Word
-- foreign import prim "stg_unpackClosurezh" unpackClosure_prim# :: Word# -> (# Addr#, ByteArray#, Array# b #)
-- foreign import ccall "getLargeBitmapPtr" getLargeBitmapPtr :: Ptr StgInfoTable -> Ptr LargeBitmap
-- #if defined(DEBUG)
-- foreign import ccall "belchStack" belchStack :: StackSnapshot# -> IO ()
-- #endif
--
-- decodeStack :: StackSnapshot -> IO [StackFrame]
-- decodeStack (StackSnapshot stack) = do
-- #if defined(DEBUG)
--   performMajorGC
--   belchStack stack
-- #endif
--   let (stgStackPtr :: Ptr FFIClosures.StackFields) = Ptr (unsafeCoerce# stack)
--   stgStack <- FFIClosures.peekStackFields stgStackPtr
--   traceM $ "stack_dirty  " ++ show (FFIClosures.stack_dirty  stgStack)
--   traceM $ "stack_marking  " ++ show (FFIClosures.stack_marking  stgStack)
--   traceM $ "stack_size " ++ show (FFIClosures.stack_size stgStack)
--   traceM $ "stack_sp  " ++ showAddr# (FFIClosures.stack_sp stgStack)
--   decodeStackChunks stgStack
--
-- decodeStackChunks :: FFIClosures.StackFields -> IO [StackFrame]
-- decodeStackChunks stgStack =
--   let
--     -- TODO: Use word size here, not just 8
--     stackSize = 8 * (FFIClosures.stack_size stgStack)
--     buttom = plusAddr# (FFIClosures.stack_stack stgStack) (integralToInt# stackSize)
--   in
--     decodeStackFrame buttom (FFIClosures.stack_sp stgStack)
--
-- decodeStackFrame :: Addr# -> Addr# -> IO [StackFrame]
-- -- TODO: Use ltAddr# ? (Does it even work?)
-- decodeStackFrame buttom sp | (addrToInt sp) >= (addrToInt buttom) = do
--               traceM $ "decodeStackFrame - buttom " ++ showAddr# buttom
--               traceM $ "decodeStackFrame - sp " ++ showAddr# sp
--               traceM "buttom reached"
--               pure []
-- decodeStackFrame buttom sp = do
-- --  traceM $ "decodeStackFrame - (addrToInt sp) >= (addrToInt buttom)" ++ show ((addrToInt sp) >= (addrToInt buttom))
-- --  traceM $ "decodeStackFrame - buttom " ++ showAddr# buttom
--   traceM $ "decodeStackFrame - sp " ++ showAddr# sp
--   frame <- toStackFrame sp
--   traceM $  "decodeStackFrame - frame " ++ show frame
--   -- TODO: This is probably not lazy and pretty ugly.
--   -- TODO: Use word size instead of 8
--   let
--     closureSize = stackFrameSizeW sp
--     closureSizeInBytes = 8 * closureSize
--     nextSp = plusAddr# sp (integralToInt# closureSizeInBytes)
--
--   traceM $ "decodeStackFrame - closureSize " ++ show closureSize ++ " sp " ++ showAddr# sp
--   traceM $ "decodeStackFrame - nextSp " ++ showAddr# nextSp
--   otherFrames <- decodeStackFrame buttom nextSp
--   return $ frame : otherFrames
--
-- toStackFrame :: Addr# -> IO StackFrame
-- toStackFrame sp = do
--   let itblPtr = getItbl sp
--   itbl <- peekItbl itblPtr
--   traceM $ "itbl " ++ show itbl
--   case tipe itbl of
--      RET_BCO -> pure RetBCO
--      RET_SMALL ->
--        let special = ((toEnum . fromInteger . toInteger) (getSpecialRetSmall sp))
--            -- TODO: Use word size here, not just 8
--            payloadAddr# = plusAddr# sp (toInt# 8)
--            bSize = getBitmapSize itblPtr
--            bWord = getBitmapWord itblPtr
--        in
--          do
--             payloads <- peekBitmapPayloadArray bSize bWord (Ptr payloadAddr#)
--             pure $ RetSmall special payloads
--      RET_BIG -> do
--        let pPtr = payloadPtr (Ptr sp)
--        traceM $ "toStackFrame - BIG_RET - pPtr " ++ show pPtr
--        let largeBitmapPtr = getLargeBitmapPtr itblPtr
--        largeBitmap <- peekStgLargeBitmap largeBitmapPtr
--        traceM $ "toStackFrame - BIG_RET - largeBitmap " ++ show largeBitmap
--        let entries = bitmapEntries pPtr largeBitmap
--        traceM $ "toStackFrame - BIG_RET - entries " ++ show entries
--        payloads <- mapM toClosure $ bitmapEntries pPtr largeBitmap
--        pure $ RetBig payloads
--      RET_FUN -> pure RetFun
--      UPDATE_FRAME -> pure UpdateFrame
--      CATCH_FRAME -> pure CatchFrame
--      UNDERFLOW_FRAME -> pure UnderflowFrame
--      STOP_FRAME -> pure StopFrame
--      ATOMICALLY_FRAME -> pure AtomicallyFrame
--      CATCH_RETRY_FRAME -> pure CatchRetryFrame
--      CATCH_STM_FRAME -> pure CatchStmFrame
--      _ -> error $ "Unexpected closure type on stack: " ++ show (tipe itbl)
--
-- toClosure :: BitmapEntry -> IO BitmapPayload
-- toClosure (BitmapEntry ptr isPrimitive) = if isPrimitive then
--           do
--             -- TODO: duplicated line with else branch
--             e <- peek ptr
--             pure $ Primitive e
--           else
--             do
--               e <- peek ptr
--               c <- getClosureDataFromHeapObject' (toWord# e)
--               pure $ Closure c
--
-- -- Idea:
-- -- 1. convert to list of (significant) bits
-- -- Convert to tupe (Ptr addr, closure type)
-- -- This is pure! And, should be easy to decode, debug and reuse.
-- data BitmapEntry = BitmapEntry {
--   closurePtr :: Ptr Word,
--   isPrimitive :: Bool
--                                } deriving (Show)
--
-- bitmapEntries :: Ptr Word -> LargeBitmap -> [BitmapEntry]
-- bitmapEntries payloadPtr (LargeBitmap size ws) =
--     map toBitmapEntry $ zip (bits size ws) [0..]
--   where
--     toBitmapEntry :: (Bool, Int) -> BitmapEntry
--     toBitmapEntry (b,i) = BitmapEntry {
--       closurePtr = plusPtr payloadPtr (i * (fromIntegral bytesInWord)),
--       isPrimitive = b
--                                       }
--
-- bits :: Word -> [Word] -> [Bool]
-- bits size ws = take (fromIntegral size) $ concat (map toBits ws)
--
-- toBits :: Word -> [Bool]
-- toBits w = go 0
--   where
--     go :: Int -> [Bool]
--     go b | b == (fromIntegral bitsInWord) = []
--     go b = (w .&. (1 `shiftL` b) == 1) : go (b + 1)
--
-- peekLargeBitmap :: Word -> [Word] -> Ptr Word -> IO [BitmapPayload]
-- peekLargeBitmap 0 _ _ = pure []
-- peekLargeBitmap _ [] _ = pure []
-- peekLargeBitmap bSize (w:ws) pPtr = do
--     payloads <- peekPayloadsFromLargeBitmapWord bSize w pPtr
--     -- TODO: Not tail-recursive, breaks lazyness
--     rest <- peekLargeBitmap remainingBitmapSize ws pPtr
--     pure $ payloads ++ rest
--   where
--     remainingBitmapSize = max 0 (bSize - bitsInWord)
--
-- peekPayloadsFromLargeBitmapWord :: Word -> Word -> Ptr Word -> IO [BitmapPayload]
-- peekPayloadsFromLargeBitmapWord bSize bWord ptr = go 0 []
--   where
--     go :: Word -> [BitmapPayload] -> IO [BitmapPayload]
--     go index acc | index >= bitsUsed = pure acc
--     go index acc = do
--                   e <- peekBitmapPayload ptr index bWord
--                   go (index + 1) (e:acc)
--     bitsUsed = min bitsInWord bSize
--
-- -- TODO: Use Ptr instead of Addr# (in all possible places)?
-- peekBitmapPayloadArray ::  Word -> Word -> Ptr Word -> IO [BitmapPayload]
-- peekBitmapPayloadArray bSize bWord ptr = go 0 []
--   where
--     go :: Word -> [BitmapPayload] -> IO [BitmapPayload]
--     go index acc | index >= bSize = pure acc
--     go index acc = do
--                   e <- peekBitmapPayload ptr index bWord
--                   go (index + 1) (e:acc)
--
-- -- | Fetch a single closure payload
-- -- As the decission about the value to marshall
-- -- to depends on the bitmap, only a `Word` is peeked.
-- peekBitmapPayload :: Ptr Word -> Word -> Word -> IO BitmapPayload
-- peekBitmapPayload ptr index bitmapWord = do
--         traceM $ "peekBitmapPayload - ptr " ++ show ptr
--         traceM $ "peekBitmapPayload - index " ++ show index
--         e <- (peekElemOff ptr i :: IO Word)
--         if isClosure then
--           do
--             c <- getClosureDataFromHeapObject' (toWord# e)
--             pure $ Closure c
--         else
--             pure $ Primitive e
--   where
--    isClosure :: Bool
--    isClosure = (bitmapWord .&. mask) == 0
--    mask :: Word
--    mask = 1 `shiftL` i
--    i :: Int
--    i = (fromInteger.toInteger) index
--
-- getClosureDataFromHeapObject'
--     :: Word#
--     -- ^ Heap object to decode.
--     -> IO Closure
--     -- ^ Heap representation of the closure.
-- getClosureDataFromHeapObject' x = do
--     case unpackClosure_prim# x of
--         (# infoTableAddr, heapRep, pointersArray #) -> do
--             let infoTablePtr = Ptr infoTableAddr
--                 ptrList = [case indexArray# pointersArray i of
--                                 (# ptr #) -> Box ptr
--                             | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
--                             ]
--
--             infoTable <- peekItbl infoTablePtr
--             case tipe infoTable of
--                 TSO   -> pure $ UnsupportedClosure infoTable
--                 STACK -> pure $ UnsupportedClosure infoTable
--                 _ -> getClosureDataFromHeapRep heapRep infoTablePtr ptrList
--
--
-- -- | Converts to 'Int#'
-- -- An 'Integral' can be bigger than the domain of 'Int#'. This function drops
-- -- the additional bits. So, the caller should better make sure that this
-- -- conversion fits.
-- integralToInt# :: Integral a => a -> Int#
-- integralToInt# w = toInt# $ (fromInteger . toInteger) w
--
-- -- | Unbox 'Int#' from 'Int'
-- toInt# :: Int -> Int#
-- toInt# (I# i) = i
--
-- toWord# :: Word -> Word#
-- toWord# (W# w#) = w#
--
-- showAddr# :: Addr# -> String
-- showAddr# addr# = showHex (addrToInt addr#) ""
--
-- addrToInt:: Addr# -> Int
-- addrToInt addr# = I# (addr2Int# addr#)
--
#endif
