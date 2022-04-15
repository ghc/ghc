{-# LANGUAGE CPP #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- TODO: Find better place than top level. Re-export from top-level?
module GHC.Exts.DecodeStack where

#if MIN_VERSION_base(4,16,0)
import Prelude
import GHC.Stack.CloneStack
import GHC.Exts.Heap
import GHC.Ptr
import Debug.Trace
import GHC.Exts
import qualified GHC.Exts.Heap.FFIClosures as FFIClosures
import Numeric

data StackFrame =
  UpdateFrame |
  CatchFrame |
  CatchStmFrame |
  CatchRetryFrame |
  AtomicallyFrame |
  UnderflowFrame |
  StopFrame |
  RetSmall |
  RetBig |
  RetFun |
  RetBCO
  deriving (Eq, Show)

foreign import ccall "stackFrameSizeW" stackFrameSizeW :: Addr# -> Word
foreign import ccall "getItbl" getItbl :: Addr# -> Ptr StgInfoTable

decodeStack :: StackSnapshot -> IO [StackFrame]
decodeStack (StackSnapshot stack) = do
  let (stgStackPtr :: Ptr FFIClosures.StackFields) = Ptr (unsafeCoerce# stack)
  stgStack <- FFIClosures.peekStackFields stgStackPtr
  traceM $ "stack_dirty  " ++ show (FFIClosures.stack_dirty  stgStack)
  traceM $ "stack_marking  " ++ show (FFIClosures.stack_marking  stgStack)
  traceM $ "stack_size " ++ show (FFIClosures.stack_size stgStack)
  traceM $ "stack_sp  " ++ showAddr# (FFIClosures.stack_sp stgStack)
  decodeStackChunks stgStack

decodeStackChunks :: FFIClosures.StackFields -> IO [StackFrame]
decodeStackChunks stgStack =
  let
    -- TODO: Use word size here, not just 8
    stackSize = 8 * (FFIClosures.stack_size stgStack)
    buttom = plusAddr# (FFIClosures.stack_stack stgStack) (integralToInt# stackSize)
  in
    decodeStackFrame buttom (FFIClosures.stack_sp stgStack)

decodeStackFrame :: Addr# -> Addr# -> IO [StackFrame]
-- TODO: Use ltAddr# ? (Does it even work?)
decodeStackFrame buttom sp | (addrToInt sp) >= (addrToInt buttom) = do
              traceM $ "decodeStackFrame - buttom " ++ showAddr# buttom
              traceM $ "decodeStackFrame - sp " ++ showAddr# sp
              traceM "buttom reached"
              pure []
decodeStackFrame buttom sp = do
  traceM $ "decodeStackFrame - (addrToInt sp) >= (addrToInt buttom)" ++ show ((addrToInt sp) >= (addrToInt buttom))
  traceM $ "decodeStackFrame - buttom " ++ showAddr# buttom
  traceM $ "decodeStackFrame - sp " ++ showAddr# sp
  frame <- toStackFrame sp
  traceM $  "decodeStackFrame - frame " ++ show frame
  -- TODO: This is probably not lazy and pretty ugly.
  -- TODO: Use word size instead of 8
  let
    closureSize = stackFrameSizeW sp
    closureSizeInBytes = 8 * closureSize
    nextSp = plusAddr# sp (integralToInt# closureSizeInBytes)
  traceM $ "decodeStackFrame - nextSp " ++ showAddr# nextSp
  otherFrames <- decodeStackFrame buttom nextSp
  return $ frame : otherFrames

toStackFrame :: Addr# -> IO StackFrame
toStackFrame sp = do
  let itblPtr = getItbl sp
      closureSize = stackFrameSizeW sp
  itbl <- peekItbl itblPtr
  traceM $ "itbl " ++ show itbl
  pure $ case tipe itbl of
     RET_BCO -> RetBCO
     RET_SMALL -> RetSmall
     RET_BIG -> RetBig
     RET_FUN -> RetFun
     UPDATE_FRAME -> UpdateFrame
     CATCH_FRAME -> CatchFrame
     UNDERFLOW_FRAME ->UnderflowFrame
     STOP_FRAME -> StopFrame
     ATOMICALLY_FRAME -> AtomicallyFrame
     CATCH_RETRY_FRAME -> CatchRetryFrame
     CATCH_STM_FRAME -> CatchStmFrame
     _ -> error $ "Unexpected closure type on stack: " ++ show (tipe itbl)

-- | Converts to 'Int#'
-- An 'Integral' can be bigger than the domain of 'Int#'. This function drops
-- the additional bits. So, the caller should better make sure that this
-- conversion fits.
integralToInt# :: Integral a => a -> Int#
integralToInt# w = toInt# $ (fromInteger . toInteger) w

-- | Unbox 'Int#' from 'Int'
toInt# :: Int -> Int#
toInt# (I# i) = i

showAddr# :: Addr# -> String
showAddr# addr# = showHex (addrToInt addr#) ""

addrToInt:: Addr# -> Int
addrToInt addr# = I# (addr2Int# addr#)
#endif
