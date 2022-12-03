{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}

module TestUtils
  ( assertEqual,
    assertThat,
    assertStackInvariants
  )
where

import Data.Array.Byte
import GHC.Exts
import GHC.Exts.DecodeStack
import GHC.Exts.Heap
import GHC.Records
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack

assertEqual :: (HasCallStack, Monad m, Show a, Eq a) => a -> a -> m ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = pure ()

assertThat :: (HasCallStack, Monad m) => String -> (a -> Bool) -> a -> m ()
assertThat s f a = if f a then pure () else error s

assertStackInvariants :: (HasCallStack, Monad m) => StackSnapshot -> [StackFrame] -> m ()
assertStackInvariants stack decodedStack = do
  assertThat
    "Last frame is stop frame"
    ( \case
        StopFrame -> True
        _ -> False
    )
    (last decodedStack)
  assertEqual
    (toClosureTypes decodedStack)
    (toClosureTypes stack)

class ToClosureTypes a where
  toClosureTypes :: a -> [ClosureType]

instance ToClosureTypes StackSnapshot where
  toClosureTypes = stackSnapshotToClosureTypes . foldStackToArrayClosure

instance ToClosureTypes StackFrame where
  toClosureTypes = stackFrameToClosureTypes

instance ToClosureTypes a => ToClosureTypes [a] where
  toClosureTypes = concatMap toClosureTypes

foreign import ccall "foldStackToArrayClosure" foldStackToArrayClosure# :: StackSnapshot# -> ByteArray#

foldStackToArrayClosure :: StackSnapshot -> ByteArray
foldStackToArrayClosure (StackSnapshot s#) = ByteArray (foldStackToArrayClosure# s#)

stackSnapshotToClosureTypes :: ByteArray -> [ClosureType]
stackSnapshotToClosureTypes = wordsToClosureTypes . toWords
  where
    toWords :: ByteArray -> [Word]
    toWords ba@(ByteArray b#) =
      let s = I# (sizeofByteArray# b#)
       in -- TODO: Adjust 8 to machine word size
          [W# (indexWordArray# b# (toInt# i)) | i <- [0 .. maxWordIndex (ba)]]
      where
        maxWordIndex :: ByteArray -> Int
        maxWordIndex (ByteArray ba#) =
          let s = I# (sizeofByteArray# ba#)
              words = s `div` 8
           in case words of
                w | w == 0 -> error "ByteArray contains no content!"
                w -> w - 1

    wordsToClosureTypes :: [Word] -> [ClosureType]
    wordsToClosureTypes = map (toEnum . fromIntegral)

toInt# :: Int -> Int#
toInt# (I# i#) = i#

stackFrameToClosureTypes :: StackFrame -> [ClosureType]
stackFrameToClosureTypes sf =
  case sf of
    (UpdateFrame {updatee, ..}) -> UPDATE_FRAME : getClosureTypes updatee
    (CatchFrame {handler, ..}) -> CATCH_FRAME : getClosureTypes handler
    (CatchStmFrame {code, handler}) -> CATCH_STM_FRAME : getClosureTypes code ++ getClosureTypes handler
    (CatchRetryFrame {first_code, alt_code, ..}) -> CATCH_RETRY_FRAME : getClosureTypes first_code ++ getClosureTypes alt_code
    (AtomicallyFrame {code, result}) -> ATOMICALLY_FRAME : getClosureTypes code ++ getClosureTypes result
    (UnderflowFrame {..}) -> [UNDERFLOW_FRAME]
    StopFrame -> [STOP_FRAME]
    (RetSmall {payload, ..}) -> RET_SMALL : getBitmapClosureTypes payload
    (RetBig {payload}) -> RET_BIG : getBitmapClosureTypes payload
    (RetFun {fun, payload, ..}) -> RET_FUN : getClosureTypes fun ++ getBitmapClosureTypes payload
    (RetBCO {instrs, literals, ptrs, payload, ..}) ->
      RET_BCO : getClosureTypes instrs ++ getClosureTypes literals ++ getClosureTypes ptrs ++ getBitmapClosureTypes payload
  where
    getClosureTypes :: Closure -> [ClosureType]
    getClosureTypes (ConstrClosure {info, ..}) = [tipe info]
    getClosureTypes (FunClosure {info, ..}) = [tipe info]
    getClosureTypes (ThunkClosure {info, ..}) = [tipe info]
    getClosureTypes (SelectorClosure {info, ..}) = [tipe info]
    getClosureTypes (PAPClosure {info, ..}) = [tipe info]
    getClosureTypes (APClosure {info, ..}) = [tipe info]
    getClosureTypes (APStackClosure {info, ..}) = [tipe info]
    getClosureTypes (IndClosure {info, ..}) = [tipe info]
    getClosureTypes (BCOClosure {info, ..}) = [tipe info]
    getClosureTypes (BlackholeClosure {info, ..}) = [tipe info]
    getClosureTypes (ArrWordsClosure {info, ..}) = [tipe info]
    getClosureTypes (MutArrClosure {info, ..}) = [tipe info]
    getClosureTypes (SmallMutArrClosure {info, ..}) = [tipe info]
    getClosureTypes (MVarClosure {info, ..}) = [tipe info]
    getClosureTypes (IOPortClosure {info, ..}) = [tipe info]
    getClosureTypes (MutVarClosure {info, ..}) = [tipe info]
    getClosureTypes (BlockingQueueClosure {info, ..}) = [tipe info]
    getClosureTypes (WeakClosure {info, ..}) = [tipe info]
    getClosureTypes (TSOClosure {info, ..}) = [tipe info]
    getClosureTypes (StackClosure {info, ..}) = [tipe info]
    getClosureTypes (OtherClosure {info, ..}) = [tipe info]
    getClosureTypes (UnsupportedClosure {info, ..}) = [tipe info]
    getClosureTypes _ = []

    getBitmapClosureTypes :: [BitmapPayload] -> [ClosureType]
    getBitmapClosureTypes bps =
      reverse $
        foldl
          ( \acc p -> case p of
              (Closure c) -> getClosureTypes c ++ acc
              (Primitive _) -> acc
          )
          []
          bps
