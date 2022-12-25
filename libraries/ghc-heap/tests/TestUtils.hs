{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}

module TestUtils
  ( assertEqual,
    assertThat,
    assertStackInvariants,
    unbox
  )
where

import Data.Array.Byte
import GHC.Exts
import GHC.Exts.DecodeStack
import GHC.Exts.Heap
import GHC.Exts.Heap.Closures
import GHC.Records
import GHC.Stack (HasCallStack)
import GHC.Stack.CloneStack
import Unsafe.Coerce (unsafeCoerce)

assertEqual :: (HasCallStack, Monad m, Show a, Eq a) => a -> a -> m ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = pure ()

assertThat :: (HasCallStack, Monad m) => String -> (a -> Bool) -> a -> m ()
assertThat s f a = if f a then pure () else error s

assertStackInvariants :: (HasCallStack, Monad m) => StackSnapshot -> [Closure] -> m ()
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

instance ToClosureTypes Closure where
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

-- TODO: Can probably be simplified once all stack closures have into tables attached.
stackFrameToClosureTypes :: Closure -> [ClosureType]
stackFrameToClosureTypes = getClosureTypes
  where
    getClosureTypes :: Closure -> [ClosureType]
    -- Stack frame closures
    getClosureTypes (UpdateFrame {updatee, ..}) = UPDATE_FRAME : getClosureTypes (unbox updatee)
    getClosureTypes (CatchFrame {handler, ..}) = CATCH_FRAME : getClosureTypes (unbox handler)
    getClosureTypes (CatchStmFrame {catchFrameCode, handler}) = CATCH_STM_FRAME : getClosureTypes (unbox catchFrameCode) ++ getClosureTypes (unbox handler)
    getClosureTypes (CatchRetryFrame {first_code, alt_code, ..}) = CATCH_RETRY_FRAME : getClosureTypes (unbox first_code) ++ getClosureTypes (unbox alt_code)
    getClosureTypes (AtomicallyFrame {atomicallyFrameCode, result}) = ATOMICALLY_FRAME : getClosureTypes (unbox atomicallyFrameCode) ++ getClosureTypes (unbox result)
    getClosureTypes (UnderflowFrame {..}) = [UNDERFLOW_FRAME]
    getClosureTypes StopFrame = [STOP_FRAME]
    getClosureTypes (RetSmall {payload, ..}) = RET_SMALL : getBitmapClosureTypes payload
    getClosureTypes (RetBig {payload}) = RET_BIG : getBitmapClosureTypes payload
    getClosureTypes (RetFun {retFunFun, retFunPayload, ..}) = RET_FUN : getClosureTypes (unbox retFunFun) ++ getBitmapClosureTypes retFunPayload
    getClosureTypes (RetBCO {bcoInstrs, bcoLiterals, bcoPtrs, bcoPayload, ..}) =
      RET_BCO : getClosureTypes (unbox bcoInstrs) ++ getClosureTypes (unbox bcoLiterals) ++ getClosureTypes (unbox bcoPtrs) ++ getBitmapClosureTypes bcoPayload
    -- Other closures
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

    getBitmapClosureTypes :: [Box] -> [ClosureType]
    getBitmapClosureTypes bps =
      reverse $
        foldl
          ( \acc p -> case unbox p of
              UnknownTypeWordSizedPrimitive _ -> acc
              c -> getClosureTypes c ++ acc
          )
          []
          bps

unbox :: Box -> Closure
unbox (Box c) = unsafeCoerce c
unbox (DecodedClosureBox c) = c
