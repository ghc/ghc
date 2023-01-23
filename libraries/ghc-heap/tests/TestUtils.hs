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
    unbox,
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
import Debug.Trace
import Data.Foldable
import Control.Monad.IO.Class

assertEqual :: (HasCallStack, Monad m, Show a, Eq a) => a -> a -> m ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = pure ()

assertThat :: (HasCallStack, Monad m) => String -> (a -> Bool) -> a -> m ()
assertThat s f a = if f a then pure () else error s

assertStackInvariants :: (HasCallStack, MonadIO m) => StackSnapshot -> [Closure] -> m ()
assertStackInvariants stack decodedStack = do
  assertThat
    "Last frame is stop frame"
    ( \case
        StopFrame info -> tipe info == STOP_FRAME
        _ -> False
    )
    (last decodedStack)
  ts1 <- liftIO $ toClosureTypes decodedStack
  ts2 <- liftIO $ toClosureTypes stack
  assertEqual ts1 ts2

class ToClosureTypes a where
  toClosureTypes ::  a -> IO [ClosureType]

instance ToClosureTypes StackSnapshot where
  toClosureTypes = pure . stackSnapshotToClosureTypes . foldStackToArrayClosure

instance ToClosureTypes Closure where
  toClosureTypes = stackFrameToClosureTypes

instance ToClosureTypes a => ToClosureTypes [a] where
  toClosureTypes cs = concat <$> mapM toClosureTypes cs

foreign import ccall "foldStackToArrayClosure" foldStackToArrayClosure# :: StackSnapshot# -> ByteArray#

foldStackToArrayClosure :: StackSnapshot -> ByteArray
foldStackToArrayClosure (StackSnapshot s#) = ByteArray (foldStackToArrayClosure# s#)

foreign import ccall "bytesInWord" bytesInWord# :: Word

stackSnapshotToClosureTypes :: ByteArray -> [ClosureType]
stackSnapshotToClosureTypes = wordsToClosureTypes . toWords
  where
    toWords :: ByteArray -> [Word]
    toWords ba@(ByteArray b#) =
      let s = I# (sizeofByteArray# b#)
       in [W# (indexWordArray# b# (toInt# i)) | i <- [0 .. maxWordIndex (ba)]]
      where
        maxWordIndex :: ByteArray -> Int
        maxWordIndex (ByteArray ba#) =
          let s = I# (sizeofByteArray# ba#)
              words = s `div` fromIntegral bytesInWord#
           in case words of
                w | w == 0 -> error "ByteArray contains no content!"
                w -> w - 1

    wordsToClosureTypes :: [Word] -> [ClosureType]
    wordsToClosureTypes = map (toEnum . fromIntegral)

toInt# :: Int -> Int#
toInt# (I# i#) = i#

-- TODO: Can probably be simplified once all stack closures have into tables attached.
stackFrameToClosureTypes :: Closure -> IO [ClosureType]
stackFrameToClosureTypes = getClosureTypes
  where
    getClosureTypes :: Closure -> IO [ClosureType]
    -- Stack frame closures
    getClosureTypes (UpdateFrame {info, updatee, ..}) = do
      u <- unbox updatee
      ts <- getClosureTypes u
      pure $ tipe info : ts
    getClosureTypes (CatchFrame {info, handler, ..}) = do
      h <- unbox handler
      ts <- getClosureTypes h
      pure $ tipe info : ts
    getClosureTypes (CatchStmFrame {info, catchFrameCode, handler}) = do
      c <- unbox catchFrameCode
      h <- unbox handler
      ts1 <- getClosureTypes c
      ts2 <- getClosureTypes h
      pure $ tipe info : ts1 ++ ts2
    getClosureTypes (CatchRetryFrame {info, first_code, alt_code, ..}) = do
      a <- unbox alt_code
      f <- unbox first_code
      ts1 <- getClosureTypes f
      ts2 <- getClosureTypes a
      pure $ tipe info : ts1 ++ ts2
    getClosureTypes (AtomicallyFrame {info, atomicallyFrameCode, result}) = do
      r <- unbox result
      a <- unbox atomicallyFrameCode
      ts1 <- getClosureTypes a
      ts2 <- getClosureTypes r
      pure $ tipe info : ts1 ++ ts2
    getClosureTypes (UnderflowFrame {..}) = pure [tipe info]
    getClosureTypes (StopFrame info) = pure [tipe info]
    getClosureTypes (RetSmall {info, payload, ..}) = do
      ts <- getBitmapClosureTypes payload
      pure $ tipe info : ts
    getClosureTypes (RetBig {info, payload}) = do
      ts <- getBitmapClosureTypes payload
      pure $ tipe info : ts
    getClosureTypes (RetFun {info, retFunFun, retFunPayload, ..}) = do
      rf <- unbox retFunFun
      ts1 <- getClosureTypes rf
      ts2 <- getBitmapClosureTypes retFunPayload
      pure $ tipe info : ts1  ++ ts2
    getClosureTypes (RetBCO {info, bco, bcoArgs, ..}) = do
      bco <- unbox bco
      bcoCls <- getClosureTypes bco
      bcoArgsCls <- getBitmapClosureTypes bcoArgs
      pure $ tipe info : bcoCls  ++ bcoArgsCls
    -- Other closures
    getClosureTypes (ConstrClosure {info, ..}) = pure [tipe info]
    getClosureTypes (FunClosure {info, ..}) = pure [tipe info]
    getClosureTypes (ThunkClosure {info, ..}) = pure [tipe info]
    getClosureTypes (SelectorClosure {info, ..}) = pure [tipe info]
    getClosureTypes (PAPClosure {info, ..}) = pure [tipe info]
    getClosureTypes (APClosure {info, ..}) = pure [tipe info]
    getClosureTypes (APStackClosure {info, ..}) = pure [tipe info]
    getClosureTypes (IndClosure {info, ..}) = pure [tipe info]
    getClosureTypes (BCOClosure {info, ..}) = pure [tipe info]
    getClosureTypes (BlackholeClosure {info, ..}) = pure [tipe info]
    getClosureTypes (ArrWordsClosure {info, ..}) = pure [tipe info]
    getClosureTypes (MutArrClosure {info, ..}) = pure [tipe info]
    getClosureTypes (SmallMutArrClosure {info, ..}) = pure [tipe info]
    getClosureTypes (MVarClosure {info, ..}) = pure [tipe info]
    getClosureTypes (IOPortClosure {info, ..}) = pure [tipe info]
    getClosureTypes (MutVarClosure {info, ..}) = pure [tipe info]
    getClosureTypes (BlockingQueueClosure {info, ..}) = pure [tipe info]
    getClosureTypes (WeakClosure {info, ..}) = pure [tipe info]
    getClosureTypes (TSOClosure {info, ..}) = pure [tipe info]
    getClosureTypes (StackClosure {info, ..}) = pure [tipe info]
    getClosureTypes (OtherClosure {info, ..}) = pure [tipe info]
    getClosureTypes (UnsupportedClosure {info, ..}) = pure [tipe info]
    getClosureTypes _ = pure []

    getBitmapClosureTypes :: [Box] -> IO [ClosureType]
    getBitmapClosureTypes bps =
      reverse <$>
        foldlM
          ( \acc p -> do
              c <- unbox p
              case c of
                UnknownTypeWordSizedPrimitive _ -> pure acc
                c -> do
                  cls <- getClosureTypes c
                  pure $ cls ++ acc
          )
          []
          bps

unbox :: Box -> IO Closure
unbox (DecodedClosureBox c) = pure c
unbox box = getBoxedClosureData box
