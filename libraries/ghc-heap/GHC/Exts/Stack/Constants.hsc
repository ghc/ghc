{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHC.Exts.Stack.Constants (
  ByteOffset(..),
  WordOffset(..),
  offsetStgCatchFrameHandler,
  sizeStgCatchFrame,
  offsetStgCatchSTMFrameCode,
  offsetStgCatchSTMFrameHandler,
  sizeStgCatchSTMFrame,
  offsetStgUpdateFrameUpdatee,
  sizeStgUpdateFrame,
  offsetStgAtomicallyFrameCode,
  offsetStgAtomicallyFrameResult,
  sizeStgAtomicallyFrame,
  offsetStgCatchRetryFrameRunningAltCode,
  offsetStgCatchRetryFrameRunningFirstCode,
  offsetStgCatchRetryFrameAltCode,
  sizeStgCatchRetryFrame,
  offsetStgRetFunFrameSize,
  offsetStgRetFunFramePayload,
  sizeStgRetFunFrame,
  offsetStgBCOFrameInstrs,
  offsetStgBCOFrameLiterals,
  offsetStgBCOFramePtrs,
  offsetStgBCOFrameArity,
  offsetStgBCOFrameSize,
  offsetStgClosurePayload,
  sizeStgClosure,
  byteOffsetToWordOffset,
  bytesToWords,
  bytesInWord,
) where

import GHC.Internal.Stack.Constants
