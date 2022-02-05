{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Type
  ( SomeExceptionWithBacktrace
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  , Exception
  ) where

import GHC.Num.Integer ()   -- See Note [Depend on GHC.Num.Integer] in GHC.Base

data SomeExceptionWithBacktrace
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeExceptionWithBacktrace

class Exception e

instance Exception SomeExceptionWithBacktrace
