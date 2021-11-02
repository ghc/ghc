{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Type
  ( SomeExceptionWithLocation
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  , Exception
  ) where

import GHC.Num.Integer ()   -- See Note [Depend on GHC.Num.Integer] in GHC.Base

data SomeExceptionWithLocation
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeExceptionWithLocation

class Exception e

instance Exception SomeExceptionWithLocation
