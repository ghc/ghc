{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Exception.Type
  ( SomeException
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  ) where

import GHC.Num.Integer ()   -- See Note [Depend on GHC.Num.Integer] in GHC.Internal.Base

data SomeException
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeException
