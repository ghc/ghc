{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Type
  ( SomeException
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  ) where

import GHC.Types ()

data SomeException
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeException
