{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Exception.Type
  ( SomeException
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  ) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Types ()

data SomeException
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeException
