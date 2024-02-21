{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.IO.Exception
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- IO-related Exception types and functions
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.IO.Exception (
  BlockedIndefinitelyOnMVar(..), blockedIndefinitelyOnMVar,
  BlockedIndefinitelyOnSTM(..), blockedIndefinitelyOnSTM,
  Deadlock(..),
  AllocationLimitExceeded(..), allocationLimitExceeded,
  AssertionFailed(..),
  CompactionFailed(..),
  cannotCompactFunction, cannotCompactPinned, cannotCompactMutable,

  SomeAsyncException(..),
  asyncExceptionToException, asyncExceptionFromException,
  AsyncException(..), stackOverflow, heapOverflow,

  ArrayException(..),
  ExitCode(..),
  FixIOException (..),

  ioException,
  ioError,
  IOError,
  IOException(..),
  IOErrorType(..),
  userError,
  assertError,
  unsupportedOperation,
  untangle,
 ) where

import GHC.Internal.IO.Exception

