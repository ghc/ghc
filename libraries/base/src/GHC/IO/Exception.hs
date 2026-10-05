{-# LANGUAGE Safe, MagicHash #-}
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
  exitWith,
 ) where

import GHC.Internal.IO.Exception

import Data.Maybe (Maybe (Nothing))
import Data.Eq ((==))
import GHC.Internal.Num as Rebindable (fromInteger)
import GHC.Types (IO)
import qualified GHC.Stack.Types as Rebindable (SrcLoc(..), pushCallStack, emptyCallStack)
import qualified GHC.Internal.Types as Rebindable (unpackCString#, unpackCStringUtf8#)
import Control.Exception.Base (throwIO)

-- ---------------------------------------------------------------------------
-- exitWith

-- | Computation 'exitWith' @code@ throws 'ExitCode' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.
--
-- On program termination, the standard 'Handle's 'stdout' and
-- 'stderr' are flushed automatically; any other buffered 'Handle's
-- need to be flushed manually, otherwise the buffered data will be
-- discarded.
--
-- A program that fails in any other way is treated as if it had
-- called 'exitFailure'.
-- A program that terminates successfully without calling 'exitWith'
-- explicitly is treated as if it had called 'exitWith' 'ExitSuccess'.
--
-- As an 'ExitCode' is an 'Control.Exception.Exception', it can be
-- caught using the functions of "Control.Exception".  This means that
-- cleanup computations added with 'GHC.Internal.Control.Exception.bracket' (from
-- "Control.Exception") are also executed properly on 'exitWith'.
--
-- Note: in GHC, 'exitWith' should be called from the main program
-- thread in order to exit the process.  When called from another
-- thread, 'exitWith' will throw an 'ExitCode' as normal, but the
-- exception will not cause the process itself to exit.
--
exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throwIO ExitSuccess
exitWith (ExitFailure 0) =
  ioError (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing Nothing)
exitWith code@(ExitFailure _) = throwIO code
