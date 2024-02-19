{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Exception.Base
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (extended exceptions)
--
-- Extensible exceptions, except for multiple handlers.
--

module Control.Exception.Base
    (-- *  The Exception type
     SomeException(..),
     Exception(..),
     IOException,
     ArithException(..),
     ArrayException(..),
     AssertionFailed(..),
     SomeAsyncException(..),
     AsyncException(..),
     asyncExceptionToException,
     asyncExceptionFromException,
     NonTermination(..),
     NestedAtomically(..),
     BlockedIndefinitelyOnMVar(..),
     FixIOException(..),
     BlockedIndefinitelyOnSTM(..),
     AllocationLimitExceeded(..),
     CompactionFailed(..),
     Deadlock(..),
     NoMethodError(..),
     PatternMatchFail(..),
     RecConError(..),
     RecSelError(..),
     RecUpdError(..),
     ErrorCall(..),
     TypeError(..),
     NoMatchingContinuationPrompt(..),
     -- *  Throwing exceptions
     throwIO,
     throw,
     ioError,
     throwTo,
     -- *  Catching Exceptions
     -- **  The @catch@ functions
     catch,
     catchJust,
     -- **  The @handle@ functions
     handle,
     handleJust,
     -- **  The @try@ functions
     try,
     tryJust,
     onException,
     -- **  The @evaluate@ function
     evaluate,
     -- **  The @mapException@ function
     mapException,
     -- *  Asynchronous Exceptions
     -- **  Asynchronous exception control
     mask,
     mask_,
     uninterruptibleMask,
     uninterruptibleMask_,
     MaskingState(..),
     getMaskingState,
     -- *  Assertions
     assert,
     -- *  Utilities
     bracket,
     bracket_,
     bracketOnError,
     finally,
     -- *  Calls for GHC runtime
     recSelError,
     recConError,
     impossibleError,
     impossibleConstraintError,
     nonExhaustiveGuardsError,
     patError,
     noMethodBindingError,
     typeError,
     nonTermination,
     nestedAtomically,
     noMatchingContinuationPrompt
     ) where

import GHC.Internal.Control.Exception.Base
