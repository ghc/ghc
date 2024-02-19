{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Exception
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (extended exceptions)
--
-- This module provides support for raising and catching both built-in
-- and user-defined exceptions.
--
-- In addition to exceptions thrown by 'IO' operations, exceptions may
-- be thrown by pure code (imprecise exceptions) or by external events
-- (asynchronous exceptions), but may only be caught in the 'IO' monad.
-- For more details, see:
--
--  * /A semantics for imprecise exceptions/, by Simon Peyton Jones,
--    Alastair Reid, Tony Hoare, Simon Marlow, Fergus Henderson,
--    in /PLDI'99/.
--
--  * /Asynchronous exceptions in Haskell/, by Simon Marlow, Simon Peyton
--    Jones, Andy Moran and John Reppy, in /PLDI'01/.
--
--  * /An Extensible Dynamically-Typed Hierarchy of Exceptions/,
--    by Simon Marlow, in /Haskell '06/.
--

module Control.Exception
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
     -- *  Throwing exceptions
     throw,
     throwIO,
     ioError,
     throwTo,
     -- *  Catching Exceptions
     -- $catching
     -- **  Catching all exceptions
     -- $catchall
     -- **  The @catch@ functions
     catch,
     catches,
     Handler(..),
     catchJust,
     -- **  The @handle@ functions
     handle,
     handleJust,
     -- **  The @try@ functions
     try,
     tryJust,
     -- **  The @evaluate@ function
     evaluate,
     -- **  The @mapException@ function
     mapException,
     -- *  Asynchronous Exceptions
     -- $async
     -- **  Asynchronous exception control
     -- | The following functions allow a thread to control delivery of
     -- asynchronous exceptions during a critical region.
     mask,
     mask_,
     uninterruptibleMask,
     uninterruptibleMask_,
     MaskingState(..),
     getMaskingState,
     interruptible,
     allowInterrupt,
     -- ***  Applying @mask@ to an exception handler
     -- $block_handler
     -- ***  Interruptible operations
     -- $interruptible
     -- *  Assertions
     assert,
     -- *  Utilities
     bracket,
     bracket_,
     bracketOnError,
     finally,
     onException
     ) where

import GHC.Internal.Control.Exception
