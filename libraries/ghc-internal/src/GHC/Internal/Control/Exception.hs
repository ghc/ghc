{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Control.Exception
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
-----------------------------------------------------------------------------

module GHC.Internal.Control.Exception (

        -- * The Exception type
        SomeException(..),
        Exception(..),          -- class
        IOException,            -- instance Eq, Ord, Show, Typeable, Exception
        ArithException(..),     -- instance Eq, Ord, Show, Typeable, Exception
        ArrayException(..),     -- instance Eq, Ord, Show, Typeable, Exception
        AssertionFailed(..),
        SomeAsyncException(..),
        AsyncException(..),     -- instance Eq, Ord, Show, Typeable, Exception
        asyncExceptionToException, asyncExceptionFromException,

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

        -- * Throwing exceptions
        throw,
        throwIO,
        rethrowIO,
        ioError,
        throwTo,

        -- ** The @catch@ functions
        catch,
        catchNoPropagate,
        catches, Handler(..),
        catchJust,

        -- ** Exception annotation

        -- ** The @handle@ functions
        handle,
        handleJust,

        -- ** The @try@ functions
        try,
        tryWithContext,
        tryJust,

        -- ** The @evaluate@ function
        evaluate,

        -- ** The @mapException@ function
        mapException,

        -- ** Asynchronous exception control
        mask,
        mask_,
        uninterruptibleMask,
        uninterruptibleMask_,
        MaskingState(..),
        getMaskingState,
        interruptible,
        allowInterrupt,

        -- * Assertions
        assert,

        -- * Utilities
        bracket,
        bracket_,
        bracketOnError,

        finally,
        onException,

        -- * Annotating exceptions

        ExceptionContext(..),
        annotateIO,
        WhileHandling(..),

  ) where

import GHC.Internal.Control.Exception.Base

import GHC.Internal.Base
import GHC.Internal.IO (interruptible)

-- | You need this when using 'catches'.
data Handler a = forall e . Exception e => Handler (e -> IO a)

-- | @since base-4.6.0.0
instance Functor Handler where
     fmap f (Handler h) = Handler (fmap f . h)

{- |
Sometimes you want to catch two different sorts of exception. You could
do something like

> f = expr `catch` \ (ex :: ArithException) -> handleArith ex
>          `catch` \ (ex :: IOException)    -> handleIO    ex

However, there are a couple of problems with this approach. The first is
that having two exception handlers is inefficient. However, the more
serious issue is that the second exception handler will catch exceptions
in the first, e.g. in the example above, if @handleArith@ throws an
@IOException@ then the second exception handler will catch it.

Instead, we provide a function 'catches', which would be used thus:

> f = expr `catches` [Handler (\ (ex :: ArithException) -> handleArith ex),
>                     Handler (\ (ex :: IOException)    -> handleIO    ex)]
-}
catches :: IO a -> [Handler a] -> IO a
catches io handlers = io `catch` catchesHandler handlers

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

-- -----------------------------------------------------------------------------
-- Asynchronous exceptions

-- | When invoked inside 'mask', this function allows a masked
-- asynchronous exception to be raised, if one exists.  It is
-- equivalent to performing an interruptible operation (see
-- #interruptible), but does not involve any actual blocking.
--
-- When called outside 'mask', or inside 'uninterruptibleMask', this
-- function has no effect.
--
-- @since base-4.4.0.0
allowInterrupt :: IO ()
allowInterrupt = interruptible $ return ()

