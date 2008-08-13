{-# OPTIONS_GHC -XNoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
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
-----------------------------------------------------------------------------

module Control.Exception (

        -- * The Exception type
#ifdef __HUGS__
        SomeException,
#else
        SomeException(..),
#endif
        Exception(..),          -- class
        IOException,            -- instance Eq, Ord, Show, Typeable, Exception
        ArithException(..),     -- instance Eq, Ord, Show, Typeable, Exception
        ArrayException(..),     -- instance Eq, Ord, Show, Typeable, Exception
        AssertionFailed(..),
        AsyncException(..),     -- instance Eq, Ord, Show, Typeable, Exception

#if __GLASGOW_HASKELL__ || __HUGS__
        NonTermination(..),
        NestedAtomically(..),
#endif
#ifdef __NHC__
        System.ExitCode(),	-- instance Exception
#endif

        BlockedOnDeadMVar(..),
        BlockedIndefinitely(..),
        Deadlock(..),
        NoMethodError(..),
        PatternMatchFail(..),
        RecConError(..),
        RecSelError(..),
        RecUpdError(..),
        ErrorCall(..),

        -- * Throwing exceptions
        throwIO,        -- :: Exception -> IO a
        throw,          -- :: Exception -> a
        ioError,        -- :: IOError -> IO a
#ifdef __GLASGOW_HASKELL__
        throwTo,        -- :: ThreadId -> Exception -> a
#endif

        -- * Catching Exceptions

        -- |There are several functions for catching and examining
        -- exceptions; all of them may only be used from within the
        -- 'IO' monad.

        -- ** The @catch@ functions
        catch,     -- :: IO a -> (Exception -> IO a) -> IO a
        catches, Handler(..),
        catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

        -- ** The @handle@ functions
        handle,    -- :: (Exception -> IO a) -> IO a -> IO a
        handleJust,-- :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a

        -- ** The @try@ functions
        try,       -- :: IO a -> IO (Either Exception a)
        tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)
        onException,

        -- ** The @evaluate@ function
        evaluate,  -- :: a -> IO a

        -- ** The @mapException@ function
        mapException,           -- :: (Exception -> Exception) -> a -> a

        -- * Asynchronous Exceptions

        -- $async

        -- ** Asynchronous exception control

        -- |The following two functions allow a thread to control delivery of
        -- asynchronous exceptions during a critical region.

        block,          -- :: IO a -> IO a
        unblock,        -- :: IO a -> IO a
        blocked,        -- :: IO Bool

        -- *** Applying @block@ to an exception handler

        -- $block_handler

        -- *** Interruptible operations

        -- $interruptible

        -- * Assertions

        assert,         -- :: Bool -> a -> a

        -- * Utilities

        bracket,        -- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
        bracket_,       -- :: IO a -> IO b -> IO c -> IO ()
        bracketOnError,

        finally,        -- :: IO a -> IO b -> IO a
  ) where

import Control.Exception.Base

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import Data.Maybe
#else
import Prelude hiding (catch)
#endif

#ifdef __NHC__
import System (ExitCode())
#endif

data Handler a = forall e . Exception e => Handler (e -> IO a)

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

{- $async

 #AsynchronousExceptions# Asynchronous exceptions are so-called because they arise due to
external influences, and can be raised at any point during execution.
'StackOverflow' and 'HeapOverflow' are two examples of
system-generated asynchronous exceptions.

The primary source of asynchronous exceptions, however, is
'throwTo':

>  throwTo :: ThreadId -> Exception -> IO ()

'throwTo' (also 'throwDynTo' and 'Control.Concurrent.killThread') allows one
running thread to raise an arbitrary exception in another thread.  The
exception is therefore asynchronous with respect to the target thread,
which could be doing anything at the time it receives the exception.
Great care should be taken with asynchronous exceptions; it is all too
easy to introduce race conditions by the over zealous use of
'throwTo'.
-}

{- $block_handler
There\'s an implied 'block' around every exception handler in a call
to one of the 'catch' family of functions.  This is because that is
what you want most of the time - it eliminates a common race condition
in starting an exception handler, because there may be no exception
handler on the stack to handle another exception if one arrives
immediately.  If asynchronous exceptions are blocked on entering the
handler, though, we have time to install a new exception handler
before being interrupted.  If this weren\'t the default, one would have
to write something like

>      block (
>           catch (unblock (...))
>                      (\e -> handler)
>      )

If you need to unblock asynchronous exceptions again in the exception
handler, just use 'unblock' as normal.

Note that 'try' and friends /do not/ have a similar default, because
there is no exception handler in this case.  If you want to use 'try'
in an asynchronous-exception-safe way, you will need to use
'block'.
-}

{- $interruptible

Some operations are /interruptible/, which means that they can receive
asynchronous exceptions even in the scope of a 'block'.  Any function
which may itself block is defined as interruptible; this includes
'Control.Concurrent.MVar.takeMVar'
(but not 'Control.Concurrent.MVar.tryTakeMVar'),
and most operations which perform
some I\/O with the outside world.  The reason for having
interruptible operations is so that we can write things like

>      block (
>         a <- takeMVar m
>         catch (unblock (...))
>               (\e -> ...)
>      )

if the 'Control.Concurrent.MVar.takeMVar' was not interruptible,
then this particular
combination could lead to deadlock, because the thread itself would be
blocked in a state where it can\'t receive any asynchronous exceptions.
With 'Control.Concurrent.MVar.takeMVar' interruptible, however, we can be
safe in the knowledge that the thread can receive exceptions right up
until the point when the 'Control.Concurrent.MVar.takeMVar' succeeds.
Similar arguments apply for other interruptible operations like
'System.IO.openFile'.
-}
