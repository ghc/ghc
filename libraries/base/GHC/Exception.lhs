\begin{code}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
-- 
-----------------------------------------------------------------------------

-- #hide
module GHC.Exception
        ( module GHC.Exception,
          throwIO, ioError )
  where

import Data.Maybe
import {-# SOURCE #-} Data.Typeable
import GHC.Base
import GHC.IOBase hiding (Exception)
import qualified GHC.IOBase
import GHC.Show
\end{code}

%*********************************************************
%*                                                      *
\subsection{Exceptions}
%*                                                      *
%*********************************************************

\begin{code}
data SomeException = forall e . Exception e => SomeException e
    deriving Typeable

instance Show SomeException where
    showsPrec p (SomeException e) = showsPrec p e

class (Typeable e, Show e) => Exception e where
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

    toException = SomeException
    fromException (SomeException e) = cast e

instance Exception SomeException where
    toException se = se
    fromException = Just
\end{code}

For now at least, make the monolithic Exception type an instance.

\begin{code}
instance Exception GHC.IOBase.Exception
\end{code}

%*********************************************************
%*                                                      *
\subsection{Primitive catch and throw}
%*                                                      *
%*********************************************************

catchException used to handle the passing around of the state to the
action and the handler.  This turned out to be a bad idea - it meant
that we had to wrap both arguments in thunks so they could be entered
as normal (remember IO returns an unboxed pair...).

Now catch# has type

    catch# :: IO a -> (b -> IO a) -> IO a

(well almost; the compiler doesn't know about the IO newtype so we
have to work around that in the definition of catchException below).

\begin{code}
catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raise# e

catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny (IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)

-- | The 'catch' function establishes a handler that receives any 'IOError'
-- raised in the action protected by 'catch'.  An 'IOError' is caught by
-- the most recent handler established by 'catch'.  These handlers are
-- not selective: all 'IOError's are caught.  Exception propagation
-- must be explicitly provided in a handler by re-raising any unwanted
-- exceptions.  For example, in
--
-- > f = catch g (\e -> if IO.isEOFError e then return [] else ioError e)
--
-- the function @f@ returns @[]@ when an end-of-file exception
-- (cf. 'System.IO.Error.isEOFError') occurs in @g@; otherwise, the
-- exception is propagated to the next outer handler.
--
-- When an exception propagates outside the main program, the Haskell
-- system prints the associated 'IOError' value and exits the program.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.catch' from "Control.Exception".
catch           :: IO a -> (IOError -> IO a) -> IO a 
catch m k       =  catchException m handler
  where handler (IOException err)   = k err
        handler other               = throw other

-- | Throw an exception.  Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
throw :: Exception e => e -> a
throw e = raise# (toException e)

-- | A variant of 'throw' that can be used within the 'IO' monad.
--
-- Although 'throwIO' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e   `seq` x  ===> throw e
-- > throwIO e `seq` x  ===> x
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throwIO' will only cause
-- an exception to be raised when it is used within the 'IO' monad.
-- The 'throwIO' variant should be used in preference to 'throw' to
-- raise an exception within the 'IO' monad because it guarantees
-- ordering with respect to other 'IO' operations, whereas 'throw'
-- does not.
throwIO :: Exception e => e -> IO a
throwIO e = IO (raiseIO# (toException e))
\end{code}


%*********************************************************
%*                                                      *
\subsection{Controlling asynchronous exception delivery}
%*                                                      *
%*********************************************************

\begin{code}
-- | Applying 'block' to a computation will
-- execute that computation with asynchronous exceptions
-- /blocked/.  That is, any thread which
-- attempts to raise an exception in the current thread with 'Control.Exception.throwTo' will be
-- blocked until asynchronous exceptions are enabled again.  There\'s
-- no need to worry about re-enabling asynchronous exceptions; that is
-- done automatically on exiting the scope of
-- 'block'.
--
-- Threads created by 'Control.Concurrent.forkIO' inherit the blocked
-- state from the parent; that is, to start a thread in blocked mode,
-- use @block $ forkIO ...@.  This is particularly useful if you need to
-- establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.
block :: IO a -> IO a

-- | To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblock :: IO a -> IO a

block (IO io) = IO $ blockAsyncExceptions# io
unblock (IO io) = IO $ unblockAsyncExceptions# io

-- | returns True if asynchronous exceptions are blocked in the
-- current thread.
blocked :: IO Bool
blocked = IO $ \s -> case asyncExceptionsBlocked# s of
                        (# s', i #) -> (# s', i /=# 0# #)
\end{code}

\begin{code}
-- | Forces its argument to be evaluated when the resultant 'IO' action
-- is executed.  It can be used to order evaluation with respect to
-- other 'IO' operations; its semantics are given by
--
-- >   evaluate x `seq` y    ==>  y
-- >   evaluate x `catch` f  ==>  (return $! x) `catch` f
-- >   evaluate x >>= f      ==>  (return $! x) >>= f
--
-- /Note:/ the first equation implies that @(evaluate x)@ is /not/ the
-- same as @(return $! x)@.  A correct definition is
--
-- >   evaluate x = (return $! x) >>= return
--
evaluate :: a -> IO a
evaluate a = IO $ \s -> case a `seq` () of () -> (# s, a #)
        -- NB. can't write  
        --      a `seq` (# s, a #)
        -- because we can't have an unboxed tuple as a function argument
\end{code}
