\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
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
	  Exception(..), AsyncException(..), 
	  IOException(..), ArithException(..), ArrayException(..),
	  throw, throwIO, ioError ) 
  where

import GHC.Base
import GHC.IOBase
\end{code}

%*********************************************************
%*							*
\subsection{Primitive catch}
%*							*
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
catchException :: IO a -> (Exception -> IO a) -> IO a
catchException (IO m) k =  IO $ \s -> catch# m (\ex -> unIO (k ex)) s

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
catch m k	=  catchException m handler
  where handler (IOException err)   = k err
	handler other               = throw other
\end{code}


%*********************************************************
%*							*
\subsection{Controlling asynchronous exception delivery}
%*							*
%*********************************************************

\begin{code}
-- | Applying 'block' to a computation will
-- execute that computation with asynchronous exceptions
-- /blocked/.  That is, any thread which
-- attempts to raise an exception in the current thread will be
-- blocked until asynchronous exceptions are enabled again.  There\'s
-- no need to worry about re-enabling asynchronous exceptions; that is
-- done automatically on exiting the scope of
-- 'block'.
block :: IO a -> IO a

-- | To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblock :: IO a -> IO a

block (IO io) = IO $ blockAsyncExceptions# io
unblock (IO io) = IO $ unblockAsyncExceptions# io
\end{code}

\begin{code}
-- | Forces its argument to be evaluated, and returns the result in
-- the 'IO' monad.  It can be used to order evaluation with respect to
-- other 'IO' operations; its semantics are given by
--
-- >   evaluate x `seq` y    ==>  y
-- >   evaluate x `catch` f  ==>  (return $! x) `catch` f
-- >   evaluate x >>= f      ==>  (return $! x) >>= f
--
-- /Note:/ the first equation implies that @(evaluate x)@ is /not/ the
-- same as @(return $! x)@.
evaluate :: a -> IO a
evaluate a = IO $ \s -> case a `seq` () of () -> (# s, a #)
        -- NB. can't write  
        --      a `seq` (# s, a #)
        -- because we can't have an unboxed tuple as a function argument
\end{code}
