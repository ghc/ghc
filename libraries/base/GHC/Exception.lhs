\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
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

#ifndef __HUGS__
module GHC.Exception 
	( module GHC.Exception, 
	  Exception(..), AsyncException(..), 
	  IOException(..), ArithException(..), ArrayException(..),
	  throw, throwIO, ioError ) 
  where

import GHC.Base
import GHC.IOBase

#endif
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
#ifdef __HUGS__
catchException m k =  ST (\s -> unST m s `primCatch'` \ err -> unST (k err) s)
#else
catchException (IO m) k =  IO $ \s -> catch# m (\ex -> unIO (k ex)) s
#endif

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

#ifndef __HUGS__
block (IO io) = IO $ blockAsyncExceptions# io
unblock (IO io) = IO $ unblockAsyncExceptions# io
#else
unblock :: IO a -> IO a
unblock (IO io) = IO io
#endif
\end{code}


