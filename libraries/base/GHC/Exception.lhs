% ------------------------------------------------------------------------------
% $Id: Exception.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1998-2000
%

Exceptions and exception-handling functions.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

#ifndef __HUGS__
module GHC.Exception 
	( module GHC.Exception, 
	  Exception(..), AsyncException(..), 
	  IOException(..), ArithException(..), ArrayException(..),
	  throw, ioError ) 
  where

import GHC.Base
import GHC.Maybe
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

catch           :: IO a -> (Exception -> IO a) -> IO a 
catch m k	=  catchException m handler
  where handler err@(IOException _) = k err
        handler err@(UserError   _) = k err
	handler other               = throw other
\end{code}


%*********************************************************
%*							*
\subsection{Try and bracket}
%*							*
%*********************************************************

The construct @try comp@ exposes errors which occur within a
computation, and which are not fully handled.  It always succeeds.

These are the IO-only try/bracket.  For the full exception try/bracket
see hslibs/lang/Exception.lhs.

\begin{code}
try            :: IO a -> IO (Either Exception a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> ioError e
\end{code}


%*********************************************************
%*							*
\subsection{Controlling asynchronous exception delivery}
%*							*
%*********************************************************

\begin{code}
#ifndef __HUGS__
block :: IO a -> IO a
block (IO io) = IO $ blockAsyncExceptions# io

unblock :: IO a -> IO a
unblock (IO io) = IO $ unblockAsyncExceptions# io
#else
-- Not implemented yet in Hugs.
block :: IO a -> IO a
block (IO io) = IO io

unblock :: IO a -> IO a
unblock (IO io) = IO io
#endif
\end{code}


