% -----------------------------------------------------------------------------
% $Id: PrelException.lhs,v 1.18 2000/04/13 08:58:27 simonmar Exp $
%
% (c) The GRAP/AQUA Project, Glasgow University, 1998
%

Exceptions and exception-handling functions.

\begin{code}
{-# OPTIONS -fcompiling-prelude -fno-implicit-prelude #-}

#ifndef __HUGS__
module PrelException where

import PrelList
import PrelBase
import PrelMaybe
import PrelShow
import PrelIOBase
import PrelST 		( STret(..) )
import PrelDynamic
import PrelGHC
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Exception datatype and operations}
%*							*
%*********************************************************

\begin{code}
data Exception
  = IOException 	IOError		-- IO exceptions (from 'ioError')
  | ArithException  	ArithException	-- Arithmetic exceptions
  | ArrayException	ArrayException  -- Array-related exceptions
  | ErrorCall		String		-- Calls to 'error'
  | NoMethodError       String		-- A non-existent method was invoked
  | PatternMatchFail	String		-- A pattern match failed
  | NonExhaustiveGuards String		-- A guard match failed
  | RecSelError		String		-- Selecting a non-existent field
  | RecConError		String		-- Field missing in record construction
  | RecUpdError		String		-- Record doesn't contain updated field
  | AssertionFailed	String		-- Assertions
  | DynException	Dynamic		-- Dynamic exceptions
  | AsyncException	AsyncException	-- Externally generated errors
  | PutFullMVar 			-- Put on a full MVar
  | BlockedOnDeadMVar			-- Blocking on a dead MVar
  | NonTermination

data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord)

data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  deriving (Eq, Ord)

data ArrayException
  = IndexOutOfBounds  	String		-- out-of-range array access
  | UndefinedElement	String		-- evaluating an undefined element

stackOverflow, heapOverflow :: Exception -- for the RTS
stackOverflow = AsyncException StackOverflow
heapOverflow  = AsyncException HeapOverflow

instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"

instance Show AsyncException where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
  showsPrec _ ThreadKilled    = showString "thread killed"

instance Show ArrayException where
  showsPrec _ (IndexOutOfBounds s)
	= showString "array index out of range"
	. (if not (null s) then showString ": " . showString s
			   else id)
  showsPrec _ (UndefinedElement s)
	= showString "undefined array element"
	. (if not (null s) then showString ": " . showString s
			   else id)

instance Show Exception where
  showsPrec _ (IOException err)	         = shows err
  showsPrec _ (ArithException err)       = shows err
  showsPrec _ (ArrayException err)       = shows err
  showsPrec _ (ErrorCall err)	         = showString err
  showsPrec _ (NoMethodError err)        = showString err
  showsPrec _ (PatternMatchFail err)     = showString err
  showsPrec _ (NonExhaustiveGuards err)  = showString err
  showsPrec _ (RecSelError err)	         = showString err
  showsPrec _ (RecConError err)	         = showString err
  showsPrec _ (RecUpdError err)	         = showString err
  showsPrec _ (AssertionFailed err)      = showString err
  showsPrec _ (AsyncException e)	 = shows e
  showsPrec _ (DynException _err)        = showString "unknown exception"
  showsPrec _ (PutFullMVar)		 = showString "putMVar: full MVar"
  showsPrec _ (BlockedOnDeadMVar)	 = showString "thread blocked indefinitely"
  showsPrec _ (NonTermination)           = showString "<<loop>>"
\end{code}

%*********************************************************
%*							*
\subsection{Primitive catch and throw}
%*							*
%*********************************************************

\begin{code}
throw :: Exception -> a

#ifdef __HUGS__
throw = primRaise
#else
throw exception = raise# exception
#endif
\end{code}

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
  where handler (IOException err) = k err
	handler other             = throw other

catchNonIO      :: IO a -> (Exception -> IO a) -> IO a 
catchNonIO m k	=  catchException m handler
  where handler (IOException err) = ioError err
	handler other             = k other
\end{code}


%*********************************************************
%*							*
\subsection{Try and bracket}
%*							*
%*********************************************************

The construct @try comp@ exposes errors which occur within a
computation, and which are not fully handled.  It always succeeds.

\begin{code}
try            :: IO a -> IO (Either IOError a)
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
\subsection{ioError}
%*							*
%*********************************************************

Why is this stuff here?  To avoid recursive module dependencies of
course.

\begin{code}
ioError         :: IOError -> IO a 
ioError err	=  IO $ \s -> throw (IOException err) s
	-- (ioError e) isn't an exception; we only throw
	-- the exception when applied to a world
\end{code}

%*********************************************************
%*							*
\subsection{Controlling asynchronous exception delivery}
%*							*
%*********************************************************

\begin{code}
#ifndef __HUGS__
blockAsyncExceptions :: IO a -> IO a
blockAsyncExceptions (IO io) = IO $ blockAsyncExceptions# io

unblockAsyncExceptions :: IO a -> IO a
unblockAsyncExceptions (IO io) = IO $ unblockAsyncExceptions# io
#else
-- Not implemented yet in Hugs.
blockAsyncExceptions :: IO a -> IO a
blockAsyncExceptions (IO io) = IO io

unblockAsyncExceptions :: IO a -> IO a
unblockAsyncExceptions (IO io) = IO io
#endif
\end{code}

