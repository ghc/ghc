% -----------------------------------------------------------------------------
% $Id: PrelException.lhs,v 1.2 1998/12/02 13:27:01 simonm Exp $
%
% (c) The GRAP/AQUA Project, Glasgow University, 1998
%

Exceptions and exception-handling functions.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

#ifndef __HUGS__
module PrelException where

import PrelBase
import PrelIOBase
import PrelST 		( STret(..) )
import PrelDynamic
import PrelGHC
#endif
\end{code}

-----------------------------------------------------------------------------
Exception datatype and operations.

\begin{code}
data Exception
  = IOException 	IOError		-- IO exceptions (from 'fail')
  | ArithException  	ArithError	-- Arithmetic exceptions
  | ErrorCall		String		-- Calls to 'error'
  | NoMethodError       String		-- A non-existent method was invoked
  | PatternMatchFail	String		-- A pattern match failed
  | NonExhaustiveGuards String		-- A guard match failed
  | RecSelError		String		-- Selecting a non-existent field
  | RecConError		String		-- Field missing in record construction
  | RecUpdError		String		-- Record doesn't contain updated field
  | AssertionFailed	String		-- Assertions
  | DynException	Dynamic		-- Dynamic exceptions
  | ExternalException   ExtError        -- External exceptions

data ArithError
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord)

data ExtError
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  deriving (Eq, Ord)

instance Show ArithError where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"

instance Show ExtError where
  showsPrec _ StackOverflow   = showString "stack overflow"
  showsPrec _ HeapOverflow    = showString "heap overflow"
  showsPrec _ ThreadKilled    = showString "thread killed"

instance Show Exception where
  showsPrec _ (IOException err)	         = shows err
  showsPrec _ (ArithException err)       = shows err
  showsPrec _ (ErrorCall err)	         = showString err
  showsPrec _ (NoMethodError err)        = showString err
  showsPrec _ (PatternMatchFail err)     = showString err
  showsPrec _ (NonExhaustiveGuards err)  = showString err
  showsPrec _ (RecSelError err)	         = showString err
  showsPrec _ (RecConError err)	         = showString err
  showsPrec _ (RecUpdError err)	         = showString err
  showsPrec _ (AssertionFailed err)      = showString err
  showsPrec _ (DynException err)         = showString "unknown exception"

-- Primitives:

throw :: Exception -> a

#ifdef __HUGS__
throw = primRaise
#else
throw exception = raise# exception
#endif
\end{code}

catch handles the passing around of the state in the IO monad; if we
don't actually apply (and hence run) an IO computation, we don't get
any exceptions!  Hence a large mantrap to watch out for is

	catch# (m :: IO ()) (handler :: NDSet Exception -> IO ())

since the computation 'm' won't actually be performed in the context
of the 'catch#'.  In fact, don't use catch# at all.

\begin{code}
catchException :: IO a -> (Exception -> IO a) -> IO a
#ifdef __HUGS__
catchException m k =  ST (\s -> unST m s `primCatch'` \ err -> unST (k err) s)
#else
catchException m k =  IO $ \s -> case catch# (liftIO m s) (\exs -> liftIO (k exs) s)
			  of STret s r -> (# s, r #)
#endif

catch           :: IO a -> (IOError -> IO a) -> IO a 
catch m k	=  catchException m handler
  where handler (IOException err) = k err
	handler other             = throw other
\end{code}

Why is this stuff here?  To avoid recursive module dependencies of
course.

\begin{code}
fail            :: IOError -> IO a 
fail err	=  throw (IOException err)
\end{code}

