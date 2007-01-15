\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Err
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The "GHC.Err" module defines the code for the wired-in error functions,
-- which have a special type in the compiler (with \"open tyvars\").
-- 
-- We cannot define these functions in a module where they might be used
-- (e.g., "GHC.Base"), because the magical wired-in type will get confused
-- with what the typechecker figures out.
-- 
-----------------------------------------------------------------------------

-- #hide
module GHC.Err 
       (
         irrefutPatError
       , noMethodBindingError
       , nonExhaustiveGuardsError
       , patError
       , recSelError
       , recConError
       , runtimeError              -- :: Addr#  -> a	-- Addr# points to UTF8 encoded C string

       , absentErr	           -- :: a
       , divZeroError		   -- :: a
       , overflowError		   -- :: a

       , error		           -- :: String -> a
       , assertError		   -- :: String -> Bool -> a -> a
       
       , undefined		   -- :: a
       ) where

#ifndef __HADDOCK__
import GHC.Base
import GHC.List     ( span )
import GHC.Exception
#endif
\end{code}

%*********************************************************
%*							*
\subsection{Error-ish functions}
%*							*
%*********************************************************

\begin{code}
-- | 'error' stops execution and displays an error message.
error :: String -> a
error s = throw (ErrorCall s)

-- | A special case of 'error'.
-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which 'undefined'
-- appears. 

undefined :: a
undefined =  error "Prelude.undefined"
\end{code}

%*********************************************************
%*							 *
\subsection{Compiler generated errors + local utils}
%*							 *
%*********************************************************

Used for compiler-generated error message;
encoding saves bytes of string junk.

\begin{code}
absentErr :: a

absentErr = error "Oops! The program has entered an `absent' argument!\n"
\end{code}

\begin{code}
recSelError, recConError, irrefutPatError, runtimeError,
	     nonExhaustiveGuardsError, patError, noMethodBindingError
	:: Addr# -> a	-- All take a UTF8-encoded C string

recSelError 		 s = throw (RecSelError (unpackCStringUtf8# s))	-- No location info unfortunately
runtimeError		 s = error (unpackCStringUtf8# s)		-- No location info unfortunately

nonExhaustiveGuardsError s = throw (PatternMatchFail (untangle s "Non-exhaustive guards in"))
irrefutPatError		 s = throw (PatternMatchFail (untangle s "Irrefutable pattern failed for pattern"))
recConError    		 s = throw (RecConError      (untangle s "Missing field in record construction"))
noMethodBindingError     s = throw (NoMethodError    (untangle s "No instance nor default method for class operation"))
patError 		 s = throw (PatternMatchFail (untangle s "Non-exhaustive patterns in"))

assertError :: Addr# -> Bool -> a -> a
assertError str pred v 
  | pred      = v
  | otherwise = throw (AssertionFailed (untangle str "Assertion failed"))
\end{code}


(untangle coded message) expects "coded" to be of the form 

	"location|details"

It prints

	location message details

\begin{code}
untangle :: Addr# -> String -> String
untangle coded message
  =  location
  ++ ": " 
  ++ message
  ++ details
  ++ "\n"
  where
    coded_str = unpackCStringUtf8# coded

    (location, details)
      = case (span not_bar coded_str) of { (loc, rest) ->
	case rest of
	  ('|':det) -> (loc, ' ' : det)
	  _	    -> (loc, "")
	}
    not_bar c = c /= '|'
\end{code}

Divide by zero and arithmetic overflow.
We put them here because they are needed relatively early
in the libraries before the Exception type has been defined yet.

\begin{code}
{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = throw (ArithException DivideByZero)

{-# NOINLINE overflowError #-}
overflowError :: a
overflowError = throw (ArithException Overflow)
\end{code}

