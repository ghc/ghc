% -----------------------------------------------------------------------------
% $Id: Err.lhs,v 1.3 2001/07/31 13:11:40 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[GHC.Err]{Module @GHC.Err@}

The GHC.Err module defines the code for the wired-in error functions,
which have a special type in the compiler (with "open tyvars").

We cannot define these functions in a module where they might be used
(e.g., GHC.Base), because the magical wired-in type will get confused
with what the typechecker figures out.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module GHC.Err 
       (
         irrefutPatError
       , noMethodBindingError
       , nonExhaustiveGuardsError
       , patError
       , recSelError
       , recConError
       , recUpdError               -- :: String -> a

       , absentErr, parError       -- :: a
       , seqError                  -- :: a

       , errorCString		   -- :: Addr# -> a	-- Arg is a ptr to C string 
       , error		           -- :: String -> a
       , assertError		   -- :: String -> Bool -> a -> a
       
       , undefined		   -- :: a
       ) where

import GHC.Base
import GHC.List     ( span )
import GHC.Exception
\end{code}

%*********************************************************
%*							*
\subsection{Error-ish functions}
%*							*
%*********************************************************

\begin{code}
-- error stops execution and displays an error message
error :: String -> a
error s = throw (ErrorCall s)

errorCString :: Addr# -> a
errorCString s = error (unpackCString# s)

-- It is expected that compilers will recognize this and insert error
-- messages which are more appropriate to the context in which undefined 
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
absentErr, parError, seqError :: a

absentErr = error "Oops! The program has entered an `absent' argument!\n"
parError  = error "Oops! Entered GHCerr.parError (a GHC bug -- please report it!)\n"
seqError = error "Oops! Entered seqError (a GHC bug -- please report it!)\n"

\end{code}

\begin{code}
irrefutPatError
   , noMethodBindingError
   , nonExhaustiveGuardsError
   , patError
   , recSelError
   , recConError
   , recUpdError :: String -> a

noMethodBindingError     s = throw (NoMethodError (untangle s "No instance nor default method for class operation"))
irrefutPatError		 s = throw (PatternMatchFail (untangle s "Irrefutable pattern failed for pattern"))
nonExhaustiveGuardsError s = throw (PatternMatchFail (untangle s "Non-exhaustive guards in"))
patError 		 s = throw (PatternMatchFail (untangle s "Non-exhaustive patterns in"))
recSelError 		 s = throw (RecSelError (untangle s "Missing field in record selection"))
recConError 		 s = throw (RecConError (untangle s "Missing field in record construction"))
recUpdError 		 s = throw (RecUpdError (untangle s "Record doesn't contain field(s) to be updated"))


assertError :: String -> Bool -> a -> a
assertError str pred v 
  | pred      = v
  | otherwise = throw (AssertionFailed (untangle str "Assertion failed"))

\end{code}


(untangle coded message) expects "coded" to be of the form 

	"location|details"

It prints

	location message details

\begin{code}
untangle :: String -> String -> String
untangle coded message
  =  location
  ++ ": " 
  ++ message
  ++ details
  ++ "\n"
  where
    (location, details)
      = case (span not_bar coded) of { (loc, rest) ->
	case rest of
	  ('|':det) -> (loc, ' ' : det)
	  _	    -> (loc, "")
	}
    not_bar c = c /= '|'
\end{code}
