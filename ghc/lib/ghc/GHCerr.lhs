%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[GHCerr]{Module @GHCerr@}

The GHCerr module defines the code for the wired-in error functions,
which have a special type in the compiler (with "open tyvars").
 
We cannot define these functions in a module where they might be used
(e.g., GHCbase), because the magical wired-in type will get confused
with what the typechecker figures out.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module GHCerr where

--import Prelude
import PrelBase
import PrelList ( span )
import IOBase

---------------------------------------------------------------
-- HACK: Magic unfoldings not implemented for unboxed lists
--	 Need to define a "build" to avoid undefined symbol
-- in this module to avoid .hi proliferation.

build   = error "GHCbase.build"
augment = error "GHCbase.augment"
--{-# GENERATE_SPECS build a #-}
--build 		:: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
--build g 	= g (:) []
\end{code}


Used for compiler-generated error message;
encoding saves bytes of string junk.

\begin{code}
absentErr, parError :: a

absentErr = error "Oops! The program has entered an `absent' argument!\n"
parError  = error "Oops! Entered GHCerr.parError (a GHC bug -- please report it!)\n"
\end{code}

\begin{code}
irrefutPatError
 , noDefaultMethodError
 , noExplicitMethodError
 , nonExhaustiveGuardsError
 , patError
 , recConError
 , recUpdError :: String -> a

noDefaultMethodError     s = error ("noDefaultMethodError:"++s)
noExplicitMethodError    s = error ("No default method for class operation "++s)
irrefutPatError		 s = error (untangle s "Irrefutable pattern failed for pattern")
nonExhaustiveGuardsError s = error (untangle s "Non-exhaustive guards in")
patError 		 s = error (untangle s "Non-exhaustive patterns in")
recConError 		 s = error (untangle s "Missing field in record construction:")
recUpdError 		 s = error (untangle s "Record to doesn't contain field(s) to be updated")
\end{code}


(untangle coded message) expects "coded" to be of the form 

	"location|details"

It prints

	location message details

\begin{code}
untangle coded message
  =  location
  ++ ": " 
  ++ message
  ++ details
  ++ "\n"
  where
    (location, details)
      = case (span not_bar coded) of { (location, rest) ->
	case rest of
	  ('|':details) -> (location, ' ' : details)
	  _	        -> (location, "")
	}
    not_bar c = c /= '|'
\end{code}

-- This local variant of "error" calls PatErrorHdrHook instead of ErrorHdrHook,
-- but the former does exactly the same as the latter, so I nuked it.
--		SLPJ Jan 97
-- patError__ = error__ (\ x -> _ccall_ PatErrorHdrHook x)

