%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelErr]{Module @PrelErr@}

The PrelErr module defines the code for the wired-in error functions,
which have a special type in the compiler (with "open tyvars").

We cannot define these functions in a module where they might be used
(e.g., PrelBase), because the magical wired-in type will get confused
with what the typechecker figures out.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module PrelErr 

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

       , error		           -- :: String -> a
       , ioError	           -- :: String -> a
       , assert__		   -- :: String -> Bool -> a -> a
       ) where

--import Prelude
import PrelBase
import PrelIOBase
import PrelAddr
import PrelList     ( span )

#ifndef __PARALLEL_HASKELL__
import PrelForeign  ( StablePtr, deRefStablePtr )
#endif

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

%*********************************************************
%*							*
\subsection{Error-ish functions}
%*							*
%*********************************************************

\begin{code}
errorIO :: IO () -> a

errorIO (IO io)
  = case (errorIO# io) of
      _ -> bottom
  where
    bottom = bottom -- Never evaluated

ioError :: String -> a
ioError s = error__ ( \ x -> _ccall_ IOErrorHdrHook x ) s

-- error stops execution and displays an error message
error :: String -> a
error s = error__ ( \ x -> _ccall_ ErrorHdrHook x ) s

error__ :: (Addr{-FILE *-} -> IO ()) -> String -> a

error__ msg_hdr s
#ifdef __PARALLEL_HASKELL__
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ stg_exit (1::Int)
	    )
#else
  = errorIO (msg_hdr sTDERR{-msg hdr-}	>>
	     _ccall_ fflush sTDERR	>>
	     fputs sTDERR s		>>
	     _ccall_ fflush sTDERR	>>
	     _ccall_ getErrorHandler	>>= \ errorHandler ->
	     if errorHandler == (-1::Int) then
		_ccall_ stg_exit (1::Int)
	     else
		_casm_ ``%r = (StgStablePtr)(%0);'' errorHandler
						>>= \ osptr ->
		_ccall_ decrementErrorCount     >>= \ () ->
		deRefStablePtr osptr            >>= \ oact ->
		oact
	    )
#endif {- !parallel -}
  where
    sTDERR = (``stderr'' :: Addr)
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
 --, noExplicitMethodError
   , nonExhaustiveGuardsError
   , patError
   , recSelError
   , recConError
   , recUpdError :: String -> a

--noDefaultMethodError     s = error ("noDefaultMethodError:"++s)
--noExplicitMethodError    s = error ("No default method for class operation "++s)
noMethodBindingError     s = error (untangle s "No instance nor default method for class operation")
irrefutPatError		 s = error (untangle s "Irrefutable pattern failed for pattern")
nonExhaustiveGuardsError s = error (untangle s "Non-exhaustive guards in")
patError 		 s = error (untangle s "Non-exhaustive patterns in")
recSelError 		 s = error (untangle s "Missing field in record selection:")
recConError 		 s = error (untangle s "Missing field in record construction:")
recUpdError 		 s = error (untangle s "Record to doesn't contain field(s) to be updated")


assert__ :: String -> Bool -> a -> a
assert__ str pred v 
  | pred      = v
  | otherwise = error (untangle str "Assertion failed")

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

