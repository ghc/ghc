%
% (c) The GRASP Project, Glasgow University, 1992-2000
%
\section{Panic error messages}

Defines basic funtions for printing error messages.

It's hard to put these functions anywhere else without causing
some unnecessary loops in the module dependency graph.

\begin{code}
module Panic  
   ( 
     GhcException(..), ghcError, progName, 
     panic, panic#, assertPanic, trace 
   ) where

import FastTypes

import Dynamic
import IOExts
import Exception

import System
#include "HsVersions.h"
\end{code}

GHC's own exception type.

\begin{code}
ghcError :: GhcException -> a
ghcError e = throwDyn e

data GhcException
  = PhaseFailed String ExitCode
  | Interrupted
  | UsageError String		-- prints the short usage msg after the error
  | Panic String		-- the `impossible' happened
  | OtherError String		-- just prints the error message
  deriving Eq

progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}

short_usage = "Usage: For basic information, try the `--help' option."
   
instance Show GhcException where
  showsPrec _ e = showString progName . showString ": " . showBarf e

showBarf (UsageError str)
   = showString str . showChar '\n' . showString short_usage
showBarf (OtherError str)
   = showString str
showBarf (PhaseFailed phase code)
   = showString phase . showString " failed, code = " . shows code
showBarf (Interrupted)
   = showString "interrupted"
showBarf (Panic s)
   = showString ("panic! (the `impossible' happened):\n\t"
	         ++ s ++ "\n\n"
	         ++ "Please report it as a compiler bug "
	         ++ "to glasgow-haskell-bugs@haskell.org.\n\n")

ghcExceptionTc = mkTyCon "GhcException"
{-# NOINLINE ghcExceptionTc #-}
instance Typeable GhcException where
  typeOf _ = mkAppTy ghcExceptionTc []
\end{code}

Panics and asserts.

\begin{code}
panic :: String -> a
panic x = throwDyn (Panic x)

-- #-versions because panic can't return an unboxed int, and that's
-- what TAG_ is with GHC at the moment.  Ugh. (Simon)
-- No, man -- Too Beautiful! (Will)

panic# :: String -> FastInt
panic# s = case (panic s) of () -> _ILIT 0

assertPanic :: String -> Int -> a
assertPanic file line = 
  throw (AssertionFailed 
           ("ASSERT failed! file " ++ file ++ ", line " ++ show line))
\end{code}
