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

import Config
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

-- error messages all take the form
--
--	<location>: <error>
--
-- If the location is on the command line, or in GHC itself, then 
-- <location>="ghc".  All of the error types below correspond to 
-- a <location> of "ghc", except for ProgramError (where the string is
-- assumed to contain a location already, so we don't print one).

data GhcException
  = PhaseFailed String		-- name of phase 
  		ExitCode	-- an external phase (eg. cpp) failed
  | Interrupted			-- someone pressed ^C
  | UsageError String		-- prints the short usage msg after the error
  | CmdLineError String		-- cmdline prob, but doesn't print usage
  | Panic String		-- the `impossible' happened
  | InstallationError String	-- an installation problem
  | ProgramError String		-- error in the user's code, probably
  deriving Eq

progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}

short_usage = "Usage: For basic information, try the `--help' option."
   
instance Show GhcException where
  showsPrec _ e@(ProgramError _) = showGhcException e
  showsPrec _ e = showString progName . showString ": " . showGhcException e

showGhcException (UsageError str)
   = showString str . showChar '\n' . showString short_usage
showGhcException (PhaseFailed phase code)
   = showString "phase `" . showString phase . 
     showString "' failed (exitcode = " . shows int_code . 
     showString ")"
  where
    int_code = 
      case code of
        ExitSuccess   -> (0::Int)
	ExitFailure x -> x
showGhcException (CmdLineError str)
   = showString str
showGhcException (ProgramError str)
   = showString str
showGhcException (InstallationError str)
   = showString str
showGhcException (Interrupted)
   = showString "interrupted"
showGhcException (Panic s)
   = showString ("panic! (the `impossible' happened, GHC version "
		 ++ cProjectVersion ++ "):\n\t"
	         ++ s ++ "\n\n"
	         ++ "Please report it as a compiler bug "
	         ++ "to glasgow-haskell-bugs@haskell.org,\n"
		 ++ "or http://sourceforge.net/projects/ghc/.\n\n")

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
