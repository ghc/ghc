%
% (c) The University of Glasgow 2006
% (c) The GRASP Project, Glasgow University, 1992-2000
%

Defines basic funtions for printing error messages.

It's hard to put these functions anywhere else without causing
some unnecessary loops in the module dependency graph.

\begin{code}
module Panic  
   ( 
     GhcException(..), showGhcException, throwGhcException, handleGhcException,
     ghcError, progName,
     pgmError,

     panic, sorry, panicFastInt, assertPanic, trace,
     
     Exception.Exception(..), showException, try, tryMost, throwTo,

     installSignalHandlers, interruptTargetThread
   ) where

#include "HsVersions.h"

import Config
import FastTypes

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif /* mingw32_HOST_OS */

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#endif

import Exception
import Control.Concurrent ( MVar, ThreadId, withMVar, newMVar, modifyMVar_,
                            myThreadId )
import Data.Dynamic
import Debug.Trace	( trace )
import System.IO.Unsafe	( unsafePerformIO )
import System.Exit
import System.Environment
\end{code}

GHC's own exception type.

\begin{code}
ghcError :: GhcException -> a
ghcError e = Exception.throw e

-- error messages all take the form
--
--	<location>: <error>
--
-- If the location is on the command line, or in GHC itself, then 
-- <location>="ghc".  All of the error types below correspond to 
-- a <location> of "ghc", except for ProgramError (where the string is
-- assumed to contain a location already, so we don't print one).

data GhcException
  = PhaseFailed  String		-- name of phase 
  		 ExitCode	-- an external phase (eg. cpp) failed
  | Signal Int                  -- some other fatal signal (SIGHUP,SIGTERM)
  | UsageError   String		-- prints the short usage msg after the error
  | CmdLineError String		-- cmdline prob, but doesn't print usage
  | Panic        String		-- the `impossible' happened
  | Sorry        String		-- the user tickled something that's known not to work yet, 
				-- and we're not counting it as a bug.
  | InstallationError String	-- an installation problem
  | ProgramError String		-- error in the user's code, probably
  deriving Eq

instance Exception GhcException

progName :: String
progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}

short_usage :: String
short_usage = "Usage: For basic information, try the `--help' option."

showException :: Exception e => e -> String
showException = show

instance Show GhcException where
  showsPrec _ e@(ProgramError _) = showGhcException e
  showsPrec _ e@(CmdLineError _) = showString "<command line>: " . showGhcException e
  showsPrec _ e = showString progName . showString ": " . showGhcException e

showGhcException :: GhcException -> String -> String
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
showGhcException (Signal n)
   = showString "signal: " . shows n
showGhcException (Panic s)
   = showString ("panic! (the 'impossible' happened)\n"
		 ++ "  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t"
	         ++ s ++ "\n\n"
	         ++ "Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug\n")
showGhcException (Sorry s)
   = showString ("sorry! (this is work in progress)\n"
		 ++ "  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t"
	         ++ s ++ "\n")


throwGhcException :: GhcException -> a
throwGhcException = Exception.throw

handleGhcException :: ExceptionMonad m => (GhcException -> m a) -> m a -> m a
handleGhcException = ghandle

ghcExceptionTc :: TyCon
ghcExceptionTc = mkTyCon "GhcException"
{-# NOINLINE ghcExceptionTc #-}
instance Typeable GhcException where
  typeOf _ = mkTyConApp ghcExceptionTc []
\end{code}

Panics and asserts.

\begin{code}
panic, sorry, pgmError :: String -> a
panic    x = throwGhcException (Panic x)
sorry    x = throwGhcException (Sorry x)
pgmError x = throwGhcException (ProgramError x)

--  #-versions because panic can't return an unboxed int, and that's
-- what TAG_ is with GHC at the moment.  Ugh. (Simon)
-- No, man -- Too Beautiful! (Will)

panicFastInt :: String -> FastInt
panicFastInt s = case (panic s) of () -> _ILIT(0)

assertPanic :: String -> Int -> a
assertPanic file line = 
  Exception.throw (Exception.AssertionFailed 
           ("ASSERT failed! file " ++ file ++ ", line " ++ show line))
\end{code}

\begin{code}
-- | tryMost is like try, but passes through UserInterrupt and Panic
-- exceptions.  Used when we want soft failures when reading interface
-- files, for example.

-- XXX I'm not entirely sure if this is catching what we really want to catch
tryMost :: IO a -> IO (Either SomeException a)
tryMost action = do r <- try action
                    case r of
                        Left se ->
                            case fromException se of
                                -- Some GhcException's we rethrow,
                                Just (Signal _)  -> throwIO se
                                Just (Panic _)   -> throwIO se
                                -- others we return
                                Just _           -> return (Left se)
                                Nothing ->
                                    case fromException se of
                                        -- All IOExceptions are returned
                                        Just (_ :: IOException) ->
                                            return (Left se)
                                        -- Anything else is rethrown
                                        Nothing -> throwIO se
                        Right v -> return (Right v)
\end{code}

Standard signal handlers for catching ^C, which just throw an
exception in the target thread.  The current target thread is
the thread at the head of the list in the MVar passed to
installSignalHandlers.

\begin{code}
installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  modifyMVar_ interruptTargetThread (return . (main_thread :))

  let
      interrupt_exn = (toException UserInterrupt)

      interrupt = do
	withMVar interruptTargetThread $ \targets ->
	  case targets of
	   [] -> return ()
	   (thread:_) -> throwTo thread interrupt_exn

  --
#if !defined(mingw32_HOST_OS)
  _ <- installHandler sigQUIT  (Catch interrupt) Nothing 
  _ <- installHandler sigINT   (Catch interrupt) Nothing
  -- see #3656; in the future we should install these automatically for
  -- all Haskell programs in the same way that we install a ^C handler.
  let fatal_signal n = throwTo main_thread (Signal (fromIntegral n))
  _ <- installHandler sigHUP   (Catch (fatal_signal sigHUP))  Nothing
  _ <- installHandler sigTERM  (Catch (fatal_signal sigTERM)) Nothing
  return ()
#else
  -- GHC 6.3+ has support for console events on Windows
  -- NOTE: running GHCi under a bash shell for some reason requires
  -- you to press Ctrl-Break rather than Ctrl-C to provoke
  -- an interrupt.  Ctrl-C is getting blocked somewhere, I don't know
  -- why --SDM 17/12/2004
  let sig_handler ControlC = interrupt
      sig_handler Break    = interrupt
      sig_handler _        = return ()

  _ <- installHandler (Catch sig_handler)
  return ()
#endif

{-# NOINLINE interruptTargetThread #-}
interruptTargetThread :: MVar [ThreadId]
interruptTargetThread = unsafePerformIO (newMVar [])
\end{code}
