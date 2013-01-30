%
% (c) The University of Glasgow 2006
% (c) The GRASP Project, Glasgow University, 1992-2000
%
Defines basic functions for printing error messages.

It's hard to put these functions anywhere else without causing
some unnecessary loops in the module dependency graph.

\begin{code}
module Panic (
     GhcException(..), showGhcException,
     throwGhcException, throwGhcExceptionIO,
     handleGhcException,
     progName,
     pgmError,

     panic, sorry, panicFastInt, assertPanic, trace,
     panicDoc, sorryDoc, panicDocFastInt, pgmErrorDoc,

     Exception.Exception(..), showException, safeShowException, try, tryMost, throwTo,

     installSignalHandlers,
     pushInterruptTargetThread, popInterruptTargetThread
) where
#include "HsVersions.h"

import {-# SOURCE #-} Outputable (SDoc)

import Config
import FastTypes
import Exception

import Control.Concurrent
import Data.Dynamic
#if __GLASGOW_HASKELL__ < 705
import Data.Maybe
#endif
import Debug.Trace        ( trace )
import System.IO.Unsafe
import System.Exit
import System.Environment

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#endif

import GHC.Stack

#if __GLASGOW_HASKELL__ >= 705
import System.Mem.Weak  ( Weak, deRefWeak )
#endif

-- | GHC's own exception type
--   error messages all take the form:
--
--  @
--      <location>: <error>
--  @
--
--   If the location is on the command line, or in GHC itself, then
--   <location>="ghc".  All of the error types below correspond to
--   a <location> of "ghc", except for ProgramError (where the string is
--  assumed to contain a location already, so we don't print one).

data GhcException
  = PhaseFailed  String         -- name of phase
                 ExitCode       -- an external phase (eg. cpp) failed

  -- | Some other fatal signal (SIGHUP,SIGTERM)
  | Signal Int

  -- | Prints the short usage msg after the error
  | UsageError   String

  -- | A problem with the command line arguments, but don't print usage.
  | CmdLineError String

  -- | The 'impossible' happened.
  | Panic        String
  | PprPanic     String SDoc

  -- | The user tickled something that's known not to work yet,
  --   but we're not counting it as a bug.
  | Sorry        String
  | PprSorry     String SDoc

  -- | An installation problem.
  | InstallationError String

  -- | An error in the user's code, probably.
  | ProgramError    String
  | PprProgramError String SDoc
  deriving (Typeable)

instance Exception GhcException

instance Show GhcException where
  showsPrec _ e@(ProgramError _) = showGhcException e
  showsPrec _ e@(CmdLineError _) = showString "<command line>: " . showGhcException e
  showsPrec _ e = showString progName . showString ": " . showGhcException e


-- | The name of this GHC.
progName :: String
progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}


-- | Short usage information to display when we are given the wrong cmd line arguments.
short_usage :: String
short_usage = "Usage: For basic information, try the `--help' option."


-- | Show an exception as a string.
showException :: Exception e => e -> String
showException = show

-- | Show an exception which can possibly throw other exceptions.
-- Used when displaying exception thrown within TH code.
safeShowException :: Exception e => e -> IO String
safeShowException e = do
    -- ensure the whole error message is evaluated inside try
    r <- try (return $! forceList (showException e))
    case r of
        Right msg -> return msg
        Left e' -> safeShowException (e' :: SomeException)
    where
        forceList [] = []
        forceList xs@(x : xt) = x `seq` forceList xt `seq` xs

-- | Append a description of the given exception to this string.
showGhcException :: GhcException -> String -> String
showGhcException exception
 = case exception of
        UsageError str
         -> showString str . showChar '\n' . showString short_usage

        PhaseFailed phase code
         -> showString "phase `" . showString phase .
            showString "' failed (exitcode = " . shows (int_code code) .
            showString ")"

        CmdLineError str        -> showString str
        PprProgramError str  _  ->
            showGhcException (ProgramError (str ++ "\n<<details unavailable>>"))
        ProgramError str        -> showString str
        InstallationError str   -> showString str
        Signal n                -> showString "signal: " . shows n

        PprPanic  s _ ->
            showGhcException (Panic (s ++ "\n<<details unavailable>>"))
        Panic s
         -> showString $
                "panic! (the 'impossible' happened)\n"
                ++ "  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t"
                ++ s ++ "\n\n"
                ++ "Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug\n"

        PprSorry  s _ ->
            showGhcException (Sorry (s ++ "\n<<details unavailable>>"))
        Sorry s
         -> showString $
                "sorry! (unimplemented feature or known bug)\n"
                 ++ "  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t"
                 ++ s ++ "\n"

  where int_code code =
          case code of
                ExitSuccess   -> (0::Int)
                ExitFailure x -> x


throwGhcException :: GhcException -> a
throwGhcException = Exception.throw

throwGhcExceptionIO :: GhcException -> IO a
throwGhcExceptionIO = Exception.throwIO

handleGhcException :: ExceptionMonad m => (GhcException -> m a) -> m a -> m a
handleGhcException = ghandle


-- | Panics and asserts.
panic, sorry, pgmError :: String -> a
panic    x = unsafeDupablePerformIO $ do
   stack <- ccsToStrings =<< getCurrentCCS x
   if null stack
      then throwGhcException (Panic x)
      else throwGhcException (Panic (x ++ '\n' : renderStack stack))

sorry    x = throwGhcException (Sorry x)
pgmError x = throwGhcException (ProgramError x)

panicDoc, sorryDoc, pgmErrorDoc :: String -> SDoc -> a
panicDoc    x doc = throwGhcException (PprPanic        x doc)
sorryDoc    x doc = throwGhcException (PprSorry        x doc)
pgmErrorDoc x doc = throwGhcException (PprProgramError x doc)


-- | Panic while pretending to return an unboxed int.
--   You can't use the regular panic functions in expressions
--   producing unboxed ints because they have the wrong kind.
panicFastInt :: String -> FastInt
panicFastInt s = case (panic s) of () -> _ILIT(0)

panicDocFastInt :: String -> SDoc -> FastInt
panicDocFastInt s d = case (panicDoc s d) of () -> _ILIT(0)


-- | Throw an failed assertion exception for a given filename and line number.
assertPanic :: String -> Int -> a
assertPanic file line =
  Exception.throw (Exception.AssertionFailed
           ("ASSERT failed! file " ++ file ++ ", line " ++ show line))


-- | Like try, but pass through UserInterrupt and Panic exceptions.
--   Used when we want soft failures when reading interface files, for example.
--   TODO: I'm not entirely sure if this is catching what we really want to catch
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


-- | Install standard signal handlers for catching ^C, which just throw an
--   exception in the target thread.  The current target thread is the
--   thread at the head of the list in the MVar passed to
--   installSignalHandlers.
installSignalHandlers :: IO ()
installSignalHandlers = do
  main_thread <- myThreadId
  pushInterruptTargetThread main_thread

  let
      interrupt_exn = (toException UserInterrupt)

      interrupt = do
        mt <- peekInterruptTargetThread
        case mt of
          Nothing -> return ()
          Just t  -> throwTo t interrupt_exn

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

#if __GLASGOW_HASKELL__ >= 705
{-# NOINLINE interruptTargetThread #-}
interruptTargetThread :: MVar [Weak ThreadId]
interruptTargetThread = unsafePerformIO (newMVar [])

pushInterruptTargetThread :: ThreadId -> IO ()
pushInterruptTargetThread tid = do
 wtid <- mkWeakThreadId tid
 modifyMVar_ interruptTargetThread $ return . (wtid :)

peekInterruptTargetThread :: IO (Maybe ThreadId)
peekInterruptTargetThread =
  withMVar interruptTargetThread $ loop
 where
   loop [] = return Nothing
   loop (t:ts) = do
     r <- deRefWeak t
     case r of
       Nothing -> loop ts
       Just t  -> return (Just t)
#else
{-# NOINLINE interruptTargetThread #-}
interruptTargetThread :: MVar [ThreadId]
interruptTargetThread = unsafePerformIO (newMVar [])

pushInterruptTargetThread :: ThreadId -> IO ()
pushInterruptTargetThread tid = do
 modifyMVar_ interruptTargetThread $ return . (tid :)

peekInterruptTargetThread :: IO (Maybe ThreadId)
peekInterruptTargetThread =
  withMVar interruptTargetThread $ return . listToMaybe
#endif

popInterruptTargetThread :: IO ()
popInterruptTargetThread =
  modifyMVar_ interruptTargetThread $
   \tids -> return $! case tids of []     -> []
                                   (_:ts) -> ts

\end{code}
