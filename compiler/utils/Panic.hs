{-
(c) The University of Glasgow 2006
(c) The GRASP Project, Glasgow University, 1992-2000

Defines basic functions for printing error messages.

It's hard to put these functions anywhere else without causing
some unnecessary loops in the module dependency graph.
-}

{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase #-}

module Panic (
     GhcException(..), showGhcException,
     throwGhcException, throwGhcExceptionIO,
     handleGhcException,
     progName,
     pgmError,

     panic, sorry, assertPanic, trace,
     panicDoc, sorryDoc, pgmErrorDoc,

     Exception.Exception(..), showException, safeShowException,
     try, tryMost, throwTo,

     withSignalHandlers,
) where
#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} Outputable (SDoc, showSDocUnsafe)

import Config
import Exception

import Control.Monad.IO.Class
import Control.Concurrent
import Debug.Trace        ( trace )
import System.IO.Unsafe
import System.Environment

#if !defined(mingw32_HOST_OS)
import System.Posix.Signals as S
#endif

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler as S
#endif

import GHC.Stack
import System.Mem.Weak  ( deRefWeak )

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
  -- | Some other fatal signal (SIGHUP,SIGTERM)
  = Signal Int

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
--
-- Note that this uses 'DynFlags.unsafeGlobalDynFlags', which may have some
-- uninitialized fields if invoked before 'GHC.initGhcMonad' has been called.
-- If the error message to be printed includes a pretty-printer document
-- which forces one of these fields this call may bottom.
showGhcException :: GhcException -> ShowS
showGhcException exception
 = case exception of
        UsageError str
         -> showString str . showChar '\n' . showString short_usage

        CmdLineError str        -> showString str
        PprProgramError str  sdoc  ->
            showString str . showString "\n\n" .
            showString (showSDocUnsafe sdoc)
        ProgramError str        -> showString str
        InstallationError str   -> showString str
        Signal n                -> showString "signal: " . shows n

        PprPanic  s sdoc ->
            panicMsg $ showString s . showString "\n\n"
                     . showString (showSDocUnsafe sdoc)
        Panic s -> panicMsg (showString s)

        PprSorry  s sdoc ->
            sorryMsg $ showString s . showString "\n\n"
                     . showString (showSDocUnsafe sdoc)
        Sorry s -> sorryMsg (showString s)
  where
    sorryMsg :: ShowS -> ShowS
    sorryMsg s =
        showString "sorry! (unimplemented feature or known bug)\n"
      . showString ("  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t")
      . s . showString "\n"

    panicMsg :: ShowS -> ShowS
    panicMsg s =
        showString "panic! (the 'impossible' happened)\n"
      . showString ("  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t")
      . s . showString "\n\n"
      . showString "Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug\n"


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


-- | Throw a failed assertion exception for a given filename and line number.
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

-- | We use reference counting for signal handlers
{-# NOINLINE signalHandlersRefCount #-}
#if !defined(mingw32_HOST_OS)
signalHandlersRefCount :: MVar (Word, Maybe (S.Handler,S.Handler
                                            ,S.Handler,S.Handler))
#else
signalHandlersRefCount :: MVar (Word, Maybe S.Handler)
#endif
signalHandlersRefCount = unsafePerformIO $ newMVar (0,Nothing)


-- | Temporarily install standard signal handlers for catching ^C, which just
-- throw an exception in the current thread.
withSignalHandlers :: (ExceptionMonad m, MonadIO m) => m a -> m a
withSignalHandlers act = do
  main_thread <- liftIO myThreadId
  wtid <- liftIO (mkWeakThreadId main_thread)

  let
      interrupt = do
        r <- deRefWeak wtid
        case r of
          Nothing -> return ()
          Just t  -> throwTo t UserInterrupt

#if !defined(mingw32_HOST_OS)
  let installHandlers = do
        let installHandler' a b = installHandler a b Nothing
        hdlQUIT <- installHandler' sigQUIT  (Catch interrupt)
        hdlINT  <- installHandler' sigINT   (Catch interrupt)
        -- see #3656; in the future we should install these automatically for
        -- all Haskell programs in the same way that we install a ^C handler.
        let fatal_signal n = throwTo main_thread (Signal (fromIntegral n))
        hdlHUP  <- installHandler' sigHUP   (Catch (fatal_signal sigHUP))
        hdlTERM <- installHandler' sigTERM  (Catch (fatal_signal sigTERM))
        return (hdlQUIT,hdlINT,hdlHUP,hdlTERM)

  let uninstallHandlers (hdlQUIT,hdlINT,hdlHUP,hdlTERM) = do
        _ <- installHandler sigQUIT  hdlQUIT Nothing
        _ <- installHandler sigINT   hdlINT  Nothing
        _ <- installHandler sigHUP   hdlHUP  Nothing
        _ <- installHandler sigTERM  hdlTERM Nothing
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

  let installHandlers   = installHandler (Catch sig_handler)
  let uninstallHandlers = installHandler -- directly install the old handler
#endif

  -- install signal handlers if necessary
  let mayInstallHandlers = liftIO $ modifyMVar_ signalHandlersRefCount $ \case
        (0,Nothing)     -> do
          hdls <- installHandlers
          return (1,Just hdls)
        (c,oldHandlers) -> return (c+1,oldHandlers)

  -- uninstall handlers if necessary
  let mayUninstallHandlers = liftIO $ modifyMVar_ signalHandlersRefCount $ \case
        (1,Just hdls)   -> do
          _ <- uninstallHandlers hdls
          return (0,Nothing)
        (c,oldHandlers) -> return (c-1,oldHandlers)

  mayInstallHandlers
  act `gfinally` mayUninstallHandlers
