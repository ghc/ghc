{-
(c) The University of Glasgow 2006
(c) The GRASP Project, Glasgow University, 1992-2000

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}

-- | Defines basic functions for printing error messages.
--
-- It's hard to put these functions anywhere else without causing
-- some unnecessary loops in the module dependency graph.
module GHC.Utils.Panic
   ( GhcException(..)
   , showGhcException
   , showGhcExceptionUnsafe
   , throwGhcException
   , throwGhcExceptionIO
   , handleGhcException

   , pgmError
   , panic
   , pprPanic
   , assertPanic
   , assertPprPanic
   , assertPpr
   , assertPprM
   , massertPpr
   , sorry
   , panicDoc
   , sorryDoc
   , pgmErrorDoc
   , cmdLineError
   , cmdLineErrorIO
   , callStackDoc
   , prettyCallStackDoc

   , Exception.Exception(..)
   , showException
   , safeShowException
   , try
   , tryMost
   , throwTo
   , withSignalHandlers
   )
where

import GHC.Prelude
import GHC.Stack

import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain
import GHC.Utils.Constants

import GHC.Utils.Exception as Exception

import Control.Monad.IO.Class
import qualified Control.Monad.Catch as MC
import Control.Concurrent
import Data.Typeable      ( cast )
import System.IO.Unsafe

#if !defined(mingw32_HOST_OS)
import System.Posix.Signals as S
#endif

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler as S
#endif

import System.Mem.Weak  ( deRefWeak )

-- | GHC's own exception type
--   error messages all take the form:
--
--  @
--      \<location>: \<error>
--  @
--
--   If the location is on the command line, or in GHC itself, then
--   \<location>="ghc".  All of the error types below correspond to
--   a \<location> of "ghc", except for ProgramError (where the string is
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

instance Exception GhcException where
  fromException (SomeException e)
    | Just ge <- cast e = Just ge
    | Just pge <- cast e = Just $
        case pge of
          PlainSignal n -> Signal n
          PlainUsageError str -> UsageError str
          PlainCmdLineError str -> CmdLineError str
          PlainPanic str -> Panic str
          PlainSorry str -> Sorry str
          PlainInstallationError str -> InstallationError str
          PlainProgramError str -> ProgramError str
    | otherwise = Nothing

instance Show GhcException where
  showsPrec _ e = showGhcExceptionUnsafe e

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
-- Note that this uses 'defaultSDocContext', which doesn't use the options
-- set by the user via DynFlags.
showGhcExceptionUnsafe :: GhcException -> ShowS
showGhcExceptionUnsafe = showGhcException defaultSDocContext

-- | Append a description of the given exception to this string.
showGhcException :: SDocContext -> GhcException -> ShowS
showGhcException ctx = showPlainGhcException . \case
  Signal n -> PlainSignal n
  UsageError str -> PlainUsageError str
  CmdLineError str -> PlainCmdLineError str
  Panic str -> PlainPanic str
  Sorry str -> PlainSorry str
  InstallationError str -> PlainInstallationError str
  ProgramError str -> PlainProgramError str

  PprPanic str sdoc -> PlainPanic $
      concat [str, "\n\n", renderWithContext ctx sdoc]
  PprSorry str sdoc -> PlainProgramError $
      concat [str, "\n\n", renderWithContext ctx sdoc]
  PprProgramError str sdoc -> PlainProgramError $
      concat [str, "\n\n", renderWithContext ctx sdoc]

throwGhcException :: GhcException -> a
throwGhcException = Exception.throw

throwGhcExceptionIO :: GhcException -> IO a
throwGhcExceptionIO = Exception.throwIO

handleGhcException :: ExceptionMonad m => (GhcException -> m a) -> m a -> m a
handleGhcException = MC.handle

-- | Throw an exception saying "bug in GHC" with a callstack
pprPanic :: HasCallStack => String -> SDoc -> a
pprPanic s doc = panicDoc s (doc $$ callStackDoc)

-- | Throw an exception saying "bug in GHC"
panicDoc :: String -> SDoc -> a
panicDoc x doc = throwGhcException (PprPanic x doc)

-- | Throw an exception saying "this isn't finished yet"
sorryDoc :: String -> SDoc -> a
sorryDoc x doc = throwGhcException (PprSorry x doc)

-- | Throw an exception saying "bug in pgm being compiled" (used for unusual program errors)
pgmErrorDoc :: String -> SDoc -> a
pgmErrorDoc x doc = throwGhcException (PprProgramError x doc)

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
withSignalHandlers :: ExceptionMonad m => m a -> m a
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
  act `MC.finally` mayUninstallHandlers

callStackDoc :: HasCallStack => SDoc
callStackDoc = prettyCallStackDoc callStack

prettyCallStackDoc :: CallStack -> SDoc
prettyCallStackDoc cs =
    hang (text "Call stack:")
       4 (vcat $ map text $ lines (prettyCallStack cs))

-- | Panic with an assertion failure, recording the given file and
-- line number. Should typically be accessed with the ASSERT family of macros
assertPprPanic :: HasCallStack => SDoc -> a
assertPprPanic msg = withFrozenCallStack (pprPanic "ASSERT failed!" msg)


assertPpr :: HasCallStack => Bool -> SDoc -> a -> a
{-# INLINE assertPpr #-}
assertPpr cond msg a =
  if debugIsOn && not cond
    then withFrozenCallStack (assertPprPanic msg)
    else a

massertPpr :: (HasCallStack, Applicative m) => Bool -> SDoc -> m ()
{-# INLINE massertPpr #-}
massertPpr cond msg = withFrozenCallStack (assertPpr cond msg (pure ()))

assertPprM :: (HasCallStack, Monad m) => m Bool -> SDoc -> m ()
{-# INLINE assertPprM #-}
assertPprM mcond msg = withFrozenCallStack (mcond >>= \cond -> massertPpr cond msg)
