{-# LANGUAGE CPP, DeriveFunctor, DerivingVia, RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2010
--
-- The Session type and related functionality
--
-- -----------------------------------------------------------------------------

module GHC.Driver.Monad (
        -- * 'Ghc' monad stuff
        GhcMonad(..),
        Ghc(..),
        GhcT(..), liftGhcT,
        reflectGhc, reifyGhc,
        getSessionDynFlags,
        liftIO,
        Session(..), withSession, modifySession, withTempSession,

        -- ** Errors and Warnings
        IsGhcError(..),
        GhcError(..), ghcErrorMsg,
        SourceError, mkSrcErr, srcErrorMessages, mkApiErr,
        throwErrors, handleSourceError,

        logWarnings, printException, printOrThrowWarnings,
        WarnErrLogger, defaultWarnErrLogger
  ) where

import GHC.Prelude

import GHC.Utils.Monad
import GHC.Data.Bag
import GHC.Driver.Types
import GHC.Driver.Session
import GHC.Parser.Error    -- parse error type
import GHC.Tc.Types        -- tc/rn error type
import GHC.Utils.Exception
import GHC.Utils.Error

import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.Trans.Reader
import Data.IORef

-- -----------------------------------------------------------------------------
-- | A monad that has all the features needed by GHC API calls.
--
-- In short, a GHC monad
--
--   - allows embedding of IO actions,
--
--   - can log warnings,
--
--   - allows handling of (extensible) exceptions, and
--
--   - maintains a current session.
--
-- If you do not use 'Ghc' or 'GhcT', make sure to call 'GHC.initGhcMonad'
-- before any call to the GHC API functions can occur.
--
class (Functor m, ExceptionMonad m, HasDynFlags m) => GhcMonad m where
  getSession :: m HscEnv
  setSession :: HscEnv -> m ()

-- | Call the argument with the current session.
withSession :: GhcMonad m => (HscEnv -> m a) -> m a
withSession f = getSession >>= f

-- | Grabs the DynFlags from the Session
getSessionDynFlags :: GhcMonad m => m DynFlags
getSessionDynFlags = withSession (return . hsc_dflags)

-- | Set the current session to the result of applying the current session to
-- the argument.
modifySession :: GhcMonad m => (HscEnv -> HscEnv) -> m ()
modifySession f = do h <- getSession
                     setSession $! f h

withSavedSession :: GhcMonad m => m a -> m a
withSavedSession m = do
  saved_session <- getSession
  m `MC.finally` setSession saved_session

-- | Call an action with a temporarily modified Session.
withTempSession :: GhcMonad m => (HscEnv -> HscEnv) -> m a -> m a
withTempSession f m =
  withSavedSession $ modifySession f >> m

-- -----------------------------------------------------------------------------
-- | A monad that allows logging of warnings.

logWarnings :: GhcMonad m => WarningMessages -> m ()
logWarnings warns = do
  dflags <- getSessionDynFlags
  liftIO $ printOrThrowWarnings dflags warns

-- | Given a bag of warnings, turn them into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings dflags warns = do
  let (make_error, warns') =
        mapAccumBagL
          (\make_err warn ->
            case isWarnMsgFatal dflags warn of
              Nothing ->
                (make_err, warn)
              Just err_reason ->
                (True, warn{ errMsgSeverity = SevError
                           , errMsgReason = ErrReason err_reason
                           }))
          False warns
  if make_error
    then throwIO (mkSrcErr $ fmap MsgErr warns')
    else printBagOfErrors dflags warns

-- -----------------------------------------------------------------------------
-- | A minimal implementation of a 'GhcMonad'.  If you need a custom monad,
-- e.g., to maintain additional state consider wrapping this monad or using
-- 'GhcT'.
newtype Ghc a = Ghc { unGhc :: Session -> IO a }
  deriving (Functor)
  deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT Session IO)

-- | The Session is a handle to the complete state of a compilation
-- session.  A compilation session consists of a set of modules
-- constituting the current program or library, the context for
-- interactive evaluation, and various caches.
data Session = Session !(IORef HscEnv)

instance Applicative Ghc where
  pure a = Ghc $ \_ -> return a
  g <*> m = do f <- g; a <- m; return (f a)

instance Monad Ghc where
  m >>= g  = Ghc $ \s -> do a <- unGhc m s; unGhc (g a) s

instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA

instance MonadFix Ghc where
  mfix f = Ghc $ \s -> mfix (\x -> unGhc (f x) s)

instance HasDynFlags Ghc where
  getDynFlags = getSessionDynFlags

instance GhcMonad Ghc where
  getSession = Ghc $ \(Session r) -> readIORef r
  setSession s' = Ghc $ \(Session r) -> writeIORef r s'

-- | Reflect a computation in the 'Ghc' monad into the 'IO' monad.
--
-- You can use this to call functions returning an action in the 'Ghc' monad
-- inside an 'IO' action.  This is needed for some (too restrictive) callback
-- arguments of some library functions:
--
-- > libFunc :: String -> (Int -> IO a) -> IO a
-- > ghcFunc :: Int -> Ghc a
-- >
-- > ghcFuncUsingLibFunc :: String -> Ghc a -> Ghc a
-- > ghcFuncUsingLibFunc str =
-- >   reifyGhc $ \s ->
-- >     libFunc $ \i -> do
-- >       reflectGhc (ghcFunc i) s
--
reflectGhc :: Ghc a -> Session -> IO a
reflectGhc m = unGhc m

-- > Dual to 'reflectGhc'.  See its documentation.
reifyGhc :: (Session -> IO a) -> Ghc a
reifyGhc act = Ghc $ act

-- -----------------------------------------------------------------------------
-- | A monad transformer to add GHC specific features to another monad.
--
-- Note that the wrapped monad must support IO and handling of exceptions.
newtype GhcT m a = GhcT { unGhcT :: Session -> m a }
  deriving (Functor)
  deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT Session m)

liftGhcT :: m a -> GhcT m a
liftGhcT m = GhcT $ \_ -> m

instance Applicative m => Applicative (GhcT m) where
  pure x  = GhcT $ \_ -> pure x
  g <*> m = GhcT $ \s -> unGhcT g s <*> unGhcT m s

instance Monad m => Monad (GhcT m) where
  m >>= k  = GhcT $ \s -> do a <- unGhcT m s; unGhcT (k a) s

instance MonadIO m => MonadIO (GhcT m) where
  liftIO ioA = GhcT $ \_ -> liftIO ioA

instance MonadIO m => HasDynFlags (GhcT m) where
  getDynFlags = GhcT $ \(Session r) -> liftM hsc_dflags (liftIO $ readIORef r)

instance ExceptionMonad m => GhcMonad (GhcT m) where
  getSession = GhcT $ \(Session r) -> liftIO $ readIORef r
  setSession s' = GhcT $ \(Session r) -> liftIO $ writeIORef r s'


-- | Print the error message and all warnings.  Useful inside exception
--   handlers.  Clears warnings after printing.
printException :: (GhcMonad m) => SourceError -> m ()
printException err = do
  dflags <- getSessionDynFlags
  liftIO $ printBagOfErrors dflags
    (fmap ghcErrorMsg $ srcErrorMessages err)

-- | A function called to log warnings and errors.
type WarnErrLogger = forall m. GhcMonad m => Maybe SourceError -> m ()

defaultWarnErrLogger :: WarnErrLogger
defaultWarnErrLogger Nothing  = return ()
defaultWarnErrLogger (Just e) = printException e

data GhcError
  = MsgErr ErrMsg     -- escape hatch
  | PsErr ParseError
  | TcRnErr TcRnError

instance Show GhcError where
  show err = case err of
    MsgErr e              -> "MsgErr => " ++ show e
    PsErr (ParseError e)  -> "PsErr => " ++ show e
    TcRnErr (TcRnError e) -> "TcRnErr => " ++ show e

ghcErrorMsg :: GhcError -> ErrMsg
ghcErrorMsg (MsgErr e)  = e
ghcErrorMsg (PsErr e)   = parseErrorMsg e
ghcErrorMsg (TcRnErr e) = tcRnErrorMsg e

class IsGhcError e where
  toGhcError :: e -> GhcError

instance IsGhcError GhcError where
  toGhcError = id

instance IsGhcError ErrMsg where
  toGhcError = MsgErr

instance IsGhcError ParseError where
  toGhcError = PsErr

instance IsGhcError TcRnError where
  toGhcError = TcRnErr

-- -----------------------------------------------------------------------------
-- Source Errors

-- When the compiler (GHC.Driver.Main) discovers errors, it throws an
-- exception in the IO monad.

mkSrcErr :: IsGhcError e => ErrorMessages e -> SourceError
mkSrcErr = SourceError . fmap toGhcError

srcErrorMessages :: SourceError -> ErrorMessages GhcError
srcErrorMessages (SourceError msgs) = msgs

throwErrors :: (MonadIO io, IsGhcError e) => ErrorMessages e -> io a
throwErrors = liftIO . throwIO . mkSrcErr

-- | A source error is an error that is caused by one or more errors in the
-- source code.  A 'SourceError' is thrown by many functions in the
-- compilation pipeline.  Inside GHC these errors are merely printed via
-- 'log_action', but API clients may treat them differently, for example,
-- insert them into a list box.  If you want the default behaviour, use the
-- idiom:
--
-- > handleSourceError printExceptionAndWarnings $ do
-- >   ... api calls that may fail ...
--
-- The 'SourceError's error messages can be accessed via 'srcErrorMessages'.
-- This list may be empty if the compiler failed due to @-Werror@
-- ('Opt_WarnIsError').
--
-- See 'printExceptionAndWarnings' for more information on what to take care
-- of when writing a custom error handler.
newtype SourceError = SourceError (ErrorMessages GhcError)

instance Show SourceError where
  show (SourceError msgs) = unlines . map show . bagToList $ msgs

instance Exception SourceError

-- | Perform the given action and call the exception handler if the action
-- throws a 'SourceError'.  See 'SourceError' for more information.
handleSourceError :: ExceptionMonad m =>
                     (SourceError -> m a) -- ^ exception handler
                  -> m a -- ^ action to perform
                  -> m a
handleSourceError handler act = gcatch act handler
