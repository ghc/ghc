{-# OPTIONS_GHC -funbox-strict-fields #-}
-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2010
--
-- The Session type and related functionality
--
-- -----------------------------------------------------------------------------

module GhcMonad (
        -- * 'Ghc' monad stuff
        GhcMonad(..),
        Ghc(..), 
        GhcT(..), liftGhcT,
        reflectGhc, reifyGhc,
        getSessionDynFlags, 
        liftIO,
        Session(..), withSession, modifySession, withTempSession,

        -- ** Warnings
        logWarnings, printException, printExceptionAndWarnings,
        WarnErrLogger, defaultWarnErrLogger
  ) where

import MonadUtils
import HscTypes
import DynFlags
import Exception
import ErrUtils

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
class (Functor m, MonadIO m, ExceptionMonad m, HasDynFlags m) => GhcMonad m where
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
  m `gfinally` setSession saved_session

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

-- -----------------------------------------------------------------------------
-- | A minimal implementation of a 'GhcMonad'.  If you need a custom monad,
-- e.g., to maintain additional state consider wrapping this monad or using
-- 'GhcT'.
newtype Ghc a = Ghc { unGhc :: Session -> IO a }

-- | The Session is a handle to the complete state of a compilation
-- session.  A compilation session consists of a set of modules
-- constituting the current program or library, the context for
-- interactive evaluation, and various caches.
data Session = Session !(IORef HscEnv) 

instance Functor Ghc where
  fmap f m = Ghc $ \s -> f `fmap` unGhc m s

instance Monad Ghc where
  return a = Ghc $ \_ -> return a
  m >>= g  = Ghc $ \s -> do a <- unGhc m s; unGhc (g a) s

instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA

instance MonadFix Ghc where
  mfix f = Ghc $ \s -> mfix (\x -> unGhc (f x) s)

instance ExceptionMonad Ghc where
  gcatch act handle =
      Ghc $ \s -> unGhc act s `gcatch` \e -> unGhc (handle e) s
  gmask f =
      Ghc $ \s -> gmask $ \io_restore ->
                             let
                                g_restore (Ghc m) = Ghc $ \s -> io_restore (m s)
                             in
                                unGhc (f g_restore) s

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
liftGhcT :: Monad m => m a -> GhcT m a
liftGhcT m = GhcT $ \_ -> m

instance Functor m => Functor (GhcT m) where
  fmap f m = GhcT $ \s -> f `fmap` unGhcT m s

instance Monad m => Monad (GhcT m) where
  return x = GhcT $ \_ -> return x
  m >>= k  = GhcT $ \s -> do a <- unGhcT m s; unGhcT (k a) s

instance MonadIO m => MonadIO (GhcT m) where
  liftIO ioA = GhcT $ \_ -> liftIO ioA

instance ExceptionMonad m => ExceptionMonad (GhcT m) where
  gcatch act handle =
      GhcT $ \s -> unGhcT act s `gcatch` \e -> unGhcT (handle e) s
  gmask f =
      GhcT $ \s -> gmask $ \io_restore ->
                           let
                              g_restore (GhcT m) = GhcT $ \s -> io_restore (m s)
                           in
                              unGhcT (f g_restore) s

instance (Functor m, ExceptionMonad m, MonadIO m) => HasDynFlags (GhcT m) where
  getDynFlags = getSessionDynFlags

instance (Functor m, ExceptionMonad m, MonadIO m) => GhcMonad (GhcT m) where
  getSession = GhcT $ \(Session r) -> liftIO $ readIORef r
  setSession s' = GhcT $ \(Session r) -> liftIO $ writeIORef r s'


-- | Print the error message and all warnings.  Useful inside exception
--   handlers.  Clears warnings after printing.
printException :: GhcMonad m => SourceError -> m ()
printException err = do
  dflags <- getSessionDynFlags
  liftIO $ printBagOfErrors dflags (srcErrorMessages err)

{-# DEPRECATED printExceptionAndWarnings "use printException instead" #-}
printExceptionAndWarnings :: GhcMonad m => SourceError -> m ()
printExceptionAndWarnings = printException

-- | A function called to log warnings and errors.
type WarnErrLogger = GhcMonad m => Maybe SourceError -> m ()

defaultWarnErrLogger :: WarnErrLogger
defaultWarnErrLogger Nothing  = return ()
defaultWarnErrLogger (Just e) = printException e

