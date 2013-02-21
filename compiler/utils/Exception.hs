{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Exception
    (
    module Control.Exception,
    module Exception
    )
    where

import Control.Exception
import Control.Monad.IO.Class

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = Control.Exception.catch

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | A monad that can catch exceptions.  A minimal definition
-- requires a definition of 'gcatch'.
--
-- Implementations on top of 'IO' should implement 'gmask' to
-- eventually call the primitive 'Control.Exception.mask'.
-- These are used for
-- implementations that support asynchronous exceptions.  The default
-- implementations of 'gbracket' and 'gfinally' use 'gmask'
-- thus rarely require overriding.
--
class MonadIO m => ExceptionMonad m where

  -- | Generalised version of 'Control.Exception.catch', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gcatch :: Exception e => m a -> (e -> m a) -> m a

  -- | Generalised version of 'Control.Exception.mask_', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gmask :: ((m a -> m a) -> m b) -> m b

  -- | Generalised version of 'Control.Exception.bracket', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gbracket :: m a -> (a -> m b) -> (a -> m c) -> m c

  -- | Generalised version of 'Control.Exception.finally', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gfinally :: m a -> m b -> m a

  gbracket before after thing =
    gmask $ \restore -> do
      a <- before
      r <- restore (thing a) `gonException` after a
      _ <- after a
      return r

  a `gfinally` sequel =
    gmask $ \restore -> do
      r <- restore a `gonException` sequel
      _ <- sequel
      return r

instance ExceptionMonad IO where
  gcatch    = Control.Exception.catch
  gmask f   = mask (\x -> f x)

gtry :: (ExceptionMonad m, Exception e) => m a -> m (Either e a)
gtry act = gcatch (act >>= \a -> return (Right a))
                  (\e -> return (Left e))

-- | Generalised version of 'Control.Exception.handle', allowing an arbitrary
-- exception handling monad instead of just 'IO'.
ghandle :: (ExceptionMonad m, Exception e) => (e -> m a) -> m a -> m a
ghandle = flip gcatch

-- | Always executes the first argument.  If this throws an exception the
-- second argument is executed and the exception is raised again.
gonException :: (ExceptionMonad m) => m a -> m b -> m a
gonException ioA cleanup = ioA `gcatch` \e ->
                             do _ <- cleanup
                                liftIO $ throwIO (e :: SomeException)

