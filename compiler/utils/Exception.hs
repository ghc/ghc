
module Exception
    (
    module Control.Exception,
    module Exception
    )
    where

import Prelude hiding (catch)

#if __GLASGOW_HASKELL__ < 609
import Control.Exception.Extensible as Control.Exception
#else
import Control.Exception
#endif

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | A monad that can catch exceptions.  A minimal definition
-- requires a definition of 'gcatch'.
--
-- Although, 'gbracket' and 'gfinally' could be modelled on top of 'gcatch',
-- they are included in the type class since GHC needs special implementations
-- of these in order to properly handle asynchronous exceptions.
class Monad m => ExceptionMonad m where
  -- | Generalised version of 'Control.Exception.catch', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gcatch :: Exception e => m a -> (e -> m a) -> m a

  -- | Generalised version of 'Control.Exception.bracket', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gbracket :: m a -> (a -> m b) -> (a -> m c) -> m c

  -- | Generalised version of 'Control.Exception.finally', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gfinally :: m a -> m b -> m a

  gbracket acquire release in_between = do
      a <- acquire
      r <- in_between a `gonException` release a
      release a
      return r

  gfinally thing cleanup = do
      r <- thing `gonException` cleanup
      cleanup
      return r

instance ExceptionMonad IO where
  gcatch    = catch
  gbracket  = bracket
  gfinally  = finally


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
                             do cleanup
                                throw (e :: SomeException)

