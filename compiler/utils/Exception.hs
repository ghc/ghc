
module Exception
    (
    module Control.Exception,
    module Exception
    )
    where

import Prelude hiding (catch)
import Control.Exception

#if __GLASGOW_HASKELL__ < 609
import Data.Typeable ( Typeable )

type SomeException = Exception

onException :: IO a -> IO () -> IO a
onException io what = io `catch` \e -> do what
                                          throw e
#endif

catchIO :: IO a -> (IOException -> IO a) -> IO a
#if __GLASGOW_HASKELL__ >= 609
catchIO = catch
#else
catchIO io handler = io `catch` handler'
    where handler' (IOException ioe) = handler ioe
          handler' e                 = throw e
#endif

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
#if __GLASGOW_HASKELL__ >= 609
tryIO = try
#else
tryIO io = do ei <- try io
              case ei of
                  Right v -> return (Right v)
                  Left (IOException ioe) -> return (Left ioe)
                  Left e -> throwIO e
#endif

-- | A monad that can catch exceptions.  A minimal definition
-- requires a definition of 'gcatch'.
--
-- Although, 'gbracket' and 'gfinally' could be modelled on top of 'gcatch',
-- they are included in the type class since GHC needs special implementations
-- of these in order to properly handle asynchronous exceptions.
class Monad m => ExceptionMonad m where
  -- | Generalised version of 'Control.Exception.catch', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
#if __GLASGOW_HASKELL__ >= 609
  gcatch :: Exception e => m a -> (e -> m a) -> m a
#else
  gcatch :: m a -> (Exception -> m a) -> m a
  gcatchDyn :: Typeable e => m a -> (e -> m a) -> m a
#endif

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
#if __GLASGOW_HASKELL__ < 609
  gcatchDyn = catchDyn
#endif
  gbracket  = bracket
  gfinally  = finally


#if __GLASGOW_HASKELL__ >= 609
gtry :: (ExceptionMonad m, Exception e) => m a -> m (Either e a)
#else
gtry :: (ExceptionMonad m) => m a -> m (Either Exception a)
#endif
gtry act = gcatch (act >>= \a -> return (Right a))
                  (\e -> return (Left e))

-- | Generalised version of 'Control.Exception.handle', allowing an arbitrary
-- exception handling monad instead of just 'IO'.
#if __GLASGOW_HASKELL__ >= 609
ghandle :: (ExceptionMonad m, Exception e) => (e -> m a) -> m a -> m a
#else
ghandle :: (ExceptionMonad m) => (Exception -> m a) -> m a -> m a
#endif
ghandle = flip gcatch

-- | Always executes the first argument.  If this throws an exception the
-- second argument is executed and the exception is raised again.
gonException :: (ExceptionMonad m) => m a -> m b -> m a
gonException ioA cleanup = ioA `gcatch` \e ->
                             do cleanup
#if __GLASGOW_HASKELL__ >= 609
                                throw (e :: SomeException)
#else
                                throw e
#endif
