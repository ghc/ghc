
module Exception
    (
    module Control.Exception,
    module Exception
    )
    where

import Prelude hiding (catch)
import Control.Exception

#if __GLASGOW_HASKELL__ < 609
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

