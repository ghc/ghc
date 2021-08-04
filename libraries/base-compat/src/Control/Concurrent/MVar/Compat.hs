{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Concurrent.MVar.Compat (
  module Base
, withMVarMasked
) where
import Control.Concurrent.MVar as Base

#if !(MIN_VERSION_base(4,7,0))
import Control.Exception (mask_, onException)
import Control.Monad (return)
import Data.Function (($))
import System.IO (IO)

{-|
  Like 'withMVar', but the @IO@ action in the second argument is executed
  with asynchronous exceptions masked.

  @since 4.7.0.0
-}
{-# INLINE withMVarMasked #-}
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked m io =
  mask_ $ do
    a <- takeMVar m
    b <- io a `onException` putMVar m a
    putMVar m a
    return b
#endif
