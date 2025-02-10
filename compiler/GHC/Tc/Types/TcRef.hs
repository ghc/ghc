module GHC.Tc.Types.TcRef (TcRef, newTcRef, readTcRef, writeTcRef, updTcRef, updTcRefM) where

import GHC.Prelude

import Control.Monad.IO.Class
import Data.IORef

-- | Type alias for 'IORef'; the convention is we'll use this for mutable
-- bits of data in the typechecker which are updated during typechecking and
-- returned at the end.
type TcRef a = IORef a

-- The following functions are all marked INLINE so that we
-- don't end up passing a Monad or MonadIO dictionary.

newTcRef :: MonadIO m => a -> m (TcRef a)
newTcRef = \ a -> liftIO $ newIORef a
{-# INLINE newTcRef #-}

readTcRef :: MonadIO m => TcRef a -> m a
readTcRef = \ ref -> liftIO $ readIORef ref
{-# INLINE readTcRef #-}

writeTcRef :: MonadIO m => TcRef a -> a -> m ()
writeTcRef = \ ref a -> liftIO $ writeIORef ref a
{-# INLINE writeTcRef #-}

updTcRef :: MonadIO m => TcRef a -> (a -> a) -> m ()
updTcRef = \ ref fn -> liftIO $ modifyIORef' ref fn
{-# INLINE updTcRef #-}

updTcRefM :: MonadIO m => TcRef a -> (a -> m a) -> m ()
updTcRefM ref upd
  = do { contents      <- readTcRef ref
       ; !new_contents <- upd contents
       ; writeTcRef ref new_contents }
{-# INLINE updTcRefM #-}
