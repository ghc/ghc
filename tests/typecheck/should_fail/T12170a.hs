{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fdefer-out-of-scope-variables #-}

module T12170a where

-- import Control.Monad  -- comment this out to cause error
import Data.IORef

class MonadRef m where
    type Ref m :: * -> *
    newRef :: a -> m (Ref m a)
    readRef :: Ref m a -> m a

instance MonadRef IO where
    type Ref IO = IORef
    newRef = newIORef
    readRef = readIORef

foo :: IO ()
foo = newRef (pure ()) >>= join . readRef
