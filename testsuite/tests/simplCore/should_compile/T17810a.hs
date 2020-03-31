module T17810a where

import Control.Monad.Except

class Monad m => ReadTCState m where
  locallyTCState :: m ()
  liftReduce :: m ()

instance ReadTCState m => ReadTCState (ExceptT err m) where
  locallyTCState = undefined
  liftReduce = lift liftReduce

instance MonadIO m => ReadTCState (TCMT m) where
  locallyTCState = (undefined <$> liftReduce) <* TCM (\_ -> return ())
  liftReduce = undefined

newtype TCMT m a = TCM { unTCM :: () -> m a }

instance MonadIO m => Functor (TCMT m) where
  fmap f (TCM m) = TCM $ \r -> liftM f (m r )

instance MonadIO m => Applicative (TCMT m) where
  pure x = TCM (\_ -> return x)
  (<*>) (TCM mf) (TCM m) = TCM $ \r -> ap (mf r) (m r)

instance MonadIO m => Monad (TCMT m) where
  (>>=) (TCM m) k = TCM $ \r -> m r >>= \x -> unTCM (k x) r
