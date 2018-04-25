{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module T12616 where

type m ~> n = forall a. m a -> n a

class MonadTrans t where
  -- > this line works:
  -- lift :: (Monad m) => m a -> t m a
  -- > this line doesn't:
  lift :: (Monad m) => m ~> t m

data StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift xM = StateT $ \ s -> do { x <- xM ; return (x,s) }

newtype OtherStateT s m a = OtherStateT { runOtherStateT :: StateT s m a }
  deriving (MonadTrans)
