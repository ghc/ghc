{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
 
module T8631 where
 
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Lazy
 
newtype AnyContT m a = AnyContT { unAnyContT :: forall r . ContT r m a }
 
class MonadAnyCont b m where
  anyContToM :: (forall r . (a -> b r) -> b r) -> m a
 
instance MonadAnyCont b (AnyContT m) where
  anyContToM _ = error "foo"
 
data DecodeState = DecodeState
 
newtype DecodeAST a = DecodeAST { unDecodeAST :: AnyContT (StateT DecodeState IO) a }
  deriving (MonadAnyCont IO)