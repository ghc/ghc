{-# LANGUAGE QuantifiedConstraints, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module T15290c where

import Prelude hiding ( Monad(..) )
import Data.Coerce ( Coercible )

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  join  :: m (m a) -> m a

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

instance Monad m => Monad (StateT s m) where
  ma >>= fmb = StateT $ \s -> runStateT ma s >>= \(s1, a) -> runStateT (fmb a) s1
  join ssa = StateT $ \s -> runStateT ssa s >>= \(s, sa) -> runStateT sa s

newtype IntStateT m a = IntStateT { runIntStateT :: StateT Int m a }

deriving instance (Monad m, forall p q. Coercible p q => Coercible (m p) (m q)) => Monad (IntStateT m)
