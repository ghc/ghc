{-# LANGUAGE TypeApplications, ImpredicativeTypes, ScopedTypeVariables,
             QuantifiedConstraints, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module T15290a where

import Prelude hiding ( Monad(..) )
import Data.Coerce ( Coercible, coerce )

class Monad m where
  join  :: m (m a) -> m a

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

newtype IntStateT m a = IntStateT { runIntStateT :: StateT Int m a }

instance Monad m => Monad (StateT s m) where
  join = error "urk"

instance (Monad m, forall p q. Coercible p q => Coercible (m p) (m q))
      => Monad (IntStateT m) where

--   Fails with the impredicative instantiation of coerce, succeeds without

-- Impredicative version (fails)
    join = coerce
          @(forall a. StateT Int m (StateT Int m a) -> StateT Int m a)
          @(forall a. IntStateT m (IntStateT m a)   -> IntStateT m a)
          join


-- Predicative version (succeeds)
--    join = (coerce
--           @(StateT Int m (StateT Int m a) -> StateT Int m a)
--           @(IntStateT m (IntStateT m a)   -> IntStateT m a)
--           join) :: forall a. IntStateT m (IntStateT m a)   -> IntStateT m a
