{-# LANGUAGE QuantifiedConstraints, PolyKinds, ScopedTypeVariables
           , StandaloneDeriving, RoleAnnotations, TypeApplications
           , UndecidableInstances, InstanceSigs
           , GeneralizedNewtypeDeriving #-}

module T9123a where

import Data.Coerce

class MyMonad m where
  join :: m (m a) -> m a

newtype StateT s m a = StateT (s -> m (a, s))

type role StateT nominal representational nominal   -- as inferred

instance MyMonad m => MyMonad (StateT s m) where
  join = error "urk"  -- A good impl exists, but is not
                      -- of interest for this test case

newtype IntStateT m a = IntStateT (StateT Int m a)

type role IntStateT representational nominal   -- as inferred

instance (MyMonad m, forall p q. Coercible p q => Coercible (m p) (m q))
               => MyMonad (IntStateT m) where
  join :: forall a. IntStateT m (IntStateT m a) -> IntStateT m a
  join = coerce @(StateT Int m (StateT Int m a) -> StateT Int m a)
                @(IntStateT  m (IntStateT  m a) -> IntStateT  m a)
                join
