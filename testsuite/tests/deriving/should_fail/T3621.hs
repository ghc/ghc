{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
module T3621 where

-- This one is ok, even though the deriving clause mentions 'a'
-- which is not a parameter of 'T'
class C a b
instance C a S
data S = MkS

newtype T = MkT S deriving( C a )


-- But this one fails, and should fail
class (Monad m) => MonadState s m | m -> s where
  state :: (s -> (a, s)) -> m a

newtype State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where {}
instance Applicative (State s) where {}
instance Monad (State s) where {}
instance MonadState s (State s) where {}

newtype WrappedState s a = WS { runWS :: State s a }
   deriving (Functor, Applicative, Monad, MonadState state)
--   deriving (Monad)

deriving instance (MonadState state (State s))
      => MonadState state (WrappedState s)

-- ASSERT error
-- deriving instance (MonadState state (State s), Monad (WrappedState s))
--      => MonadState s (WrappedState s)


-- We try
--   instance MonadState state (State state a)
--         => MonadState state (WrappedState state a)
--
-- Superclass needs (Monad (WrappedState state a))
