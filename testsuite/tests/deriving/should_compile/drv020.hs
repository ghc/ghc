{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- Test deriving of a multi-parameter class for 
-- one-argument newtype defined in the same module
module ShouldSucceed where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

-- library stuff

class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

newtype State s a = State {
                           runState :: (s -> (a, s))
                          }

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Monad (State s) where
	return a = State $ \s -> (a, s)
	m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'

instance MonadState s (State s) where
	get   = State $ \s -> (s, s)
	put s = State $ \_ -> ((), s)

-- test code

newtype Foo a = MkFoo (State Int a)
 deriving (Functor, Applicative, Monad, MonadState Int)

f :: Foo Int
f = get
