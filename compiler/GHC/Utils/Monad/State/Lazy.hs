{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A lazy state monad.
module GHC.Utils.Monad.State.Lazy
  ( -- * The State monda
    State(State)
  , state
  , evalState
  , execState
  , runState
    -- * Operations
  , get
  , gets
  , put
  , modify
  ) where

import GHC.Prelude

import GHC.Exts (oneShot)

-- | A state monad which is lazy in the state.
newtype State s a = State' { runState' :: s -> (# a, s #) }
    deriving (Functor)

pattern State :: (s -> (# a, s #))
              -> State s a

-- This pattern synonym makes the monad eta-expand,
-- which as a very beneficial effect on compiler performance
-- See #18202.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern State m <- State' m
  where
    State m = State' (oneShot $ \s -> m s)

instance Applicative (State s) where
   pure x   = State $ \s -> (# x, s #)
   m <*> n  = State $ \s -> case runState' m s of
                            (# f, s' #) -> case runState' n s' of
                                           (# x, s'' #) -> (# f x, s'' #)

instance Monad (State s) where
    m >>= n  = State $ \s -> case runState' m s of
                             (# r, s' #) -> runState' (n r) s'

state :: (s -> (a, s)) -> State s a
state f = State $ \s -> case f s of
                        (r, s') -> (# r, s' #)

get :: State s s
get = State $ \s -> (# s, s #)

gets :: (s -> a) -> State s a
gets f = State $ \s -> (# f s, s #)

put :: s -> State s ()
put s' = State $ \_ -> (# (), s' #)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (# (), f s #)


evalState :: State s a -> s -> a
evalState s i = case runState' s i of
                (# a, _ #) -> a


execState :: State s a -> s -> s
execState s i = case runState' s i of
                (# _, s' #) -> s'


runState :: State s a -> s -> (a, s)
runState s i = case runState' s i of
               (# a, s' #) -> (a, s')
