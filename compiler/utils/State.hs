{-# LANGUAGE UnboxedTuples #-}

module State (module State, mapAccumLM {- XXX hack -}) where

import MonadUtils

newtype State s a = State { runState' :: s -> (# a, s #) }

instance Functor (State s) where
    fmap f m  = State $ \s -> case runState' m s of
                              (# r, s' #) -> (# f r, s' #)

instance Applicative (State s) where
   pure x   = State $ \s -> (# x, s #)
   m <*> n  = State $ \s -> case runState' m s of
                            (# f, s' #) -> case runState' n s' of
                                           (# x, s'' #) -> (# f x, s'' #)

instance Monad (State s) where
    return x = State $ \s -> (# x, s #)
    m >>= n  = State $ \s -> case runState' m s of
                             (# r, s' #) -> runState' (n r) s'

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
