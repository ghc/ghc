{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module T10340 where

import GHC.Exts (Any)

class MonadState s m where
  get :: m s

newtype State s a = State (s -> (s, a))

instance MonadState s (State s) where
  get = State $ \s -> (s, s)

foo :: State Any Any
foo = get
