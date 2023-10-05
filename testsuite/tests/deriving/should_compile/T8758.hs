{-# LANGUAGE RankNTypes #-}

module T8758 where

class C m where
  foo :: (forall b. b -> m b) -> c -> m c

instance C [] where
  foo f c = f c