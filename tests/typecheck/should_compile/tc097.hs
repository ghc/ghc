{-# LANGUAGE RankNTypes #-}
-- !!! Local universal quantification.
module ShouldSucceed where

data Monad2 m = MkMonad2 (forall a. a -> m a)
                         (forall a b.  m a -> (a -> m b) -> m b)

halfListMonad  :: (forall a b. [a] -> (a -> [b]) -> [b]) -> Monad2 []
halfListMonad b = MkMonad2 (\x -> [x]) b
