--!!! Local universal quantification.
module ShouldSucceed where

import PrelGHC -- to get at All

data Monad2 m = MkMonad2 (All a => a -> m a)
                         ((All a, All b) =>  m a -> (a -> m b) -> m b)

halfListMonad  :: ((All a, All b) => [a] -> (a -> [b]) -> [b]) -> Monad2 []
halfListMonad b = MkMonad2 (\x -> [x]) b
