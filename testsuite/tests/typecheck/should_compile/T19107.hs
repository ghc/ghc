{-# LANGUAGE GADTs #-}
-- NB: NO FlexibleContexts

module T19107 where

data T a where
  MkT :: Show a => [a] -> T a

f (MkT x) = show x
