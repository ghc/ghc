{-# LANGUAGE GADTs #-}
module T18791 where

data T where
  MkT :: Int -> T
