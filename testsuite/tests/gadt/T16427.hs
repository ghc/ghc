{-# LANGUAGE RankNTypes, GADTs #-}

module T16427 where

data D where C :: Int -> forall b . b -> D
