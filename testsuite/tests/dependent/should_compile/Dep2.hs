{-# LANGUAGE TypeInType, GADTs #-}

module Dep2 where

data G (a :: k) where
  G1 :: G Int
  G2 :: G Maybe
