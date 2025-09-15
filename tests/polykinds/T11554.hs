{-# LANGUAGE GADTs, PolyKinds, RankNTypes, TypeApplications, DataKinds #-}

module T11554 where

import Data.Kind

data P1 (x :: k) = Q1
data A1 :: Type where
  B1 :: forall (a :: A1). P1 a -> A1

data P2 k (x :: k) = Q2
data A2 :: Type where
  B2 :: P2 A2 a -> A2

data P3 (x :: k) = Q3
data A3 :: Type where
  B3 :: P3 @A3 a -> A3
