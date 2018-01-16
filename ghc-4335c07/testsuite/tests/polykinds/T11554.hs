{-# LANGUAGE GADTs, TypeInType, RankNTypes #-}

module T11554 where

import Data.Kind

data P (x :: k) = Q

data A :: Type where
  B :: forall (a :: A). P a -> A
