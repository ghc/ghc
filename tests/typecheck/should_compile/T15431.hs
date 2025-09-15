{-# LANGUAGE GADTs, FlexibleContexts #-}

module T15431 where

import Data.Coerce

data T t where
  A :: Show (t a) => t a -> T t
  B :: Coercible Int (t a) => t a -> T t

f :: T t -> String
f (A t) = show t

g :: T t -> Int
g (B t) = coerce t
