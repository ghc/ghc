-- The method `alterF` inside containers is marked as INLINEABLE
-- and hence should be specialized to the `CheckRes` Functor.

-- This only started to work with 9.6, this test checks that we don't
-- regress on this.

{-# language LambdaCase, Strict, DeriveFunctor, DerivingStrategies #-}

module T22611 where

import Data.Map.Strict as Map
import qualified Data.Map.Strict as Map

foo :: Either Int Char -> Map (Either Int Char) v -> Maybe (v, (Map (Either Int Char) v))
foo x subst = case Map.alterF alt x subst of
  NotFound -> foo (fmap (toEnum . (+1) . fromEnum) x) subst
  Found p q -> Just (p, q)
  where
    alt :: Maybe a1 -> CheckRes a1 (Maybe a2)
    alt = (\case {Nothing -> NotFound; Just t -> Found t Nothing})

data CheckRes a m = NotFound | Found !a ~m
  deriving stock Functor
