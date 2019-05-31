{-# LANGUAGE PolyKinds, FunctionalDependencies, MultiParamTypeClasses,
             TopLevelKindSignatures #-}

module T9201 where

import Data.Kind

type MonoidalCCC :: (x -> y) -> (y -> y -> Type) -> Constraint
class MonoidalCCC f d | f -> d where
  ret :: d a (f a)
