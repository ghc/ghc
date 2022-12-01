{-# LANGUAGE DataKinds #-}

module T22560b where

import Data.Kind

data P a b = MkP

type C :: forall i. (i -> i -> i) -> Constraint
class C @i a where
  p :: P a i

p' :: forall i (a :: i -> i -> i). C a => P a i
p' = p

type T1 :: forall k. Maybe k
type T1 @a = Nothing :: Maybe a

type T2 :: forall k. Maybe (Maybe k)
type T2 @a = Just (Nothing :: Maybe a)