{-# LANGUAGE TypeFamilies, TypeOperators, GADTs,  RankNTypes, FlexibleContexts #-}
module Equality( (:=:), eq_elim, eq_refl ) where

data a:=: b where
  EQUAL :: a :=: a

eq_refl :: a :=: a
eq_refl = EQUAL

eq_elim :: (a~b) => a :=: b -> (a~b => p) -> p
eq_elim EQUAL p = p 
