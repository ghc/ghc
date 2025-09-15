{-# LANGUAGE AllowAmbiguousTypes #-}
module T14266 where

class A t where
  f :: forall x m. Monoid x => t m -> m
  f = undefined
instance A []
