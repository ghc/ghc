{-# LANGUAGE QuantifiedConstraints, DatatypeContexts, PolyKinds #-}
-- NB: This actually works with -XNoPolyKinds, due to defaulting.

module T17567StupidTheta where

data (forall a. a b ~ a c) => T b c
