{-# LANGUAGE QuantifiedConstraints, DatatypeContexts, TypeFamilies #-}
-- NB: -XNoPolyKinds, to get defaulting.

module T17567StupidThetaB where

data (forall a. a b ~ a c) => T b c
