{-# LANGUAGE QuantifiedConstraints, MultiParamTypeClasses, PolyKinds #-}
-- NB: PolyKinds. This is actually accepted with -XNoPolyKinds because of defaulting.
-- See T17562b for the NoPolyKinds case.

module T17562 where

class (forall a. a b ~ a c) => C b c
