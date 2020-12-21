{-# LANGUAGE QuantifiedConstraints, MultiParamTypeClasses, TypeFamilies #-}
-- NB: No PolyKinds

module T17562b where

class (forall a. a b ~ a c) => C b c
