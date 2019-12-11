{-# LANGUAGE QuantifiedConstraints, MultiParamTypeClasses,
             PolyKinds #-}

module T17562b where

class (forall a. a b ~ a c) => C b c
