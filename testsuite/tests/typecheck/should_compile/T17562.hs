{-# LANGUAGE QuantifiedConstraints, MultiParamTypeClasses #-}

module T17562 where

class (forall a. a b ~ a c) => C b c
