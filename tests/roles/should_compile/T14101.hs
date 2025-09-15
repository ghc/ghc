{-# LANGUAGE RoleAnnotations #-}
module T14101 where

type role Array representational
data Array a

type Arr = Array

data Foo a = Foo (Arr a)
type role Foo representational
