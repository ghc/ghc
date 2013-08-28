{-# LANGUAGE RoleAnnotations #-}

module Roles4 where

type role C1 nominal
class C1 a where
  meth1 :: a -> a

type role C2 representational
class C2 a where
  meth2 :: a -> a

type Syn1 a = [a]

class C3 a where
  meth3 :: a -> Syn1 a

