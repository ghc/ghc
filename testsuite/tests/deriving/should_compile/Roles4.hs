{-# LANGUAGE RoleAnnotations #-}

module Roles4 where

class C1 a@N where
  meth1 :: a -> a

class C2 a@R where
  meth2 :: a -> a

type Syn1 a@N = [a]

class C3 a where
  meth3 :: a -> Syn1 a

