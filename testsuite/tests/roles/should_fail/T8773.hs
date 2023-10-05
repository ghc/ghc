{-# LANGUAGE RoleAnnotations #-}

module T8773 where

type role C2 representational
class C2 a where
  meth2 :: a -> a
