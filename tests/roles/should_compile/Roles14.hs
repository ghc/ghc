{-# LANGUAGE RoleAnnotations, IncoherentInstances #-}

module Roles12 where

type role C2 representational
class C2 a where
  meth2 :: a -> a
