{-# LANGUAGE GeneralizedNewtypeDeriving, RoleAnnotations #-}

module Roles9 where

type role C nominal
class C a where
  meth :: a -> a

instance C Int where
  meth = (+ 1)

newtype Age = MkAge Int
  deriving C
