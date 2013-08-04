{-# LANGUAGE GeneralizedNewtypeDeriving, RoleAnnotations #-}

module Roles9 where

class C a@N where
  meth :: a -> a

instance C Int where
  meth = (+ 1)

newtype Age = MkAge Int
  deriving C
