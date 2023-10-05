{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Roles10 where

type family F a
type instance F Int = Bool
type instance F Age = Char

class C a where
  meth :: a -> F a

instance C Int where
  meth = (> 0)

newtype Age = MkAge Int
  deriving C