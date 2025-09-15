{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -ddump-deriv #-}

module GenDerivOutput where

import GHC.Generics (Generic, Generic1)

data List a = Nil | Cons { element :: a, rest :: List a }
  deriving (Generic, Generic1, Functor)

data Rose a = Empty | Rose a (List (Rose a))
  deriving (Generic, Generic1)
