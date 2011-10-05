{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -ddump-deriv #-}

module GenDerivOutput where

import GHC.Generics (Generic)

data List a = Nil | Cons { element :: a, rest :: List a } deriving Generic
