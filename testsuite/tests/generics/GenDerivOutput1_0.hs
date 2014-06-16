{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -ddump-deriv #-}

module GenDerivOutput1_0 where

import GHC.Generics (Generic1)

data List a = Nil | Cons { element :: a, rest :: List a } deriving Generic1
