{-# OPTIONS -fno-implicit-prelude #-}
module Data.Dynamic where
import {-# SOURCE #-} Data.Typeable (TypeRep)
data Dynamic
dynTypeRep :: Dynamic -> TypeRep
