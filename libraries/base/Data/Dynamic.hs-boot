{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module Data.Dynamic where
import {-# SOURCE #-} Data.Typeable (TypeRep)
data Dynamic
dynTypeRep :: Dynamic -> TypeRep
