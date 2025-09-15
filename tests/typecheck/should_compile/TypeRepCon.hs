{-# LANGUAGE ExplicitNamespaces #-}

module TypeRepCon1 where

import Type.Reflection
  ( TypeRep, data Con )

-- Simple test of the 'NotApplication' custom type error
-- in Data.Typeable.Internal.

isApp :: TypeRep (f a) -> Bool
isApp (Con _) = False -- Should warn about redundant pattern (insoluble Given)
isApp _       = True
