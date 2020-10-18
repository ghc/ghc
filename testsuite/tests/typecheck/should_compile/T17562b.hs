{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE QuantifiedConstraints, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
-- NB: No PolyKinds

module T17562b where

class (forall a. a b ~ a c) => C b c
