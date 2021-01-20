{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module T10604_no_PolyKinds where

import GHC.Generics

data F (f :: * -> *) = F deriving Generic1
