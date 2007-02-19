
{-# OPTIONS -fno-implicit-prelude #-}

module Data.Typeable where

import GHC.Base

data TypeRep
data TyCon

mkTyCon :: String -> TyCon
mkTyConApp :: TyCon -> [TypeRep] -> TypeRep

class Typeable a where
  typeOf :: a -> TypeRep

