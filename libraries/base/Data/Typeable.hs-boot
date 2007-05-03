
{-# OPTIONS -fno-implicit-prelude #-}

module Data.Typeable where

import GHC.Base
import GHC.Show

data TypeRep
data TyCon

mkTyCon      :: String -> TyCon
mkTyConApp   :: TyCon -> [TypeRep] -> TypeRep
showsTypeRep :: TypeRep -> ShowS

class Typeable a where
  typeOf :: a -> TypeRep

