
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module Data.Typeable where

import Data.Maybe
import GHC.Base
import GHC.Show

data TypeRep
data TyCon

mkTyCon      :: String -> TyCon
mkTyConApp   :: TyCon -> [TypeRep] -> TypeRep
showsTypeRep :: TypeRep -> ShowS

cast :: (Typeable a, Typeable b) => a -> Maybe b

class Typeable a where
  typeOf :: a -> TypeRep

