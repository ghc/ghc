
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module Data.Typeable where

import Data.Maybe
import GHC.Base

data TypeRep
data TyCon

mkTyCon      :: String -> TyCon
mkTyConApp   :: TyCon -> [TypeRep] -> TypeRep

cast :: (Typeable a, Typeable b) => a -> Maybe b

class Typeable a where
  typeOf :: a -> TypeRep

