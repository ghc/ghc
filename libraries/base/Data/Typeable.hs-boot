{-# LANGUAGE NoImplicitPrelude #-}

module Data.Typeable where

import Data.Maybe
import GHC.Base
import {-# SOURCE #-} Data.Typeable.Internal

mkTyConApp   :: TyCon -> [TypeRep] -> TypeRep

cast :: (Typeable a, Typeable b) => a -> Maybe b

class Typeable a where
  typeOf :: a -> TypeRep

