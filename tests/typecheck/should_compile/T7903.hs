{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE KindSignatures #-}

module T7903 where

import Data.Kind (Type)

instance Eq (((->) a :: Type -> Type) b)
instance (Ord b) => Ord (((->) a) b)
