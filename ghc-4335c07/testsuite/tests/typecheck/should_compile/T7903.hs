{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE KindSignatures #-}

module T7903 where

instance Eq (((->) a :: * -> *) b)
instance (Ord b) => Ord (((->) a) b)
