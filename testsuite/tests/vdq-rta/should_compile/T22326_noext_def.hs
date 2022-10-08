-- Helper module for use in T22326_noext.
-- Defines id_vdq.

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_noext_def (id_vdq) where

id_vdq :: forall a -> a -> a
id_vdq = \(type _) -> id