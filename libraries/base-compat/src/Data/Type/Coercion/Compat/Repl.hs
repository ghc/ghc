{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Type.Coercion.Compat"
-- from a globally unique namespace.
module Data.Type.Coercion.Compat.Repl (
  module Data.Type.Coercion.Compat
) where
import "this" Data.Type.Coercion.Compat
