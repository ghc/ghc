{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Semigroup.Compat"
-- from a globally unique namespace.
module Data.Semigroup.Compat.Repl (
  module Data.Semigroup.Compat
) where
import "this" Data.Semigroup.Compat
