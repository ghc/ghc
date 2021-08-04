{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Monoid.Compat"
-- from a globally unique namespace.
module Data.Monoid.Compat.Repl (
  module Data.Monoid.Compat
) where
import "this" Data.Monoid.Compat
