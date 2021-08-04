{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Bifoldable.Compat"
-- from a globally unique namespace.
module Data.Bifoldable.Compat.Repl (
  module Data.Bifoldable.Compat
) where
import "this" Data.Bifoldable.Compat
