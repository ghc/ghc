{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.List.NonEmpty.Compat"
-- from a globally unique namespace.
module Data.List.NonEmpty.Compat.Repl (
  module Data.List.NonEmpty.Compat
) where
import "this" Data.List.NonEmpty.Compat
