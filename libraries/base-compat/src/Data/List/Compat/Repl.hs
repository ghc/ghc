{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.List.Compat"
-- from a globally unique namespace.
module Data.List.Compat.Repl (
  module Data.List.Compat
) where
import "this" Data.List.Compat
