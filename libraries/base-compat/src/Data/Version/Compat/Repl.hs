{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Version.Compat"
-- from a globally unique namespace.
module Data.Version.Compat.Repl (
  module Data.Version.Compat
) where
import "this" Data.Version.Compat
