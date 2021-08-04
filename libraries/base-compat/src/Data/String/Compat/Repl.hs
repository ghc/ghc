{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.String.Compat"
-- from a globally unique namespace.
module Data.String.Compat.Repl (
  module Data.String.Compat
) where
import "this" Data.String.Compat
