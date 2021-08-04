{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Void.Compat"
-- from a globally unique namespace.
module Data.Void.Compat.Repl (
  module Data.Void.Compat
) where
import "this" Data.Void.Compat
