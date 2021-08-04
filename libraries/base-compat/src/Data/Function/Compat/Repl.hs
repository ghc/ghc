{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Function.Compat"
-- from a globally unique namespace.
module Data.Function.Compat.Repl (
  module Data.Function.Compat
) where
import "this" Data.Function.Compat
