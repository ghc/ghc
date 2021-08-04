{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Bool.Compat"
-- from a globally unique namespace.
module Data.Bool.Compat.Repl (
  module Data.Bool.Compat
) where
import "this" Data.Bool.Compat
