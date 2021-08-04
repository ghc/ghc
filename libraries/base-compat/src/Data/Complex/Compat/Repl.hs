{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Complex.Compat"
-- from a globally unique namespace.
module Data.Complex.Compat.Repl (
  module Data.Complex.Compat
) where
import "this" Data.Complex.Compat
