{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Bits.Compat"
-- from a globally unique namespace.
module Data.Bits.Compat.Repl (
  module Data.Bits.Compat
) where
import "this" Data.Bits.Compat
