{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Ratio.Compat"
-- from a globally unique namespace.
module Data.Ratio.Compat.Repl (
  module Data.Ratio.Compat
) where
import "this" Data.Ratio.Compat
