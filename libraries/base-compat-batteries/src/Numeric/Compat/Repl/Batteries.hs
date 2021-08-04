{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Numeric.Compat"
-- from a globally unique namespace.
module Numeric.Compat.Repl.Batteries (
  module Numeric.Compat
) where
import "this" Numeric.Compat
