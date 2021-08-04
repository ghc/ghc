{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Numeric.Natural.Compat"
-- from a globally unique namespace.
module Numeric.Natural.Compat.Repl (
  module Numeric.Natural.Compat
) where
import "this" Numeric.Natural.Compat
