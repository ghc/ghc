{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Prelude.Compat"
-- from a globally unique namespace.
module Prelude.Compat.Repl (
  module Prelude.Compat
) where
import "this" Prelude.Compat
