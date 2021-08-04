{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "System.IO.Error.Compat"
-- from a globally unique namespace.
module System.IO.Error.Compat.Repl.Batteries (
  module System.IO.Error.Compat
) where
import "this" System.IO.Error.Compat
