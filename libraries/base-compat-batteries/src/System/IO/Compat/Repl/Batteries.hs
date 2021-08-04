{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "System.IO.Compat"
-- from a globally unique namespace.
module System.IO.Compat.Repl.Batteries (
  module System.IO.Compat
) where
import "this" System.IO.Compat
