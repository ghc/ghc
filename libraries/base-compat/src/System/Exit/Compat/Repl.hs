{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "System.Exit.Compat"
-- from a globally unique namespace.
module System.Exit.Compat.Repl (
  module System.Exit.Compat
) where
import "this" System.Exit.Compat
