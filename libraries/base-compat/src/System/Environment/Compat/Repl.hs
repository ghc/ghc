{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "System.Environment.Compat"
-- from a globally unique namespace.
module System.Environment.Compat.Repl (
  module System.Environment.Compat
) where
import "this" System.Environment.Compat
