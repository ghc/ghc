{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "System.IO.Unsafe.Compat"
-- from a globally unique namespace.
module System.IO.Unsafe.Compat.Repl (
  module System.IO.Unsafe.Compat
) where
import "this" System.IO.Unsafe.Compat
