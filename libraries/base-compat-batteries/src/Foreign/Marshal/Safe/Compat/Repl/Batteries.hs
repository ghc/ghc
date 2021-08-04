{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Foreign.Marshal.Safe.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Safe.Compat.Repl.Batteries (
  module Foreign.Marshal.Safe.Compat
) where
import "this" Foreign.Marshal.Safe.Compat
