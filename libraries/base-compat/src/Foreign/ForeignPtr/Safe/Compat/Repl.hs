{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Foreign.ForeignPtr.Safe.Compat"
-- from a globally unique namespace.
module Foreign.ForeignPtr.Safe.Compat.Repl (
  module Foreign.ForeignPtr.Safe.Compat
) where
import "this" Foreign.ForeignPtr.Safe.Compat
