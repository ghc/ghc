{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Foreign.ForeignPtr.Unsafe.Compat"
-- from a globally unique namespace.
module Foreign.ForeignPtr.Unsafe.Compat.Repl (
  module Foreign.ForeignPtr.Unsafe.Compat
) where
import "this" Foreign.ForeignPtr.Unsafe.Compat
