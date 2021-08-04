{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Foreign.Marshal.Alloc.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Alloc.Compat.Repl (
  module Foreign.Marshal.Alloc.Compat
) where
import "this" Foreign.Marshal.Alloc.Compat
