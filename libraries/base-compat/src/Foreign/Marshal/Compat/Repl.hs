{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Foreign.Marshal.Compat"
-- from a globally unique namespace.
module Foreign.Marshal.Compat.Repl (
  module Foreign.Marshal.Compat
) where
import "this" Foreign.Marshal.Compat
