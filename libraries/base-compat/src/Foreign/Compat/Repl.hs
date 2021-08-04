{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Foreign.Compat"
-- from a globally unique namespace.
module Foreign.Compat.Repl (
  module Foreign.Compat
) where
import "this" Foreign.Compat
