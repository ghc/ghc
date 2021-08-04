{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Debug.Trace.Compat"
-- from a globally unique namespace.
module Debug.Trace.Compat.Repl (
  module Debug.Trace.Compat
) where
import "this" Debug.Trace.Compat
