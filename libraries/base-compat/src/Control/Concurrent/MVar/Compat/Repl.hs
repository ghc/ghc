{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Control.Concurrent.MVar.Compat"
-- from a globally unique namespace.
module Control.Concurrent.MVar.Compat.Repl (
  module Control.Concurrent.MVar.Compat
) where
import "this" Control.Concurrent.MVar.Compat
