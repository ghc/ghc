{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Control.Exception.Compat"
-- from a globally unique namespace.
module Control.Exception.Compat.Repl (
  module Control.Exception.Compat
) where
import "this" Control.Exception.Compat
