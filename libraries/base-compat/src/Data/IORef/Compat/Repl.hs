{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.IORef.Compat"
-- from a globally unique namespace.
module Data.IORef.Compat.Repl (
  module Data.IORef.Compat
) where
import "this" Data.IORef.Compat
