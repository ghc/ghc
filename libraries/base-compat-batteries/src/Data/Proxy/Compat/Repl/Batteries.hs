{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Proxy.Compat"
-- from a globally unique namespace.
module Data.Proxy.Compat.Repl.Batteries (
  module Data.Proxy.Compat
) where
import "this" Data.Proxy.Compat
