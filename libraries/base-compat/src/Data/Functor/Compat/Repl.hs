{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Functor.Compat"
-- from a globally unique namespace.
module Data.Functor.Compat.Repl (
  module Data.Functor.Compat
) where
import "this" Data.Functor.Compat
