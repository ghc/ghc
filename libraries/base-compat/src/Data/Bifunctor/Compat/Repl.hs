{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Bifunctor.Compat"
-- from a globally unique namespace.
module Data.Bifunctor.Compat.Repl (
  module Data.Bifunctor.Compat
) where
import "this" Data.Bifunctor.Compat
