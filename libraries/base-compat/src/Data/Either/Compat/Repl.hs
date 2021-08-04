{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Either.Compat"
-- from a globally unique namespace.
module Data.Either.Compat.Repl (
  module Data.Either.Compat
) where
import "this" Data.Either.Compat
