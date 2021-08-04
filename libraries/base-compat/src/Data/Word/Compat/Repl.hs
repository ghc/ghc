{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Data.Word.Compat"
-- from a globally unique namespace.
module Data.Word.Compat.Repl (
  module Data.Word.Compat
) where
import "this" Data.Word.Compat
