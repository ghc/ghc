{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Control.Monad.Fail.Compat"
-- from a globally unique namespace.
module Control.Monad.Fail.Compat.Repl (
  module Control.Monad.Fail.Compat
) where
import "this" Control.Monad.Fail.Compat
