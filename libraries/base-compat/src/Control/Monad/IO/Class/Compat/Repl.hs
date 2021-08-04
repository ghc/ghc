{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}
-- | Reexports "Control.Monad.IO.Class.Compat"
-- from a globally unique namespace.
module Control.Monad.IO.Class.Compat.Repl (
  module Control.Monad.IO.Class.Compat
) where
import "this" Control.Monad.IO.Class.Compat
