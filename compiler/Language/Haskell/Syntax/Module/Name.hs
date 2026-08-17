{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Syntax.Module.Name where

import Prelude

import Control.DeepSeq

import Language.Haskell.Syntax.Extension

-- | A ModuleName is essentially a simple string, e.g. @Data.List@.
--
-- The concrete representation of the string is selected by the consumer
-- of the syntax tree.
data ModuleNameP pass
  = ModuleName !(XCModuleName pass)
  | XModuleName !(XXModuleName pass)

deriving instance (Eq (XCModuleName pass), Eq (XXModuleName pass)) => Eq (ModuleNameP pass)
deriving instance (Show (XCModuleName pass), Show (XXModuleName pass)) => Show (ModuleNameP pass)

instance (NFData (XCModuleName pass), NFData (XXModuleName pass)) => NFData (ModuleNameP pass) where
  rnf (ModuleName mod) = rnf mod
  rnf (XModuleName ext) = rnf ext
