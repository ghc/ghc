module Language.Haskell.Syntax.Module.Name
  ( HsModuleName(..)
  , hsModuleNameString
  ) where

import Prelude
import Data.Data

import Language.Haskell.Syntax.Text

-- | A module name in the AST is just 'HText'
newtype HsModuleName = HsModuleName HText
  deriving (Eq, Ord, Show, Data)

hsModuleNameString :: HsModuleName -> String
hsModuleNameString (HsModuleName mn) = unpackHText mn
