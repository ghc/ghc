module Language.Haskell.Syntax.StringMeta (
  StringMeta (..),
  defaultStrMeta,
) where

import Prelude

import Language.Haskell.Syntax.Module.Name (ModuleName)

-- libraries:
import Data.Data (Data)

-- -----------------------------------------------------------------------------
-- StringMeta

data StringMeta = StringMeta
  { strMetaMultiline  :: Bool
  , strMetaQualified  :: Maybe ModuleName
  }
  deriving (Show, Data)

defaultStrMeta :: StringMeta
defaultStrMeta =
  StringMeta
    { strMetaMultiline = False
    , strMetaQualified = Nothing
    }
