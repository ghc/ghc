module GHC.Data.StringMeta (
  StringMeta (..),
  defaultStrMeta,
) where

import GHC.Prelude

import Data.Data (Data)
import Language.Haskell.Syntax.Module.Name (ModuleName)

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
