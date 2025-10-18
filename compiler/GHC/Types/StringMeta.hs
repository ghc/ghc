module GHC.Types.StringMeta (
  StringMeta (..),
  emptyStrMeta,
  defaultStrMeta,
) where

import GHC.Prelude

import Data.Data (Data)
import GHC.Types.SourceText (SourceText (..))
import Language.Haskell.Syntax.Module.Name (ModuleName)

data StringMeta = StringMeta
  { strMetaSrc :: SourceText
    -- ^ Note [Literal source text] in "GHC.Types.SourceText"
  , strMetaMultiline  :: Bool
  , strMetaQualified  :: Maybe ModuleName
  }
  deriving (Show, Data)

emptyStrMeta :: StringMeta
emptyStrMeta =
  StringMeta
    { strMetaSrc = NoSourceText
    , strMetaMultiline = False
    , strMetaQualified = Nothing
    }

defaultStrMeta :: SourceText -> StringMeta
defaultStrMeta src = emptyStrMeta{strMetaSrc = src}
