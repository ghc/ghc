module GHC.Types.StringMeta (
  StringMeta (..),
  emptyStrMeta,
  defaultStrMeta,
) where

import GHC.Prelude

import Data.Data (Data)
import GHC.Types.SourceText (SourceText (..))

data StringMeta = StringMeta
  { strMetaSrc :: SourceText
    -- ^ Note [Literal source text] in "GHC.Types.SourceText"
  , strMetaMultiline  :: Bool
  }
  deriving (Show, Data)

emptyStrMeta :: StringMeta
emptyStrMeta =
  StringMeta
    { strMetaSrc = NoSourceText
    , strMetaMultiline = False
    }

defaultStrMeta :: SourceText -> StringMeta
defaultStrMeta src = emptyStrMeta{strMetaSrc = src}
