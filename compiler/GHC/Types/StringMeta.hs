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

{-
Note [Implementation of QualifiedStrings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
QualifiedStrings reuses the HsString constructor in HsLit, with the qualified
module name stored in the strMetaQualified field in the StringMeta metadata.
Then we check this field in every location where qualified strings differ from
normal strings:
* Desugaring qualified string expressions in GHC.Rename.Expr.rnExpr
* Desugaring qualified string patterns in GHC.Rename.Pat.rnPatAndThen
* TH quasiquoting in GHC.HsToCore.Quote.repLiteral
* Overlapping patterns checker in GHC.HsToCore.Pmc.Desugar.desugarPat
-}
