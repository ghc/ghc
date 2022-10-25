module Lookup
  (
    keywordToString
  , AnnKeywordId(..)
  , Comment(..)
  ) where

import GHC (AnnKeywordId(..))
import Types

-- | Maps `AnnKeywordId` to the corresponding String representation.
-- There is no specific mapping for the following constructors.
-- `AnnOpen`, `AnnClose`, `AnnVal`, `AnnPackageName`, `AnnHeader`, `AnnFunId`,
-- `AnnInfix`
keywordToString :: AnnKeywordId -> String
keywordToString kw =
  let mkErr x = error $ "keywordToString: missing case for:" ++ show x
  in
  case kw of
      -- Specifically handle all cases so that there are pattern match
      -- warnings if new constructors are added.
      AnnAnyclass     -> "anyclass"
      AnnOpen         -> mkErr kw
      AnnClose        -> mkErr kw
      AnnVal          -> mkErr kw
      AnnPackageName  -> mkErr kw
      AnnHeader       -> mkErr kw
      AnnFunId        -> mkErr kw
      AnnInfix        -> mkErr kw
      AnnValStr       -> mkErr kw
      AnnName         -> mkErr kw
      AnnAs           -> "as"
      AnnBang         -> "!"
      AnnBackquote    -> "`"
      AnnBy           -> "by"
      AnnCase         -> "case"
      AnnCases        -> "cases"
      AnnClass        -> "class"
      AnnCloseB       -> "|)"
      AnnCloseBU      -> "⦈"
      AnnCloseC       -> "}"
      AnnCloseP       -> ")"
      AnnClosePH      -> "#)"
      AnnCloseQ       -> "|]"
      AnnCloseQU      -> "⟧"
      AnnCloseS       -> "]"
      AnnColon        -> ":"
      AnnComma        -> ","
      AnnCommaTuple   -> ","
      AnnDarrow       -> "=>"
      AnnData         -> "data"
      AnnDcolon       -> "::"
      AnnDefault      -> "default"
      AnnDeriving     -> "deriving"
      AnnDo           -> "do"
      AnnDot          -> "."
      AnnDotdot       -> ".."
      AnnElse         -> "else"
      AnnEqual        -> "="
      AnnExport       -> "export"
      AnnFamily       -> "family"
      AnnForall       -> "forall"
      AnnForeign      -> "foreign"
      AnnGroup        -> "group"
      AnnHiding       -> "hiding"
      AnnIf           -> "if"
      AnnImport       -> "import"
      AnnIn           -> "in"
      AnnInstance     -> "instance"
      AnnLam          -> "\\"
      AnnLarrow       -> "<-"
      AnnLet          -> "let"
      AnnLollyU       -> "⊸"
      AnnMdo          -> "mdo"
      AnnMinus        -> "-"
      AnnModule       -> "module"
      AnnNewtype      -> "newtype"
      AnnOf           -> "of"
      AnnOne          -> "one"
      AnnOpenB        -> "(|"
      AnnOpenBU       ->  "⦇"
      AnnOpenC        -> "{"
      AnnOpenE        -> "[e|"
      AnnOpenEQ       -> "[|"
      AnnOpenEQU      ->  "⟦"
      AnnOpenP        -> "("
      AnnOpenPH       -> "(#"
      AnnOpenS        -> "["
      AnnPattern      -> "pattern"
      AnnPercent      -> "%"
      AnnPercentOne   -> "%1"
      AnnProc         -> "proc"
      AnnQualified    -> "qualified"
      AnnRarrow       -> "->"
      AnnRec          -> "rec"
      AnnRole         -> "role"
      AnnSafe         -> "safe"
      AnnSemi         -> ";"
      AnnSignature    -> "signature"
      AnnStock        -> "stock"
      AnnStatic       -> "static"
      AnnThen         -> "then"
      AnnTilde        -> "~"
      AnnType         -> "type"
      AnnUnit         -> "()"
      AnnUsing        -> "using"
      AnnVbar         -> "|"
      AnnWhere        -> "where"
      Annlarrowtail   -> "-<"
      Annrarrowtail   -> ">-"
      AnnLarrowtail   -> "-<<"
      AnnRarrowtail   -> ">>-"
      AnnSimpleQuote  -> "'"
      AnnThTyQuote    -> "''"
      AnnDollar       -> "$"
      AnnDollarDollar -> "$$"
      AnnDarrowU      -> "⇒"
      AnnDcolonU      -> "∷"
      AnnForallU      -> "∀"
      AnnLarrowU      -> "←"
      AnnLarrowtailU  -> "⤛"
      AnnRarrowU      -> "→"
      AnnRarrowtailU  -> "⤜"
      AnnlarrowtailU  -> "⤙"
      AnnrarrowtailU  -> "⤚"
      AnnVia          -> "via"
