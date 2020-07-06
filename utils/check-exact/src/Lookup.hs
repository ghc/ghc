module Lookup
  (
    keywordToString
  , KeywordId(..)
  , Comment(..)
  ) where

-- import Language.Haskell.ExactPrint.Types
import GHC (AnnKeywordId(..))
-- import GHC.Utils.Outputable hiding ( (<>) )
-- import Data.Data (Data)
-- import GHC.Types.SrcLoc
-- import GHC.Driver.Session
import Types

-- | Maps `AnnKeywordId` to the corresponding String representation.
-- There is no specific mapping for the following constructors.
-- `AnnOpen`, `AnnClose`, `AnnVal`, `AnnPackageName`, `AnnHeader`, `AnnFunId`,
-- `AnnInfix`
keywordToString :: KeywordId -> String
keywordToString kw =
  let mkErr x = error $ "keywordToString: missing case for:" ++ show x
  in
  case kw of
      -- Specifically handle all cases so that there are pattern match
      -- warnings if new constructors are added.
      AnnComment _      -> mkErr kw
      AnnString _       -> mkErr kw
      AnnSemiSep        -> ";"
      (G AnnAnyclass) -> "anyclass"
      (G AnnOpen  ) -> mkErr kw
      (G AnnClose ) -> mkErr kw
      (G AnnVal   ) -> mkErr kw
      (G AnnPackageName) -> mkErr kw
      (G AnnHeader ) -> mkErr kw
      (G AnnFunId  ) -> mkErr kw
      (G AnnInfix  ) -> mkErr kw
      (G AnnValStr ) -> mkErr kw
      (G AnnName   ) -> mkErr kw
      (G AnnAs     ) -> "as"
      (G AnnAt     ) -> "@"
      (G AnnBang   ) -> "!"
      (G AnnBackquote ) -> "`"
      (G AnnBy     ) -> "by"
      (G AnnCase   ) -> "case"
      (G AnnClass   ) -> "class"
      (G AnnCloseB  ) -> "|)"
      (G AnnCloseBU ) -> "⦈"
      (G AnnCloseC  ) -> "}"
      (G AnnCloseP  ) -> ")"
      (G AnnClosePH ) -> "#)"
      (G AnnCloseQ  ) -> "|]"
      (G AnnCloseQU ) -> "⟧"
      (G AnnCloseS  ) -> "]"
      (G AnnColon   ) -> ":"
      (G AnnComma   ) -> ","
      (G AnnCommaTuple ) -> ","
      (G AnnDarrow  ) -> "=>"
      (G AnnData    ) -> "data"
      (G AnnDcolon  ) -> "::"
      (G AnnDefault ) -> "default"
      (G AnnDeriving ) -> "deriving"
      (G AnnDo       ) -> "do"
      (G AnnDot      ) -> "."
      (G AnnDotdot   ) -> ".."
      (G AnnElse     ) -> "else"
      (G AnnEqual    ) -> "="
      (G AnnExport   ) -> "export"
      (G AnnFamily   ) -> "family"
      (G AnnForall   ) -> "forall"
      (G AnnForeign  ) -> "foreign"
      (G AnnGroup    ) -> "group"
      (G AnnHiding   ) -> "hiding"
      (G AnnIf       ) -> "if"
      (G AnnImport   ) -> "import"
      (G AnnIn       ) -> "in"
      (G AnnInstance ) -> "instance"
      (G AnnLam      ) -> "\\"
      (G AnnLarrow   ) -> "<-"
      (G AnnLet      ) -> "let"
      -- (G AnnLolly    ) -> "#->"
      (G AnnLollyU   ) -> "⊸"
      (G AnnMdo      ) -> "mdo"
      (G AnnMinus    ) -> "-"
      (G AnnModule   ) -> "module"
      (G AnnNewtype  ) -> "newtype"
      (G AnnOf       ) -> "of"
      (G AnnOpenB    ) -> "(|"
      (G AnnOpenBU   ) ->  "⦇"
      (G AnnOpenC    ) -> "{"
      (G AnnOpenE    ) -> "[e|"
      (G AnnOpenEQ   ) -> "[|"
      (G AnnOpenEQU  ) ->  "⟦"
      (G AnnOpenP    ) -> "("
      (G AnnOpenPH   ) -> "(#"
      -- (G AnnOpenPE   ) -> "$("
      -- (G AnnOpenPTE  ) -> "$$("
      (G AnnOpenS    ) -> "["
      (G AnnPattern  ) -> "pattern"
      (G AnnProc     ) -> "proc"
      (G AnnQualified ) -> "qualified"
      (G AnnRarrow   ) -> "->"
      (G AnnRec      ) -> "rec"
      (G AnnRole     ) -> "role"
      (G AnnSafe     ) -> "safe"
      (G AnnSemi     ) -> ";"
      (G AnnSignature) -> "signature"
      (G AnnStock    ) -> "stock"
      (G AnnStatic   ) -> "static"
      (G AnnThen     ) -> "then"
      (G AnnTilde    ) -> "~"
      (G AnnType     ) -> "type"
      (G AnnUnit     ) -> "()"
      (G AnnUsing    ) -> "using"
      (G AnnVbar     ) -> "|"
      (G AnnWhere    ) -> "where"
      (G Annlarrowtail ) -> "-<"
      (G Annrarrowtail ) -> ">-"
      (G AnnLarrowtail ) -> "-<<"
      (G AnnRarrowtail ) -> ">>-"
      (G AnnSimpleQuote  ) -> "'"
      (G AnnThTyQuote    ) -> "''"
      (G AnnDollar       ) -> "$"
      (G AnnDollarDollar ) -> "$$"
      (G AnnDarrowU) -> "⇒"
      (G AnnDcolonU) -> "∷"
      (G AnnForallU) -> "∀"
      (G AnnLarrowU) -> "←"
      (G AnnLarrowtailU) -> "⤛"
      (G AnnRarrowU) -> "→"
      (G AnnRarrowtailU) -> "⤜"
      (G AnnlarrowtailU) -> "⤙"
      (G AnnrarrowtailU) -> "⤚"
      AnnTypeApp             -> "@"
      (G AnnVia) -> "via"
