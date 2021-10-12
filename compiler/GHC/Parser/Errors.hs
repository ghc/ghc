module GHC.Parser.Errors
   ( PsWarning(..)
   , TransLayoutReason(..)
   , OperatorWhitespaceSymbol(..)
   , OperatorWhitespaceOccurrence(..)
   , NumUnderscoreReason(..)
   , PsError(..)
   , PsErrorDesc(..)
   , LexErr(..)
   , CmmParserError(..)
   , LexErrKind(..)
   , Hint(..)
   , StarIsType (..)
   )
where

import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Parser.Types
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Hs.Lit
import GHC.Hs.Decls
import GHC.Core.Coercion.Axiom (Role)
import GHC.Utils.Outputable (SDoc)
import GHC.Data.FastString
import GHC.Unit.Module.Name
import Data.List.NonEmpty (NonEmpty)

-- | A warning that might arise during parsing.
data PsWarning

     -- | Warn when tabulations are found
   = PsWarnTab
      { tabFirst :: !SrcSpan -- ^ First occurrence of a tab
      , tabCount :: !Word    -- ^ Number of other occurrences
      }

   {-| PsWarnBidirectionalFormatChars is a warning (controlled by the -Wwarn-bidirectional-format-characters flag)
   that occurs when unicode bi-directional format characters are found within in a file

   The 'PsLoc' contains the exact position in the buffer the character occured, and the
   string contains a description of the character.
   -}
   | PsWarnBidirectionalFormatChars (NonEmpty (PsLoc, Char, String))

   | PsWarnTransitionalLayout !SrcSpan !TransLayoutReason
      -- ^ Transitional layout warnings

   | PsWarnUnrecognisedPragma !SrcSpan
      -- ^ Unrecognised pragma

   | PsWarnHaddockInvalidPos !SrcSpan
      -- ^ Invalid Haddock comment position

   | PsWarnHaddockIgnoreMulti !SrcSpan
      -- ^ Multiple Haddock comment for the same entity

   | PsWarnStarBinder !SrcSpan
      -- ^ Found binding occurrence of "*" while StarIsType is enabled

   | PsWarnStarIsType !SrcSpan
      -- ^ Using "*" for "Type" without StarIsType enabled

   | PsWarnImportPreQualified !SrcSpan
      -- ^ Pre qualified import with 'WarnPrepositiveQualifiedModule' enabled

   | PsWarnOperatorWhitespaceExtConflict !SrcSpan !OperatorWhitespaceSymbol
   | PsWarnOperatorWhitespace !SrcSpan !FastString !OperatorWhitespaceOccurrence

-- | The operator symbol in the 'WarnOperatorWhitespaceExtConflict' warning.
data OperatorWhitespaceSymbol
   = OperatorWhitespaceSymbol_PrefixPercent
   | OperatorWhitespaceSymbol_PrefixDollar
   | OperatorWhitespaceSymbol_PrefixDollarDollar

-- | The operator occurrence type in the 'WarnOperatorWhitespace' warning.
data OperatorWhitespaceOccurrence
   = OperatorWhitespaceOccurrence_Prefix
   | OperatorWhitespaceOccurrence_Suffix
   | OperatorWhitespaceOccurrence_TightInfix

data TransLayoutReason
   = TransLayout_Where -- ^ "`where' clause at the same depth as implicit layout block"
   | TransLayout_Pipe  -- ^ "`|' at the same depth as implicit layout block")

data PsError = PsError
   { errDesc  :: !PsErrorDesc   -- ^ Error description
   , errHints :: ![Hint]      -- ^ Hints
   , errLoc   :: !SrcSpan     -- ^ Error position
   }

data PsErrorDesc
   = PsErrLambdaCase
      -- ^ LambdaCase syntax used without the extension enabled

   | PsErrNumUnderscores !NumUnderscoreReason
      -- ^ Underscores in literals without the extension enabled

   | PsErrPrimStringInvalidChar
      -- ^ Invalid character in primitive string

   | PsErrMissingBlock
      -- ^ Missing block

   | PsErrLexer !LexErr !LexErrKind
      -- ^ Lexer error

   | PsErrSuffixAT
      -- ^ Suffix occurrence of `@`

   | PsErrParse !String
      -- ^ Parse errors

   | PsErrCmmLexer
      -- ^ Cmm lexer error

   | PsErrUnsupportedBoxedSumExpr !(SumOrTuple (HsExpr GhcPs))
      -- ^ Unsupported boxed sum in expression

   | PsErrUnsupportedBoxedSumPat !(SumOrTuple (PatBuilder GhcPs))
      -- ^ Unsupported boxed sum in pattern

   | PsErrUnexpectedQualifiedConstructor !RdrName
      -- ^ Unexpected qualified constructor

   | PsErrTupleSectionInPat
      -- ^ Tuple section in pattern context

   | PsErrIllegalBangPattern !(Pat GhcPs)
      -- ^ Bang-pattern without BangPattterns enabled

   | PsErrOpFewArgs !StarIsType !RdrName
      -- ^ Operator applied to too few arguments

   | PsErrImportQualifiedTwice
      -- ^ Import: multiple occurrences of 'qualified'

   | PsErrImportPostQualified
      -- ^ Post qualified import without 'ImportQualifiedPost'

   | PsErrIllegalExplicitNamespace
      -- ^ Explicit namespace keyword without 'ExplicitNamespaces'

   | PsErrVarForTyCon !RdrName
      -- ^ Expecting a type constructor but found a variable

   | PsErrIllegalPatSynExport
      -- ^ Illegal export form allowed by PatternSynonyms

   | PsErrMalformedEntityString
      -- ^ Malformed entity string

   | PsErrDotsInRecordUpdate
      -- ^ Dots used in record update

   | PsErrPrecedenceOutOfRange !Int
      -- ^ Precedence out of range

   | PsErrOverloadedRecordDotInvalid
      -- ^ Invalid use of record dot syntax `.'

   | PsErrOverloadedRecordUpdateNotEnabled
      -- ^ `OverloadedRecordUpdate` is not enabled.

   | PsErrOverloadedRecordUpdateNoQualifiedFields
      -- ^ Can't use qualified fields when OverloadedRecordUpdate is enabled.

   | PsErrInvalidDataCon !(HsType GhcPs)
      -- ^ Cannot parse data constructor in a data/newtype declaration

   | PsErrInvalidInfixDataCon !(HsType GhcPs) !RdrName !(HsType GhcPs)
      -- ^ Cannot parse data constructor in a data/newtype declaration

   | PsErrUnpackDataCon
      -- ^ UNPACK applied to a data constructor

   | PsErrUnexpectedKindAppInDataCon !DataConBuilder !(HsType GhcPs)
      -- ^ Unexpected kind application in data/newtype declaration

   | PsErrInvalidRecordCon !(PatBuilder GhcPs)
      -- ^ Not a record constructor

   | PsErrIllegalUnboxedStringInPat !(HsLit GhcPs)
      -- ^ Illegal unboxed string literal in pattern

   | PsErrDoNotationInPat
      -- ^ Do-notation in pattern

   | PsErrIfTheElseInPat
      -- ^ If-then-else syntax in pattern

   | PsErrLambdaCaseInPat
      -- ^ Lambda-case in pattern

   | PsErrCaseInPat
      -- ^ case..of in pattern

   | PsErrLetInPat
      -- ^ let-syntax in pattern

   | PsErrLambdaInPat
      -- ^ Lambda-syntax in pattern

   | PsErrArrowExprInPat !(HsExpr GhcPs)
      -- ^ Arrow expression-syntax in pattern

   | PsErrArrowCmdInPat !(HsCmd GhcPs)
      -- ^ Arrow command-syntax in pattern

   | PsErrArrowCmdInExpr !(HsCmd GhcPs)
      -- ^ Arrow command-syntax in expression

   | PsErrViewPatInExpr !(LHsExpr GhcPs) !(LHsExpr GhcPs)
      -- ^ View-pattern in expression

   | PsErrTypeAppWithoutSpace !RdrName !(LHsExpr GhcPs)
      -- ^ Type-application without space before '@'

   | PsErrLazyPatWithoutSpace !(LHsExpr GhcPs)
      -- ^ Lazy-pattern ('~') without space after it

   | PsErrBangPatWithoutSpace !(LHsExpr GhcPs)
      -- ^ Bang-pattern ('!') without space after it

   | PsErrUnallowedPragma !(HsPragE GhcPs)
      -- ^ Pragma not allowed in this position

   | PsErrQualifiedDoInCmd !ModuleName
      -- ^ Qualified do block in command

   | PsErrInvalidInfixHole
      -- ^ Invalid infix hole, expected an infix operator

   | PsErrSemiColonsInCondExpr
      -- ^ Unexpected semi-colons in conditional expression
         !(HsExpr GhcPs) -- ^ conditional expr
         !Bool           -- ^ "then" semi-colon?
         !(HsExpr GhcPs) -- ^ "then" expr
         !Bool           -- ^ "else" semi-colon?
         !(HsExpr GhcPs) -- ^ "else" expr

   | PsErrSemiColonsInCondCmd
      -- ^ Unexpected semi-colons in conditional command
         !(HsExpr GhcPs) -- ^ conditional expr
         !Bool           -- ^ "then" semi-colon?
         !(HsCmd GhcPs)  -- ^ "then" expr
         !Bool           -- ^ "else" semi-colon?
         !(HsCmd GhcPs)  -- ^ "else" expr

   | PsErrAtInPatPos
      -- ^ @-operator in a pattern position

   | PsErrLambdaCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected lambda command in function application

   | PsErrCaseCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected case command in function application

   | PsErrIfCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected if command in function application

   | PsErrLetCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected let command in function application

   | PsErrDoCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected do command in function application

   | PsErrDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)
      -- ^ Unexpected do block in function application

   | PsErrMDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)
      -- ^ Unexpected mdo block in function application

   | PsErrLambdaInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected lambda expression in function application

   | PsErrCaseInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected case expression in function application

   | PsErrLambdaCaseInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected lambda-case expression in function application

   | PsErrLetInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected let expression in function application

   | PsErrIfInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected if expression in function application

   | PsErrProcInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected proc expression in function application

   | PsErrMalformedTyOrClDecl !(LHsType GhcPs)
      -- ^ Malformed head of type or class declaration

   | PsErrIllegalWhereInDataDecl
      -- ^ Illegal 'where' keyword in data declaration

   | PsErrIllegalDataTypeContext !(LHsContext GhcPs)
      -- ^ Illegal datatyp context

   | PsErrParseErrorOnInput !OccName
      -- ^ Parse error on input

   | PsErrMalformedDecl !SDoc !RdrName
      -- ^ Malformed ... declaration for ...

   | PsErrUnexpectedTypeAppInDecl !(LHsType GhcPs) !SDoc !RdrName
      -- ^ Unexpected type application in a declaration

   | PsErrNotADataCon !RdrName
      -- ^ Not a data constructor

   | PsErrRecordSyntaxInPatSynDecl !(LPat GhcPs)
      -- ^ Record syntax used in pattern synonym declaration

   | PsErrEmptyWhereInPatSynDecl !RdrName
      -- ^ Empty 'where' clause in pattern-synonym declaration

   | PsErrInvalidWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)
      -- ^ Invalid binding name in 'where' clause of pattern-synonym declaration

   | PsErrNoSingleWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)
      -- ^ Multiple bindings in 'where' clause of pattern-synonym declaration

   | PsErrDeclSpliceNotAtTopLevel !(SpliceDecl GhcPs)
      -- ^ Declaration splice not a top-level

   | PsErrInferredTypeVarNotAllowed
      -- ^ Inferred type variables not allowed here

   | PsErrMultipleNamesInStandaloneKindSignature [LIdP GhcPs]
      -- ^ Multiple names in standalone kind signatures

   | PsErrIllegalImportBundleForm
      -- ^ Illegal import bundle form

   | PsErrIllegalRoleName !FastString [Role]
      -- ^ Illegal role name

   | PsErrInvalidTypeSignature !(LHsExpr GhcPs)
      -- ^ Invalid type signature

   | PsErrUnexpectedTypeInDecl !(LHsType GhcPs) !SDoc !RdrName [LHsTypeArg GhcPs] !SDoc
      -- ^ Unexpected type in declaration

   | PsErrExpectedHyphen
      -- ^ Expected a hyphen

   | PsErrSpaceInSCC
      -- ^ Found a space in a SCC

   | PsErrEmptyDoubleQuotes !Bool-- Is TH on?
      -- ^ Found two single quotes

   | PsErrInvalidPackageName !FastString
      -- ^ Invalid package name

   | PsErrInvalidRuleActivationMarker
      -- ^ Invalid rule activation marker

   | PsErrLinearFunction
      -- ^ Linear function found but LinearTypes not enabled

   | PsErrMultiWayIf
      -- ^ Multi-way if-expression found but MultiWayIf not enabled

   | PsErrExplicitForall !Bool -- is Unicode forall?
      -- ^ Explicit forall found but no extension allowing it is enabled

   | PsErrIllegalQualifiedDo !SDoc
      -- ^ Found qualified-do without QualifiedDo enabled

   | PsErrCmmParser !CmmParserError
      -- ^ Cmm parser error

   | PsErrIllegalTraditionalRecordSyntax !SDoc
      -- ^ Illegal traditional record syntax
      --
      -- TODO: distinguish errors without using SDoc

   | PsErrParseErrorInCmd !SDoc
      -- ^ Parse error in command
      --
      -- TODO: distinguish errors without using SDoc

   | PsErrParseErrorInPat !SDoc
      -- ^ Parse error in pattern
      --
      -- TODO: distinguish errors without using SDoc


newtype StarIsType = StarIsType Bool

data NumUnderscoreReason
   = NumUnderscore_Integral
   | NumUnderscore_Float
   deriving (Show,Eq,Ord)

data Hint
   = SuggestTH
   | SuggestRecursiveDo
   | SuggestDo
   | SuggestMissingDo
   | SuggestLetInDo
   | SuggestPatternSynonyms
   | SuggestInfixBindMaybeAtPat !RdrName
   | TypeApplicationsInPatternsOnlyDataCons -- ^ Type applications in patterns are only allowed on data constructors


data LexErrKind
   = LexErrKind_EOF        -- ^ End of input
   | LexErrKind_UTF8       -- ^ UTF-8 decoding error
   | LexErrKind_Char !Char -- ^ Error at given character
   deriving (Show,Eq,Ord)

data LexErr
   = LexError               -- ^ Lexical error
   | LexUnknownPragma       -- ^ Unknown pragma
   | LexErrorInPragma       -- ^ Lexical error in pragma
   | LexNumEscapeRange      -- ^ Numeric escape sequence out of range
   | LexStringCharLit       -- ^ Llexical error in string/character literal
   | LexStringCharLitEOF    -- ^ Unexpected end-of-file in string/character literal
   | LexUnterminatedComment -- ^ Unterminated `{-'
   | LexUnterminatedOptions -- ^ Unterminated OPTIONS pragma
   | LexUnterminatedQQ      -- ^ Unterminated quasiquotation

-- | Errors from the Cmm parser
data CmmParserError
   = CmmUnknownPrimitive    !FastString -- ^ Unknown Cmm primitive
   | CmmUnknownMacro        !FastString -- ^ Unknown macro
   | CmmUnknownCConv        !String     -- ^ Unknown calling convention
   | CmmUnrecognisedSafety  !String     -- ^ Unrecognised safety
   | CmmUnrecognisedHint    !String     -- ^ Unrecognised hint
