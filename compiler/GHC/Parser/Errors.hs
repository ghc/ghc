module GHC.Parser.Errors
   ( Warning(..)
   , TransLayoutReason(..)
   , OperatorWhitespaceSymbol(..)
   , OperatorWhitespaceOccurrence(..)
   , NumUnderscoreReason(..)
   , Error(..)
   , ErrorDesc(..)
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

data Warning

     -- | Warn when tabulations are found
   = WarnTab
      { tabFirst :: !SrcSpan -- ^ First occurence of a tab
      , tabCount :: !Word    -- ^ Number of other occurences
      }

   | WarnTransitionalLayout !SrcSpan !TransLayoutReason
      -- ^ Transitional layout warnings

   | WarnUnrecognisedPragma !SrcSpan
      -- ^ Unrecognised pragma

   | WarnHaddockInvalidPos !SrcSpan
      -- ^ Invalid Haddock comment position

   | WarnHaddockIgnoreMulti !SrcSpan
      -- ^ Multiple Haddock comment for the same entity

   | WarnStarBinder !SrcSpan
      -- ^ Found binding occurence of "*" while StarIsType is enabled

   | WarnStarIsType !SrcSpan
      -- ^ Using "*" for "Type" without StarIsType enabled

   | WarnImportPreQualified !SrcSpan
      -- ^ Pre qualified import with 'WarnPrepositiveQualifiedModule' enabled

   | WarnOperatorWhitespaceExtConflict !SrcSpan !OperatorWhitespaceSymbol
   | WarnOperatorWhitespace !SrcSpan !FastString !OperatorWhitespaceOccurrence

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

data Error = Error
   { errDesc  :: !ErrorDesc   -- ^ Error description
   , errHints :: ![Hint]      -- ^ Hints
   , errLoc   :: !SrcSpan     -- ^ Error position
   }

data ErrorDesc
   = ErrLambdaCase
      -- ^ LambdaCase syntax used without the extension enabled

   | ErrNumUnderscores !NumUnderscoreReason
      -- ^ Underscores in literals without the extension enabled

   | ErrPrimStringInvalidChar
      -- ^ Invalid character in primitive string

   | ErrMissingBlock
      -- ^ Missing block

   | ErrLexer !LexErr !LexErrKind
      -- ^ Lexer error

   | ErrSuffixAT
      -- ^ Suffix occurence of `@`

   | ErrParse !String
      -- ^ Parse errors

   | ErrCmmLexer
      -- ^ Cmm lexer error

   | ErrUnsupportedBoxedSumExpr !(SumOrTuple (HsExpr GhcPs))
      -- ^ Unsupported boxed sum in expression

   | ErrUnsupportedBoxedSumPat !(SumOrTuple (PatBuilder GhcPs))
      -- ^ Unsupported boxed sum in pattern

   | ErrUnexpectedQualifiedConstructor !RdrName
      -- ^ Unexpected qualified constructor

   | ErrTupleSectionInPat
      -- ^ Tuple section in pattern context

   | ErrIllegalBangPattern !(Pat GhcPs)
      -- ^ Bang-pattern without BangPattterns enabled

   | ErrOpFewArgs !StarIsType !RdrName
      -- ^ Operator applied to too few arguments

   | ErrImportQualifiedTwice
      -- ^ Import: multiple occurrences of 'qualified'

   | ErrImportPostQualified
      -- ^ Post qualified import without 'ImportQualifiedPost'

   | ErrIllegalExplicitNamespace
      -- ^ Explicit namespace keyword without 'ExplicitNamespaces'

   | ErrVarForTyCon !RdrName
      -- ^ Expecting a type constructor but found a variable

   | ErrIllegalPatSynExport
      -- ^ Illegal export form allowed by PatternSynonyms

   | ErrMalformedEntityString
      -- ^ Malformed entity string

   | ErrDotsInRecordUpdate
      -- ^ Dots used in record update

   | ErrPrecedenceOutOfRange !Int
      -- ^ Precedence out of range

   | ErrInvalidDataCon !(HsType GhcPs)
      -- ^ Cannot parse data constructor in a data/newtype declaration

   | ErrInvalidInfixDataCon !(HsType GhcPs) !RdrName !(HsType GhcPs)
      -- ^ Cannot parse data constructor in a data/newtype declaration

   | ErrUnpackDataCon
      -- ^ UNPACK applied to a data constructor

   | ErrUnexpectedKindAppInDataCon !DataConBuilder !(HsType GhcPs)
      -- ^ Unexpected kind application in data/newtype declaration

   | ErrInvalidRecordCon !(PatBuilder GhcPs)
      -- ^ Not a record constructor

   | ErrIllegalUnboxedStringInPat !(HsLit GhcPs)
      -- ^ Illegal unboxed string literal in pattern

   | ErrDoNotationInPat
      -- ^ Do-notation in pattern

   | ErrIfTheElseInPat
      -- ^ If-then-else syntax in pattern

   | ErrTypeAppInPat
      -- ^ Type-application in pattern

   | ErrLambdaCaseInPat
      -- ^ Lambda-case in pattern

   | ErrCaseInPat
      -- ^ case..of in pattern

   | ErrLetInPat
      -- ^ let-syntax in pattern

   | ErrLambdaInPat
      -- ^ Lambda-syntax in pattern

   | ErrArrowExprInPat !(HsExpr GhcPs)
      -- ^ Arrow expression-syntax in pattern

   | ErrArrowCmdInPat !(HsCmd GhcPs)
      -- ^ Arrow command-syntax in pattern

   | ErrArrowCmdInExpr !(HsCmd GhcPs)
      -- ^ Arrow command-syntax in expression

   | ErrViewPatInExpr !(LHsExpr GhcPs) !(LHsExpr GhcPs)
      -- ^ View-pattern in expression

   | ErrTypeAppWithoutSpace !RdrName !(LHsExpr GhcPs)
      -- ^ Type-application without space before '@'

   | ErrLazyPatWithoutSpace !(LHsExpr GhcPs)
      -- ^ Lazy-pattern ('~') without space after it

   | ErrBangPatWithoutSpace !(LHsExpr GhcPs)
      -- ^ Bang-pattern ('!') without space after it

   | ErrUnallowedPragma !(HsPragE GhcPs)
      -- ^ Pragma not allowed in this position

   | ErrQualifiedDoInCmd !ModuleName
      -- ^ Qualified do block in command

   | ErrInvalidInfixHole
      -- ^ Invalid infix hole, expected an infix operator

   | ErrSemiColonsInCondExpr
      -- ^ Unexpected semi-colons in conditional expression
         !(HsExpr GhcPs) -- ^ conditional expr
         !Bool           -- ^ "then" semi-colon?
         !(HsExpr GhcPs) -- ^ "then" expr
         !Bool           -- ^ "else" semi-colon?
         !(HsExpr GhcPs) -- ^ "else" expr

   | ErrSemiColonsInCondCmd
      -- ^ Unexpected semi-colons in conditional command
         !(HsExpr GhcPs) -- ^ conditional expr
         !Bool           -- ^ "then" semi-colon?
         !(HsCmd GhcPs)  -- ^ "then" expr
         !Bool           -- ^ "else" semi-colon?
         !(HsCmd GhcPs)  -- ^ "else" expr

   | ErrAtInPatPos
      -- ^ @-operator in a pattern position

   | ErrLambdaCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected lambda command in function application

   | ErrCaseCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected case command in function application

   | ErrIfCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected if command in function application

   | ErrLetCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected let command in function application

   | ErrDoCmdInFunAppCmd !(LHsCmd GhcPs)
      -- ^ Unexpected do command in function application

   | ErrDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)
      -- ^ Unexpected do block in function application

   | ErrMDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)
      -- ^ Unexpected mdo block in function application

   | ErrLambdaInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected lambda expression in function application

   | ErrCaseInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected case expression in function application

   | ErrLambdaCaseInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected lambda-case expression in function application

   | ErrLetInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected let expression in function application

   | ErrIfInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected if expression in function application

   | ErrProcInFunAppExpr !(LHsExpr GhcPs)
      -- ^ Unexpected proc expression in function application

   | ErrMalformedTyOrClDecl !(LHsType GhcPs)
      -- ^ Malformed head of type or class declaration

   | ErrIllegalWhereInDataDecl
      -- ^ Illegal 'where' keyword in data declaration

   | ErrIllegalDataTypeContext !(LHsContext GhcPs)
      -- ^ Illegal datatyp context

   | ErrParseErrorOnInput !OccName
      -- ^ Parse error on input

   | ErrMalformedDecl !SDoc !RdrName
      -- ^ Malformed ... declaration for ...

   | ErrUnexpectedTypeAppInDecl !(LHsType GhcPs) !SDoc !RdrName
      -- ^ Unexpected type application in a declaration

   | ErrNotADataCon !RdrName
      -- ^ Not a data constructor

   | ErrRecordSyntaxInPatSynDecl !(LPat GhcPs)
      -- ^ Record syntax used in pattern synonym declaration

   | ErrEmptyWhereInPatSynDecl !RdrName
      -- ^ Empty 'where' clause in pattern-synonym declaration

   | ErrInvalidWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)
      -- ^ Invalid binding name in 'where' clause of pattern-synonym declaration

   | ErrNoSingleWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)
      -- ^ Multiple bindings in 'where' clause of pattern-synonym declaration

   | ErrDeclSpliceNotAtTopLevel !(SpliceDecl GhcPs)
      -- ^ Declaration splice not a top-level

   | ErrInferredTypeVarNotAllowed
      -- ^ Inferred type variables not allowed here

   | ErrMultipleNamesInStandaloneKindSignature [LIdP GhcPs]
      -- ^ Multiple names in standalone kind signatures

   | ErrIllegalImportBundleForm
      -- ^ Illegal import bundle form

   | ErrIllegalRoleName !FastString [Role]
      -- ^ Illegal role name

   | ErrInvalidTypeSignature !(LHsExpr GhcPs)
      -- ^ Invalid type signature

   | ErrUnexpectedTypeInDecl !(LHsType GhcPs) !SDoc !RdrName [LHsTypeArg GhcPs] !SDoc
      -- ^ Unexpected type in declaration

   | ErrExpectedHyphen
      -- ^ Expected a hyphen

   | ErrSpaceInSCC
      -- ^ Found a space in a SCC

   | ErrEmptyDoubleQuotes !Bool-- Is TH on?
      -- ^ Found two single quotes

   | ErrInvalidPackageName !FastString
      -- ^ Invalid package name

   | ErrInvalidRuleActivationMarker
      -- ^ Invalid rule activation marker

   | ErrLinearFunction
      -- ^ Linear function found but LinearTypes not enabled

   | ErrMultiWayIf
      -- ^ Multi-way if-expression found but MultiWayIf not enabled

   | ErrExplicitForall !Bool -- is Unicode forall?
      -- ^ Explicit forall found but no extension allowing it is enabled

   | ErrIllegalQualifiedDo !SDoc
      -- ^ Found qualified-do without QualifiedDo enabled

   | ErrCmmParser !CmmParserError
      -- ^ Cmm parser error

   | ErrIllegalTraditionalRecordSyntax !SDoc
      -- ^ Illegal traditional record syntax
      --
      -- TODO: distinguish errors without using SDoc

   | ErrParseErrorInCmd !SDoc
      -- ^ Parse error in command
      --
      -- TODO: distinguish errors without using SDoc

   | ErrParseErrorInPat !SDoc
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
