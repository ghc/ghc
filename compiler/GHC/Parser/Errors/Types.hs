
module GHC.Parser.Errors.Types where

import GHC.Prelude

import GHC.Core.TyCon (Role)
import GHC.Data.FastString
import GHC.Hs
import GHC.Parser.Types
import GHC.Types.Error
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Name.Reader
import GHC.Unit.Module.Name
import GHC.LanguageExtensions (Extension)

data PsLayoutMessage
  =
    {-| PsTabulationsFound occurs when tabs are found in a file. The first argument is the
        number of other occurrences apart from the first one found.

       Example: -

       Test cases: -
    -}
    PsTabulationsFound !PsHint !Word -- Number of other occurrences

      -- | Transitional layout warnings
  | PsTransitionalLayout !TransLayoutReason

  | PsOperatorWhitespaceExtConflict !OperatorWhitespaceSymbol

  | PsOperatorWhitespace !FastString !OperatorWhitespaceOccurrence

     -- | Type-application without space before '@'
  | PsTypeAppWithoutSpace !RdrName !(LHsExpr GhcPs)

     -- | Lazy-pattern ('~') without space after it
  | PsLazyPatWithoutSpace !(LHsExpr GhcPs)

     -- | Bang-pattern ('!') without space after it
  | PsBangPatWithoutSpace !(LHsExpr GhcPs)

     -- | Invalid infix hole, expected an infix operator
  | PsInvalidInfixHole

     -- | Expected a hyphen
  | PsExpectedHyphen

     -- | Found a space in a SCC
  | PsSpaceInSCC

     -- | Found two single quotes
  | PsEmptyDoubleQuotes !Bool-- Is TH on?


data PsHaddockMessage
  = PsHaddockInvalidPosition
    -- ^ Invalid Haddock comment position

  | PsHaddockIgnoreMultipleComments
    -- ^ Multiple Haddock comment for the same entity


-- | Diagnostic messages regarding the use (or misuse) of language
-- extensions.
data PsExtensionMessage
  =
    -- | The syntax for an extension was used without the extension enabled
    PsSyntaxUsedButNotEnabled !Extension !PsHint

    -- | Found binding occurrence of "*" while StarIsType is enabled
  | PsStarBinderWithStarIsType

     -- | Using "*" for "Type" without StarIsType enabled
  | PsStarIsTypeWithoutExtension

     -- | Underscores in literals without the extension enabled
  | PsNumUnderscoresWithoutExtension !NumUnderscoreReason

     -- | Bang-pattern without BangPattterns enabled
  | PsIllegalBangPattern !(Pat GhcPs)

     -- | Invalid use of record dot syntax `.'
  | PsOverloadedRecordDotInvalid

     -- | Illegal export form allowed by PatternSynonyms
  | PsIllegalPatSynExport

     -- | Can't use qualified fields when OverloadedRecordUpdate is enabled.
  | PsOverloadedRecordUpdateNoQualifiedFields

     -- | Explicit forall found but no extension allowing it is enabled
  | PsExplicitForallWithoutExtension !Bool -- is Unicode forall?

     -- | Found qualified-do without QualifiedDo enabled
  | PsIllegalQualifiedDoWithoutExtension !SDoc

     -- | Qualified do block in command
  | PsQualifiedDoInCmd !ModuleName

     -- | Record syntax used in pattern synonym declaration
  | PsRecordSyntaxInPatSynDecl !(LPat GhcPs)

     -- | Empty 'where' clause in pattern-synonym declaration
  | PsEmptyWhereInPatSynDecl !RdrName

     -- | Invalid binding name in 'where' clause of pattern-synonym declaration
  | PsInvalidWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)

     -- | Multiple bindings in 'where' clause of pattern-synonym declaration
  | PsNoSingleWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)

     -- | Declaration splice not a top-level
  | PsDeclSpliceNotAtTopLevel !(SpliceDecl GhcPs)

  | PsMultipleNamesInStandaloneKindSignature [LIdP GhcPs]
     -- ^ Multiple names in standalone kind signatures

     -- ^ Explicit namespace keyword without 'ExplicitNamespaces'
  | PsIllegalExplicitNamespace


data PsMalformedExprMessage
  = -- | Missing block
    PsMissingBlock

     -- | Unsupported boxed sum in expression
  | PsUnsupportedBoxedSumExpr !(SumOrTuple (HsExpr GhcPs))

     -- | Unsupported boxed sum in pattern
  | PsUnsupportedBoxedSumPattern !(SumOrTuple (PatBuilder GhcPs))

     -- | Unexpected qualified constructor
  | PsUnexpectedQualifiedConstructor !RdrName

     -- | Tuple section in pattern context
  | PsTupleSectionInPattern

     -- | Operator applied to too few arguments
  | PsOpTooFewArgs !StarIsType !RdrName

     -- | Expecting a type constructor but found a variable
  | PsVarForTyCon !RdrName

     -- | Malformed entity string
  | PsMalformedEntityString

     -- | Dots used in record update
  | PsDotsInRecordUpdate

     -- | Cannot parse data constructor in a data/newtype declaration
  | PsInvalidDataCon !(HsType GhcPs)

     -- | Cannot parse data constructor in a data/newtype declaration
  | PsInvalidInfixDataCon !(HsType GhcPs) !RdrName !(HsType GhcPs)

     -- | UNPACK applied to a data constructor
  | PsUnpackDataCon

     -- | Unexpected kind application in data/newtype declaration
  | PsUnexpectedKindAppInDataCon !DataConBuilder !(HsType GhcPs)

     -- | Not a record constructor
  | PsInvalidRecordCon !(PatBuilder GhcPs)

     -- | Illegal unboxed string literal in pattern
  | PsIllegalUnboxedStringInPat !(HsLit GhcPs)

     -- | Do-notation in pattern
  | PsDoNotationInPat

     -- | If-then-else syntax in pattern
  | PsIfThenElseInPat

     -- | Lambda-case in pattern
  | PsLambdaCaseInPat

     -- | case..of in pattern
  | PsCaseInPat

     -- | let-syntax in pattern
  | PsLetInPat

     -- | Lambda-syntax in pattern
  | PsLambdaInPat

     -- | Arrow expression-syntax in pattern
  | PsArrowExprInPat !(HsExpr GhcPs)

     -- | Arrow command-syntax in pattern
  | PsArrowCmdInPat !(HsCmd GhcPs)

     -- | Arrow command-syntax in expression
  | PsArrowCmdInExpr !(HsCmd GhcPs)

     -- | View-pattern in expression
  | PsViewPatInExpr !(LHsExpr GhcPs) !(LHsExpr GhcPs)

     -- | Unexpected lambda command in function application
  | PsLambdaCmdInFunAppCmd !(LHsCmd GhcPs)

     -- | Unexpected case command in function application
  | PsCaseCmdInFunAppCmd !(LHsCmd GhcPs)

     -- | Unexpected if command in function application
  | PsIfCmdInFunAppCmd !(LHsCmd GhcPs)

     -- | Unexpected let command in function application
  | PsLetCmdInFunAppCmd !(LHsCmd GhcPs)

     -- | Unexpected do command in function application
  | PsDoCmdInFunAppCmd !(LHsCmd GhcPs)

     -- | Unexpected do block in function application
  | PsDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)

     -- | Unexpected mdo block in function application
  | PsMDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)

     -- | Unexpected lambda expression in function application
  | PsLambdaInFunAppExpr !(LHsExpr GhcPs)

     -- | Unexpected case expression in function application
  | PsCaseInFunAppExpr !(LHsExpr GhcPs)

     -- | Unexpected lambda-case expression in function application
  | PsLambdaCaseInFunAppExpr !(LHsExpr GhcPs)

     -- | Unexpected let expression in function application
  | PsLetInFunAppExpr !(LHsExpr GhcPs)

     -- | Unexpected if expression in function application
  | PsIfInFunAppExpr !(LHsExpr GhcPs)

     -- | Unexpected proc expression in function application
  | PsProcInFunAppExpr !(LHsExpr GhcPs)

     -- | Malformed head of type or class declaration
  | PsMalformedTyOrClDecl !(LHsType GhcPs)

     -- | Illegal 'where' keyword in data declaration
  | PsIllegalWhereInDataDecl

     -- | Illegal datatyp context
  | PsIllegalDataTypeContext !(LHsContext GhcPs)

     -- | Invalid character in primitive string
  | PsPrimStringInvalidChar

     -- | Suffix occurrence of `@`
  | PsSuffixAT

     -- | Precedence out of range
  | PsPrecedenceOutOfRange !Int

     -- | Unexpected semi-colons in conditional expression
  | PsSemiColonsInCondExpr
        !(HsExpr GhcPs) -- ^ conditional expr
        !Bool           -- ^ "then" semi-colon?
        !(HsExpr GhcPs) -- ^ "then" expr
        !Bool           -- ^ "else" semi-colon?
        !(HsExpr GhcPs) -- ^ "else" expr

     -- | Unexpected semi-colons in conditional command
  | PsSemiColonsInCondCmd
        !(HsExpr GhcPs) -- ^ conditional expr
        !Bool           -- ^ "then" semi-colon?
        !(HsCmd GhcPs)  -- ^ "then" expr
        !Bool           -- ^ "else" semi-colon?
        !(HsCmd GhcPs)  -- ^ "else" expr

     -- | @-operator in a pattern position
  | PsAtInPatternPosition

     -- | Parse error on input
  | PsParseErrorOnInput !OccName

     -- | Malformed ... declaration for ...
  | PsMalformedDecl !SDoc !RdrName

     -- | Unexpected type application in a declaration
  | PsUnexpectedTypeAppInDecl !(LHsType GhcPs) !SDoc !RdrName

     -- | Not a data constructor
  | PsNotADataCon !RdrName

     -- | Inferred type variables not allowed here
  | PsInferredTypeVarNotAllowed

     -- | Illegal traditional record syntax
     --
     -- TODO: distinguish errors without using SDoc
  | PsIllegalTraditionalRecordSyntax !SDoc

     -- | Parse error in command
     --
     -- TODO: distinguish errors without using SDoc
  | PsParseErrorInCmd !SDoc

     -- | Parse error in pattern
     --
     -- TODO: distinguish errors without using SDoc
  | PsParseErrorInPat !SDoc

     -- | Illegal role name
  | PsIllegalRoleName !FastString [Role]

     -- | Invalid type signature
  | PsInvalidTypeSignature !(LHsExpr GhcPs)

     -- | Unexpected type in declaration
  | PsUnexpectedTypeInDecl !(LHsType GhcPs) !SDoc !RdrName [LHsTypeArg GhcPs] !SDoc

     -- | Invalid package name
  | PsInvalidPackageName !FastString


-- | Messages regarding the import section or the use of pragmas.
data PsImportOrPragmaMessage
  =
     -- | Unrecognised pragma
    PsUnrecognisedPragma

     -- | Pragma not allowed in this position
  | PsUnallowedPragma !(HsPragE GhcPs)

     -- | Pre qualified import with 'WarnPrepositiveQualifiedModule' enabled
  | PsImportPreQualified

     -- | Post qualified import without 'ImportQualifiedPost'
  | PsImportPostQualified

     -- | Import: multiple occurrences of 'qualified'
  | PsImportQualifiedTwice

     -- | Illegal import bundle form
  | PsIllegalImportBundleForm

     -- | Invalid rule activation marker
  | PsInvalidRuleActivationMarker

-- | A diagnostic message emitted during parsing.
data PsMessage
  =
    -- | Simply rewraps a generic 'DiagnosticMessage'. Useful
    -- to embed messages where the provenance is unknown, for
    -- example in case of a plugin author willing to add a
    -- custom parser message.
    PsUnknownMessage [PsHint] !DiagnosticMessage

    -- | Messages regarding layout parsing and formatting
  | PsLayoutMessage !PsLayoutMessage

    -- | Messages regarding Haddock
  | PsHaddockMessage !PsHaddockMessage

    -- | Messages regarding the use (or misuse) of language extensions
  | PsExtensionMessage !PsExtensionMessage

    -- | Messages regarding module imports and pragmas
  | PsImportOrPragmaMessage !PsImportOrPragmaMessage

     -- | Lexer errors
  | PsLexerError !LexErr !LexErrKind

     -- | Parse errors
  | PsParseError !String [PsHint]

     -- | Cmm lexer error
  | PsCmmLexerError

     -- | Cmm parser error
  | PsCmmParserError !CmmParserError

     -- | Messages emitted in case of malformed expressions
  | PsMalformedExprMessage !PsMalformedExprMessage


newtype StarIsType = StarIsType Bool

data NumUnderscoreReason
   = NumUnderscore_Integral
   | NumUnderscore_Float
   deriving (Show,Eq,Ord)

data PsHint
   = SuggestExtension !Extension
   | SuggestDo
   | SuggestMissingDo
   | SuggestLetInDo
   | SuggestInfixBindMaybeAtPat !RdrName
   | TypeApplicationsInPatternsOnlyDataCons -- ^ Type applications in patterns are only allowed on data constructors
   | UseSpaces

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

-- | The operator symbol in the 'PsOperatorWhitespaceExtConflictMessage' diagnostic.
data OperatorWhitespaceSymbol
   = OperatorWhitespaceSymbol_PrefixPercent
   | OperatorWhitespaceSymbol_PrefixDollar
   | OperatorWhitespaceSymbol_PrefixDollarDollar

-- | The operator occurrence type in the 'PsOperatorWhitespaceMessage' diagnostic.
data OperatorWhitespaceOccurrence
   = OperatorWhitespaceOccurrence_Prefix
   | OperatorWhitespaceOccurrence_Suffix
   | OperatorWhitespaceOccurrence_TightInfix

data TransLayoutReason
   = TransLayout_Where -- ^ "`where' clause at the same depth as implicit layout block"
   | TransLayout_Pipe  -- ^ "`|' at the same depth as implicit layout block")
