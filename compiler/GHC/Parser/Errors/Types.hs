{-# LANGUAGE ExistentialQuantification #-}

module GHC.Parser.Errors.Types where

import GHC.Prelude

import Data.Typeable

import GHC.Core.TyCon (Role)
import GHC.Data.FastString
import GHC.Hs
import GHC.Parser.Types
import GHC.Parser.Errors.Basic
import GHC.Types.Error
import GHC.Types.Hint
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Name.Reader
import GHC.Unit.Module.Name
import GHC.Utils.Outputable
import Data.List.NonEmpty (NonEmpty)
import GHC.Types.SrcLoc (PsLoc)

-- The type aliases below are useful to make some type signatures a bit more
-- descriptive, like 'handleWarningsThrowErrors' in 'GHC.Driver.Main'.

type PsWarning = PsMessage   -- /INVARIANT/: The diagnosticReason is a Warning reason
type PsError   = PsMessage   -- /INVARIANT/: The diagnosticReason is ErrorWithoutFlag

{-
Note [Messages from GHC.Parser.Header
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We group the messages from 'GHC.Parser.Header' because we need to
be able to pattern match on them in the driver code. This is because
in functions like 'GHC.Driver.Pipeline.preprocess' we want to handle
only a specific subset of parser messages, during dependency analysis,
and having a single constructor to handle them all is handy.

-}

data PsHeaderMessage
  = PsErrParseLanguagePragma
  | PsErrUnsupportedExt !String ![String]
  | PsErrParseOptionsPragma !String

  {-| PsErrUnsupportedOptionsPragma is an error that occurs when an unknown
      OPTIONS_GHC pragma is supplied is found.

      Example(s):
        {-# OPTIONS_GHC foo #-}

      Test case(s):

        tests/safeHaskell/flags/SafeFlags28
        tests/safeHaskell/flags/SafeFlags19
        tests/safeHaskell/flags/SafeFlags29
        tests/parser/should_fail/T19923c
        tests/parser/should_fail/T19923b
        tests/parser/should_fail/readFail044
        tests/driver/T2499
  -}
  | PsErrUnknownOptionsPragma !String


data PsMessage
  =
    {-| An \"unknown\" message from the parser. This type constructor allows
        arbitrary messages to be embedded. The typical use case would be GHC plugins
        willing to emit custom diagnostics.
    -}
   forall a. (Diagnostic a, Typeable a) => PsUnknownMessage a

    {-| A group of parser messages emitted in 'GHC.Parser.Header'.
        See Note [Messages from GHC.Parser.Header].
    -}
   | PsHeaderMessage !PsHeaderMessage

   {-| PsWarnBidirectionalFormatChars is a warning (controlled by the -Wwarn-bidirectional-format-characters flag)
   that occurs when unicode bi-directional format characters are found within in a file

   The 'PsLoc' contains the exact position in the buffer the character occured, and the
   string contains a description of the character.
   -}
   | PsWarnBidirectionalFormatChars (NonEmpty (PsLoc, Char, String))

   {-| PsWarnTab is a warning (controlled by the -Wwarn-tabs flag) that occurs
       when tabulations (tabs) are found within a file.

       Test case(s): parser/should_fail/T12610
                     parser/should_compile/T9723b
                     parser/should_compile/T9723a
                     parser/should_compile/read043
                     parser/should_fail/T16270
                     warnings/should_compile/T9230

   -}
   | PsWarnTab !Word -- ^ Number of other occurrences other than the first one

   {-| PsWarnTransitionalLayout is a warning (controlled by the
       -Walternative-layout-rule-transitional flag) that occurs when pipes ('|')
       or 'where' are at the same depth of an implicit layout block.

       Example(s):

          f :: IO ()
          f
           | True = do
           let x = ()
               y = ()
           return ()
           | True = return ()

       Test case(s): layout/layout006
                     layout/layout003
                     layout/layout001

   -}
   | PsWarnTransitionalLayout !TransLayoutReason

   -- | Unrecognised pragma
   | PsWarnUnrecognisedPragma
   | PsWarnMisplacedPragma !FileHeaderPragmaType

   -- | Invalid Haddock comment position
   | PsWarnHaddockInvalidPos

   -- | Multiple Haddock comment for the same entity
   | PsWarnHaddockIgnoreMulti

   -- | Found binding occurrence of "*" while StarIsType is enabled
   | PsWarnStarBinder

   -- | Using "*" for "Type" without StarIsType enabled
   | PsWarnStarIsType

   -- | Pre qualified import with 'WarnPrepositiveQualifiedModule' enabled
   | PsWarnImportPreQualified

   | PsWarnOperatorWhitespaceExtConflict !OperatorWhitespaceSymbol

   | PsWarnOperatorWhitespace !FastString !OperatorWhitespaceOccurrence

   -- | LambdaCase syntax used without the extension enabled
   | PsErrLambdaCase

   -- | A lambda requires at least one parameter
   | PsErrEmptyLambda

   -- | Underscores in literals without the extension enabled
   | PsErrNumUnderscores !NumUnderscoreReason

   -- | Invalid character in primitive string
   | PsErrPrimStringInvalidChar

   -- | Missing block
   | PsErrMissingBlock

   -- | Lexer error
   | PsErrLexer !LexErr !LexErrKind

   -- | Suffix occurrence of `@`
   | PsErrSuffixAT

   -- | Parse errors
   | PsErrParse !String !PsErrParseDetails

   -- | Cmm lexer error
   | PsErrCmmLexer

   -- | Unsupported boxed sum in expression
   | PsErrUnsupportedBoxedSumExpr !(SumOrTuple (HsExpr GhcPs))

   -- | Unsupported boxed sum in pattern
   | PsErrUnsupportedBoxedSumPat !(SumOrTuple (PatBuilder GhcPs))

   -- | Unexpected qualified constructor
   | PsErrUnexpectedQualifiedConstructor !RdrName

   -- | Tuple section in pattern context
   | PsErrTupleSectionInPat

   -- | Bang-pattern without BangPattterns enabled
   | PsErrIllegalBangPattern !(Pat GhcPs)

   -- | Operator applied to too few arguments
   | PsErrOpFewArgs !StarIsType !RdrName

   -- | Import: multiple occurrences of 'qualified'
   | PsErrImportQualifiedTwice

   -- | Post qualified import without 'ImportQualifiedPost'
   | PsErrImportPostQualified

   -- | Explicit namespace keyword without 'ExplicitNamespaces'
   | PsErrIllegalExplicitNamespace

   -- | Expecting a type constructor but found a variable
   | PsErrVarForTyCon !RdrName

   -- | Illegal export form allowed by PatternSynonyms
   | PsErrIllegalPatSynExport

   -- | Malformed entity string
   | PsErrMalformedEntityString

   -- | Dots used in record update
   | PsErrDotsInRecordUpdate

   -- | Precedence out of range
   | PsErrPrecedenceOutOfRange !Int

   -- | Invalid use of record dot syntax `.'
   | PsErrOverloadedRecordDotInvalid

   -- | `OverloadedRecordUpdate` is not enabled.
   | PsErrOverloadedRecordUpdateNotEnabled

   -- | Can't use qualified fields when OverloadedRecordUpdate is enabled.
   | PsErrOverloadedRecordUpdateNoQualifiedFields

   -- | Cannot parse data constructor in a data/newtype declaration
   | PsErrInvalidDataCon !(HsType GhcPs)

   -- | Cannot parse data constructor in a data/newtype declaration
   | PsErrInvalidInfixDataCon !(HsType GhcPs) !RdrName !(HsType GhcPs)

   -- | Illegal DataKinds quote mark in data/newtype constructor declaration
   | PsErrIllegalPromotionQuoteDataCon !RdrName

   -- | UNPACK applied to a data constructor
   | PsErrUnpackDataCon

   -- | Unexpected kind application in data/newtype declaration
   | PsErrUnexpectedKindAppInDataCon !DataConBuilder !(HsType GhcPs)

   -- | Not a record constructor
   | PsErrInvalidRecordCon !(PatBuilder GhcPs)

   -- | Illegal unboxed string literal in pattern
   | PsErrIllegalUnboxedStringInPat !(HsLit GhcPs)

   -- | Illegal primitive floating point literal in pattern
   | PsErrIllegalUnboxedFloatingLitInPat !(HsLit GhcPs)

   -- | Do-notation in pattern
   | PsErrDoNotationInPat

   -- | If-then-else syntax in pattern
   | PsErrIfThenElseInPat

   -- | Lambda-case in pattern
   | PsErrLambdaCaseInPat LamCaseVariant

   -- | case..of in pattern
   | PsErrCaseInPat

   -- | let-syntax in pattern
   | PsErrLetInPat

   -- | Lambda-syntax in pattern
   | PsErrLambdaInPat

   -- | Arrow expression-syntax in pattern
   | PsErrArrowExprInPat !(HsExpr GhcPs)

   -- | Arrow command-syntax in pattern
   | PsErrArrowCmdInPat !(HsCmd GhcPs)

   -- | Arrow command-syntax in expression
   | PsErrArrowCmdInExpr !(HsCmd GhcPs)

   -- | View-pattern in expression
   | PsErrViewPatInExpr !(LHsExpr GhcPs) !(LHsExpr GhcPs)

   -- | Type-application without space before '@'
   | PsErrTypeAppWithoutSpace !RdrName !(LHsExpr GhcPs)

   -- | Lazy-pattern ('~') without space after it
   | PsErrLazyPatWithoutSpace !(LHsExpr GhcPs)

   -- | Bang-pattern ('!') without space after it
   | PsErrBangPatWithoutSpace !(LHsExpr GhcPs)

   -- | Pragma not allowed in this position
   | PsErrUnallowedPragma !(HsPragE GhcPs)

   -- | Qualified do block in command
   | PsErrQualifiedDoInCmd !ModuleName

   -- | Invalid infix hole, expected an infix operator
   | PsErrInvalidInfixHole

   -- | Unexpected semi-colons in conditional expression
   | PsErrSemiColonsInCondExpr
       !(HsExpr GhcPs) -- ^ conditional expr
       !Bool           -- ^ "then" semi-colon?
       !(HsExpr GhcPs) -- ^ "then" expr
       !Bool           -- ^ "else" semi-colon?
       !(HsExpr GhcPs) -- ^ "else" expr

   -- | Unexpected semi-colons in conditional command
   | PsErrSemiColonsInCondCmd
       !(HsExpr GhcPs) -- ^ conditional expr
       !Bool           -- ^ "then" semi-colon?
       !(HsCmd GhcPs)  -- ^ "then" expr
       !Bool           -- ^ "else" semi-colon?
       !(HsCmd GhcPs)  -- ^ "else" expr

   -- | @-operator in a pattern position
   | PsErrAtInPatPos

   -- | Unexpected lambda command in function application
   | PsErrLambdaCmdInFunAppCmd !(LHsCmd GhcPs)

   -- | Unexpected case command in function application
   | PsErrCaseCmdInFunAppCmd !(LHsCmd GhcPs)

   -- | Unexpected \case(s) command in function application
   | PsErrLambdaCaseCmdInFunAppCmd !LamCaseVariant !(LHsCmd GhcPs)

   -- | Unexpected if command in function application
   | PsErrIfCmdInFunAppCmd !(LHsCmd GhcPs)

   -- | Unexpected let command in function application
   | PsErrLetCmdInFunAppCmd !(LHsCmd GhcPs)

   -- | Unexpected do command in function application
   | PsErrDoCmdInFunAppCmd !(LHsCmd GhcPs)

   -- | Unexpected do block in function application
   | PsErrDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)

   -- | Unexpected mdo block in function application
   | PsErrMDoInFunAppExpr !(Maybe ModuleName) !(LHsExpr GhcPs)

   -- | Unexpected lambda expression in function application
   | PsErrLambdaInFunAppExpr !(LHsExpr GhcPs)

   -- | Unexpected case expression in function application
   | PsErrCaseInFunAppExpr !(LHsExpr GhcPs)

   -- | Unexpected \case(s) expression in function application
   | PsErrLambdaCaseInFunAppExpr !LamCaseVariant !(LHsExpr GhcPs)

   -- | Unexpected let expression in function application
   | PsErrLetInFunAppExpr !(LHsExpr GhcPs)

   -- | Unexpected if expression in function application
   | PsErrIfInFunAppExpr !(LHsExpr GhcPs)

   -- | Unexpected proc expression in function application
   | PsErrProcInFunAppExpr !(LHsExpr GhcPs)

   -- | Malformed head of type or class declaration
   | PsErrMalformedTyOrClDecl !(LHsType GhcPs)

   -- | Illegal 'where' keyword in data declaration
   | PsErrIllegalWhereInDataDecl

   -- | Illegal datatype context
   | PsErrIllegalDataTypeContext !(LHsContext GhcPs)

   -- | Parse error on input
   | PsErrParseErrorOnInput !OccName

   -- | Malformed ... declaration for ...
   | PsErrMalformedDecl !SDoc !RdrName

   -- | Unexpected type application in a declaration
   | PsErrUnexpectedTypeAppInDecl !(LHsType GhcPs) !SDoc !RdrName

   -- | Not a data constructor
   | PsErrNotADataCon !RdrName

   -- | Record syntax used in pattern synonym declaration
   | PsErrRecordSyntaxInPatSynDecl !(LPat GhcPs)

   -- | Empty 'where' clause in pattern-synonym declaration
   | PsErrEmptyWhereInPatSynDecl !RdrName

   -- | Invalid binding name in 'where' clause of pattern-synonym declaration
   | PsErrInvalidWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)

   -- | Multiple bindings in 'where' clause of pattern-synonym declaration
   | PsErrNoSingleWhereBindInPatSynDecl !RdrName !(HsDecl GhcPs)

   -- | Declaration splice not a top-level
   | PsErrDeclSpliceNotAtTopLevel !(SpliceDecl GhcPs)

   -- | Inferred type variables not allowed here
   | PsErrInferredTypeVarNotAllowed

   -- | Multiple names in standalone kind signatures
   | PsErrMultipleNamesInStandaloneKindSignature [LIdP GhcPs]

   -- | Illegal import bundle form
   | PsErrIllegalImportBundleForm

   -- | Illegal role name
   | PsErrIllegalRoleName !FastString [Role]

   -- | Invalid type signature
   | PsErrInvalidTypeSignature !(LHsExpr GhcPs)

   -- | Unexpected type in declaration
   | PsErrUnexpectedTypeInDecl !(LHsType GhcPs)
                               !SDoc
                               !RdrName
                               [LHsTypeArg GhcPs]
                               !SDoc

   -- | Expected a hyphen
   | PsErrExpectedHyphen

   -- | Found a space in a SCC
   | PsErrSpaceInSCC

   -- | Found two single quotes
   | PsErrEmptyDoubleQuotes !Bool
                            -- ^ Is TH on?

   -- | Invalid package name
   | PsErrInvalidPackageName !FastString

   -- | Invalid rule activation marker
   | PsErrInvalidRuleActivationMarker

   -- | Linear function found but LinearTypes not enabled
   | PsErrLinearFunction

   -- | Multi-way if-expression found but MultiWayIf not enabled
   | PsErrMultiWayIf

   -- | Explicit forall found but no extension allowing it is enabled
   | PsErrExplicitForall !Bool
                         -- ^ is Unicode forall?

   -- | Found qualified-do without QualifiedDo enabled
   | PsErrIllegalQualifiedDo !SDoc

   -- | Cmm parser error
   | PsErrCmmParser !CmmParserError

   -- | Illegal traditional record syntax
   --
   -- TODO: distinguish errors without using SDoc
   | PsErrIllegalTraditionalRecordSyntax !SDoc

   -- | Parse error in command
   --
   -- TODO: distinguish errors without using SDoc
   | PsErrParseErrorInCmd !SDoc

   -- | Parse error in pattern
   | PsErrInPat !(PatBuilder GhcPs) !PsErrInPatDetails

   -- | Parse error in right operator section pattern
   -- TODO: embed the proper operator, if possible
   | forall infixOcc. (OutputableBndr infixOcc) => PsErrParseRightOpSectionInPat !infixOcc !(PatBuilder GhcPs)

   -- | Illegal linear arrow or multiplicity annotation in GADT record syntax
   | PsErrIllegalGadtRecordMultiplicity !(HsArrow GhcPs)

   | PsErrInvalidCApiImport

-- | Extra details about a parse error, which helps
-- us in determining which should be the hints to
-- suggest.
data PsErrParseDetails
  = PsErrParseDetails
  { ped_th_enabled :: !Bool
    -- Is 'TemplateHaskell' enabled?
  , ped_do_in_last_100 :: !Bool
    -- ^ Is there a 'do' in the last 100 characters?
  , ped_mdo_in_last_100 :: !Bool
    -- ^ Is there an 'mdo' in the last 100 characters?
  , ped_pat_syn_enabled :: !Bool
    -- ^ Is 'PatternSynonyms' enabled?
  , ped_pattern_parsed :: !Bool
    -- ^ Did we parse a \"pattern\" keyword?
  }

-- | Is the parsed pattern recursive?
data PatIsRecursive
  = YesPatIsRecursive
  | NoPatIsRecursive

data PatIncompleteDoBlock
  = YesIncompleteDoBlock
  | NoIncompleteDoBlock
  deriving Eq

-- | Extra information for the expression GHC is currently inspecting/parsing.
-- It can be used to generate more informative parser diagnostics and hints.
data ParseContext
  = ParseContext
  { is_infix :: !(Maybe RdrName)
    -- ^ If 'Just', this is an infix
    -- pattern with the binded operator name
  , incomplete_do_block :: !PatIncompleteDoBlock
    -- ^ Did the parser likely fail due to an incomplete do block?
  } deriving Eq

data PsErrInPatDetails
  = PEIP_NegApp
    -- ^ Negative application pattern?
  | PEIP_TypeArgs [HsPatSigType GhcPs]
    -- ^ The list of type arguments for the pattern
  | PEIP_RecPattern [LPat GhcPs]    -- ^ The pattern arguments
                    !PatIsRecursive -- ^ Is the parsed pattern recursive?
                    !ParseContext
  | PEIP_OtherPatDetails !ParseContext

noParseContext :: ParseContext
noParseContext = ParseContext Nothing NoIncompleteDoBlock

incompleteDoBlock :: ParseContext
incompleteDoBlock = ParseContext Nothing YesIncompleteDoBlock

-- | Builds a 'PsErrInPatDetails' with the information provided by the 'ParseContext'.
fromParseContext :: ParseContext -> PsErrInPatDetails
fromParseContext = PEIP_OtherPatDetails

data NumUnderscoreReason
   = NumUnderscore_Integral
   | NumUnderscore_Float
   deriving (Show,Eq,Ord)

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
   | LexStringCharLit       -- ^ Lexical error in string/character literal
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

data TransLayoutReason
   = TransLayout_Where -- ^ "`where' clause at the same depth as implicit layout block"
   | TransLayout_Pipe  -- ^ "`|' at the same depth as implicit layout block")


data FileHeaderPragmaType
  = OptionsPrag
  | IncludePrag
  | LanguagePrag
  | DocOptionsPrag
