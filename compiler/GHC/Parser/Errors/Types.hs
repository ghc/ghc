{-# LANGUAGE GADTs #-}

module GHC.Parser.Errors.Types where

import GHC.Prelude

import Data.Typeable

import GHC.Core.TyCon (Role)
import GHC.Data.FastString
import GHC.Hs
import GHC.Parser.Types
import GHC.Types.Error
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Name.Reader
import GHC.Unit.Module.Name
import GHC.Utils.Outputable

data PsMessage where
   PsUnknownMessage :: (Diagnostic a, Typeable a) => a -> PsMessage

   -- | Warn when tabulations are found
   PsWarnTab :: !Word -> PsMessage
                -- ^ Number of other occurrences other than the first one

   -- | Transitional layout warnings
   PsWarnTransitionalLayout :: !TransLayoutReason -> PsMessage

   -- | Unrecognised pragma
   PsWarnUnrecognisedPragma :: PsMessage

   -- | Invalid Haddock comment position
   PsWarnHaddockInvalidPos :: PsMessage

   -- | Multiple Haddock comment for the same entity
   PsWarnHaddockIgnoreMulti :: PsMessage

   -- | Found binding occurrence of "*" while StarIsType is enabled
   PsWarnStarBinder :: PsMessage

   -- | Using "*" for "Type" without StarIsType enabled
   PsWarnStarIsType :: PsMessage

   -- | Pre qualified import with 'WarnPrepositiveQualifiedModule' enabled
   PsWarnImportPreQualified :: PsMessage

   PsWarnOperatorWhitespaceExtConflict :: !OperatorWhitespaceSymbol -> PsMessage

   PsWarnOperatorWhitespace :: !FastString -> !OperatorWhitespaceOccurrence -> PsMessage

   -- | LambdaCase syntax used without the extension enabled
   PsErrLambdaCase :: PsMessage

   -- | A lambda requires at least one parameter
   PsErrEmptyLambda :: PsMessage

   -- | Underscores in literals without the extension enabled
   PsErrNumUnderscores :: !NumUnderscoreReason -> PsMessage

   -- | Invalid character in primitive string
   PsErrPrimStringInvalidChar :: PsMessage

   -- | Missing block
   PsErrMissingBlock :: PsMessage

   -- | Lexer error
   PsErrLexer :: !LexErr -> !LexErrKind -> PsMessage

   -- | Suffix occurrence of `@`
   PsErrSuffixAT :: PsMessage

   -- | Parse errors
   PsErrParse :: !String -> !PsErrParseDetails -> PsMessage

   -- | Cmm lexer error
   PsErrCmmLexer :: PsMessage

   -- | Unsupported boxed sum in expression
   PsErrUnsupportedBoxedSumExpr :: !(SumOrTuple (HsExpr GhcPs)) -> PsMessage

   -- | Unsupported boxed sum in pattern
   PsErrUnsupportedBoxedSumPat :: !(SumOrTuple (PatBuilder GhcPs)) -> PsMessage

   -- | Unexpected qualified constructor
   PsErrUnexpectedQualifiedConstructor :: !RdrName -> PsMessage

   -- | Tuple section in pattern context
   PsErrTupleSectionInPat :: PsMessage

   -- | Bang-pattern without BangPattterns enabled
   PsErrIllegalBangPattern :: !(Pat GhcPs) -> PsMessage

   -- | Operator applied to too few arguments
   PsErrOpFewArgs :: !StarIsType -> !RdrName -> PsMessage

   -- | Import: multiple occurrences of 'qualified'
   PsErrImportQualifiedTwice :: PsMessage

   -- | Post qualified import without 'ImportQualifiedPost'
   PsErrImportPostQualified :: PsMessage

   -- | Explicit namespace keyword without 'ExplicitNamespaces'
   PsErrIllegalExplicitNamespace :: PsMessage

   -- | Expecting a type constructor but found a variable
   PsErrVarForTyCon :: !RdrName -> PsMessage

   -- | Illegal export form allowed by PatternSynonyms
   PsErrIllegalPatSynExport :: PsMessage

   -- | Malformed entity string
   PsErrMalformedEntityString :: PsMessage

   -- | Dots used in record update
   PsErrDotsInRecordUpdate :: PsMessage

   -- | Precedence out of range
   PsErrPrecedenceOutOfRange :: !Int -> PsMessage

   -- | Invalid use of record dot syntax `.'
   PsErrOverloadedRecordDotInvalid :: PsMessage

   -- | `OverloadedRecordUpdate` is not enabled.
   PsErrOverloadedRecordUpdateNotEnabled :: PsMessage

   -- | Can't use qualified fields when OverloadedRecordUpdate is enabled.
   PsErrOverloadedRecordUpdateNoQualifiedFields :: PsMessage

   -- | Cannot parse data constructor in a data/newtype declaration
   PsErrInvalidDataCon :: !(HsType GhcPs) -> PsMessage

   -- | Cannot parse data constructor in a data/newtype declaration
   PsErrInvalidInfixDataCon :: !(HsType GhcPs) -> !RdrName -> !(HsType GhcPs) -> PsMessage

   -- | UNPACK applied to a data constructor
   PsErrUnpackDataCon :: PsMessage

   -- | Unexpected kind application in data/newtype declaration
   PsErrUnexpectedKindAppInDataCon :: !DataConBuilder -> !(HsType GhcPs) -> PsMessage

   -- | Not a record constructor
   PsErrInvalidRecordCon :: !(PatBuilder GhcPs) -> PsMessage

   -- | Illegal unboxed string literal in pattern
   PsErrIllegalUnboxedStringInPat :: !(HsLit GhcPs) -> PsMessage

   -- | Do-notation in pattern
   PsErrDoNotationInPat :: PsMessage

   -- | If-then-else syntax in pattern
   PsErrIfThenElseInPat :: PsMessage

   -- | Lambda-case in pattern
   PsErrLambdaCaseInPat :: PsMessage

   -- | case..of in pattern
   PsErrCaseInPat :: PsMessage

   -- | let-syntax in pattern
   PsErrLetInPat :: PsMessage

   -- | Lambda-syntax in pattern
   PsErrLambdaInPat :: PsMessage

   -- | Arrow expression-syntax in pattern
   PsErrArrowExprInPat :: !(HsExpr GhcPs) -> PsMessage

   -- | Arrow command-syntax in pattern
   PsErrArrowCmdInPat :: !(HsCmd GhcPs) -> PsMessage

   -- | Arrow command-syntax in expression
   PsErrArrowCmdInExpr :: !(HsCmd GhcPs) -> PsMessage

   -- | View-pattern in expression
   PsErrViewPatInExpr :: !(LHsExpr GhcPs) -> !(LHsExpr GhcPs) -> PsMessage

   -- | Type-application without space before '@'
   PsErrTypeAppWithoutSpace :: !RdrName -> !(LHsExpr GhcPs) -> PsMessage

   -- | Lazy-pattern ('~') without space after it
   PsErrLazyPatWithoutSpace :: !(LHsExpr GhcPs) -> PsMessage

   -- | Bang-pattern ('!') without space after it
   PsErrBangPatWithoutSpace :: !(LHsExpr GhcPs) -> PsMessage

   -- | Pragma not allowed in this position
   PsErrUnallowedPragma :: !(HsPragE GhcPs) -> PsMessage

   -- | Qualified do block in command
   PsErrQualifiedDoInCmd :: !ModuleName -> PsMessage

   -- | Invalid infix hole, expected an infix operator
   PsErrInvalidInfixHole :: PsMessage

   -- | Unexpected semi-colons in conditional expression
   PsErrSemiColonsInCondExpr
       :: !(HsExpr GhcPs) -- ^ conditional expr
       -> !Bool           -- ^ "then" semi-colon?
       -> !(HsExpr GhcPs) -- ^ "then" expr
       -> !Bool           -- ^ "else" semi-colon?
       -> !(HsExpr GhcPs) -- ^ "else" expr
       -> PsMessage

   -- | Unexpected semi-colons in conditional command
   PsErrSemiColonsInCondCmd
       :: !(HsExpr GhcPs) -- ^ conditional expr
       -> !Bool           -- ^ "then" semi-colon?
       -> !(HsCmd GhcPs)  -- ^ "then" expr
       -> !Bool           -- ^ "else" semi-colon?
       -> !(HsCmd GhcPs)  -- ^ "else" expr
       -> PsMessage

   -- | @-operator in a pattern position
   PsErrAtInPatPos :: PsMessage

   -- | Unexpected lambda command in function application
   PsErrLambdaCmdInFunAppCmd :: !(LHsCmd GhcPs) -> PsMessage

   -- | Unexpected case command in function application
   PsErrCaseCmdInFunAppCmd :: !(LHsCmd GhcPs) -> PsMessage

   -- | Unexpected if command in function application
   PsErrIfCmdInFunAppCmd :: !(LHsCmd GhcPs) -> PsMessage

   -- | Unexpected let command in function application
   PsErrLetCmdInFunAppCmd :: !(LHsCmd GhcPs) -> PsMessage

   -- | Unexpected do command in function application
   PsErrDoCmdInFunAppCmd :: !(LHsCmd GhcPs) -> PsMessage

   -- | Unexpected do block in function application
   PsErrDoInFunAppExpr :: !(Maybe ModuleName) -> !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected mdo block in function application
   PsErrMDoInFunAppExpr :: !(Maybe ModuleName) -> !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected lambda expression in function application
   PsErrLambdaInFunAppExpr :: !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected case expression in function application
   PsErrCaseInFunAppExpr :: !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected lambda-case expression in function application
   PsErrLambdaCaseInFunAppExpr :: !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected let expression in function application
   PsErrLetInFunAppExpr :: !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected if expression in function application
   PsErrIfInFunAppExpr :: !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected proc expression in function application
   PsErrProcInFunAppExpr :: !(LHsExpr GhcPs) -> PsMessage

   -- | Malformed head of type or class declaration
   PsErrMalformedTyOrClDecl :: !(LHsType GhcPs) -> PsMessage

   -- | Illegal 'where' keyword in data declaration
   PsErrIllegalWhereInDataDecl :: PsMessage

   -- | Illegal datatype context
   PsErrIllegalDataTypeContext :: !(LHsContext GhcPs) -> PsMessage

   -- | Parse error on input
   PsErrParseErrorOnInput :: !OccName -> PsMessage

   -- | Malformed ... declaration for ...
   PsErrMalformedDecl :: !SDoc -> !RdrName -> PsMessage

   -- | Unexpected type application in a declaration
   PsErrUnexpectedTypeAppInDecl :: !(LHsType GhcPs) -> !SDoc -> !RdrName -> PsMessage

   -- | Not a data constructor
   PsErrNotADataCon :: !RdrName -> PsMessage

   -- | Record syntax used in pattern synonym declaration
   PsErrRecordSyntaxInPatSynDecl :: !(LPat GhcPs) -> PsMessage

   -- | Empty 'where' clause in pattern-synonym declaration
   PsErrEmptyWhereInPatSynDecl :: !RdrName -> PsMessage

   -- | Invalid binding name in 'where' clause of pattern-synonym declaration
   PsErrInvalidWhereBindInPatSynDecl :: !RdrName -> !(HsDecl GhcPs) -> PsMessage

   -- | Multiple bindings in 'where' clause of pattern-synonym declaration
   PsErrNoSingleWhereBindInPatSynDecl :: !RdrName -> !(HsDecl GhcPs) -> PsMessage

   -- | Declaration splice not a top-level
   PsErrDeclSpliceNotAtTopLevel :: !(SpliceDecl GhcPs) -> PsMessage

   -- | Inferred type variables not allowed here
   PsErrInferredTypeVarNotAllowed :: PsMessage

   -- | Multiple names in standalone kind signatures
   PsErrMultipleNamesInStandaloneKindSignature :: [LIdP GhcPs] -> PsMessage

   -- | Illegal import bundle form
   PsErrIllegalImportBundleForm :: PsMessage

   -- | Illegal role name
   PsErrIllegalRoleName :: !FastString -> [Role] -> PsMessage

   -- | Invalid type signature
   PsErrInvalidTypeSignature :: !(LHsExpr GhcPs) -> PsMessage

   -- | Unexpected type in declaration
   PsErrUnexpectedTypeInDecl :: !(LHsType GhcPs)
                             -> !SDoc
                             -> !RdrName
                             -> [LHsTypeArg GhcPs]
                             -> !SDoc
                             -> PsMessage

   -- | Expected a hyphen
   PsErrExpectedHyphen :: PsMessage

   -- | Found a space in a SCC
   PsErrSpaceInSCC :: PsMessage

   -- | Found two single quotes
   PsErrEmptyDoubleQuotes :: !Bool
                             -- Is TH on?
                          -> PsMessage

   -- | Invalid package name
   PsErrInvalidPackageName :: !FastString -> PsMessage

   -- | Invalid rule activation marker
   PsErrInvalidRuleActivationMarker :: PsMessage

   -- | Linear function found but LinearTypes not enabled
   PsErrLinearFunction :: PsMessage

   -- | Multi-way if-expression found but MultiWayIf not enabled
   PsErrMultiWayIf :: PsMessage

   -- | Explicit forall found but no extension allowing it is enabled
   PsErrExplicitForall :: !Bool
                          -- is Unicode forall?
                       -> PsMessage

   -- | Found qualified-do without QualifiedDo enabled
   PsErrIllegalQualifiedDo :: !SDoc -> PsMessage

   -- | Cmm parser error
   PsErrCmmParser :: !CmmParserError -> PsMessage

   -- | Illegal traditional record syntax
   --
   -- TODO: distinguish errors without using SDoc
   PsErrIllegalTraditionalRecordSyntax :: !SDoc -> PsMessage

   -- | Parse error in command
   --
   -- TODO: distinguish errors without using SDoc
   PsErrParseErrorInCmd :: !SDoc -> PsMessage

   -- | Parse error in pattern
   PsErrParseErrorInPat :: !(PatBuilder GhcPs) -> !PsParseErrorInPatDetails -> PsMessage

   -- | Parse error in right operator section pattern
   -- TODO: embed the proper operator, if possible
   PsErrParseRightOpSectionInPat :: OutputableBndr infixOcc => !infixOcc -> PatBuilder GhcPs -> PsMessage

newtype StarIsType = StarIsType Bool

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

data PsParseErrorInPatDetails
  = PsParseErrorInPatDetails
  { peipd_tyargs :: [HsPatSigType GhcPs]
    -- ^ The number of tyargs
  , peipd_args_num :: !Int
    -- ^ The number of arguments
  , peipd_pat_is_rec :: !Bool
    -- ^ Is the parsed pattern recursive?
  , peipd_neg_app :: !Bool
    -- ^ Negative application pattern?
  , peipd_is_infix :: !(Maybe RdrName)
    -- ^ If 'Just', this is an infix pattern with the binded operator name
  , peipd_incomplete_do_block :: !Bool
    -- ^ If 'True', the parser likely failed due to an incomplete do block
  }

noParseErrorInPatDetails :: PsParseErrorInPatDetails
noParseErrorInPatDetails = PsParseErrorInPatDetails
  { peipd_tyargs   = []
  , peipd_args_num = 0
  , peipd_pat_is_rec = False
  , peipd_neg_app = False
  , peipd_is_infix = Nothing
  , peipd_incomplete_do_block = False
  }

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
