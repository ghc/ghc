{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic PsMessage

module GHC.Parser.Errors.Ppr
   ( mkParserWarn
   , mkParserErr
   , pprPsError
   )
where

import GHC.Prelude
import GHC.Driver.Flags
import GHC.Parser.Errors
import qualified GHC.Parser.Errors.Types as T
import GHC.Parser.Types
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader (starInfo, rdrNameOcc, opIsAt, mkUnqual)
import GHC.Types.Name.Occurrence (isSymOcc, occNameFS, varName)
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Data.FastString
import GHC.Hs.Expr (prependQualified,HsExpr(..))
import GHC.Hs.Type (pprLHsContext)
import GHC.Builtin.Names (allNameStrings)
import GHC.Builtin.Types (filterCTuple)
import GHC.Driver.Session (DynFlags)
import GHC.Utils.Error (diagReasonSeverity)
import qualified GHC.LanguageExtensions as Ext

--
-- Diagnostic instances
--

instance Diagnostic T.PsMessage where
  diagnosticMessage = \case
    T.PsUnknownMessage _hints m
      -> diagnosticMessage m
    T.PsLayoutMessage layoutDiagnostic
      -> diagnosticMessage layoutDiagnostic
    T.PsHaddockMessage haddockDiagnostic
      -> diagnosticMessage haddockDiagnostic
    T.PsExtensionMessage extDiagnostic
      -> diagnosticMessage extDiagnostic
    T.PsImportOrPragmaMessage impDiagnostic
      -> diagnosticMessage impDiagnostic
    T.PsLexerError err kind
      -> mkSimpleDecorated $ hcat
           [ text $ case err of
              T.LexError               -> "lexical error"
              T.LexUnknownPragma       -> "unknown pragma"
              T.LexErrorInPragma       -> "lexical error in pragma"
              T.LexNumEscapeRange      -> "numeric escape sequence out of range"
              T.LexStringCharLit       -> "lexical error in string/character literal"
              T.LexStringCharLitEOF    -> "unexpected end-of-file in string/character literal"
              T.LexUnterminatedComment -> "unterminated `{-'"
              T.LexUnterminatedOptions -> "unterminated OPTIONS pragma"
              T.LexUnterminatedQQ      -> "unterminated quasiquotation"

           , text $ case kind of
              T.LexErrKind_EOF    -> " at end of input"
              T.LexErrKind_UTF8   -> " (UTF-8 decoding error)"
              T.LexErrKind_Char c -> " at character " ++ show c
           ]
    T.PsParseError token
      | null token
      -> mkSimpleDecorated $ text "parse error (possibly incorrect indentation or mismatched brackets)"

      | otherwise
      -> mkSimpleDecorated $ text "parse error on input" <+> quotes (text token)
    T.PsCmmLexerError
      -> mkSimpleDecorated $ text "Cmm lexical error"
    T.PsCmmParserError cmm_err -> mkSimpleDecorated $ case cmm_err of
      T.CmmUnknownPrimitive name     -> text "unknown primitive" <+> ftext name
      T.CmmUnknownMacro fun          -> text "unknown macro" <+> ftext fun
      T.CmmUnknownCConv cconv        -> text "unknown calling convention:" <+> text cconv
      T.CmmUnrecognisedSafety safety -> text "unrecognised safety" <+> text safety
      T.CmmUnrecognisedHint hint     -> text "unrecognised hint:" <+> text hint
    T.PsMalformedExpr malExpr
      -> diagnosticMessage malExpr
  diagnosticReason  = \case
    T.PsUnknownMessage _hints m
      -> diagnosticReason m
    T.PsLayoutMessage layoutDiagnostic
      -> diagnosticReason layoutDiagnostic
    T.PsHaddockMessage haddockDiagnostic
      -> diagnosticReason haddockDiagnostic
    T.PsExtensionMessage extDiagnostic
      -> diagnosticReason extDiagnostic
    T.PsImportOrPragmaMessage impDiagnostic
      -> diagnosticReason impDiagnostic
    T.PsLexerError{}
      -> ErrorWithoutFlag
    T.PsParseError{}
      -> ErrorWithoutFlag
    T.PsCmmLexerError
      -> ErrorWithoutFlag
    T.PsCmmParserError{}
      -> ErrorWithoutFlag
    T.PsMalformedExpr malExpr
      -> diagnosticReason malExpr


instance Diagnostic T.PsLayoutMessage where
  diagnosticMessage = \case
    T.PsTabulationsFound hint tc
      -> mkSimpleDecorated $
           text "Tab character found here"
             <> (if tc == 1
                 then text ""
                 else text ", and in" <+> speakNOf (fromIntegral (tc - 1)) (text "further location"))
             <> text "."
             $+$ pp_hint hint
    T.PsTransitionalLayout reason
      -> mkSimpleDecorated $
            text "transitional layout will not be accepted in the future:"
            $$ text (case reason of
               T.TransLayout_Where -> "`where' clause at the same depth as implicit layout block"
               T.TransLayout_Pipe  -> "`|' at the same depth as implicit layout block"
            )
    T.PsOperatorWhitespaceExtConflict sym
      -> let mk_prefix_msg operator_symbol extension_name syntax_meaning =
                  text "The prefix use of a" <+> quotes (text operator_symbol)
                    <+> text "would denote" <+> text syntax_meaning
               $$ nest 2 (text "were the" <+> text extension_name <+> text "extension enabled.")
               $$ text "Suggested fix: add whitespace after the"
                    <+> quotes (text operator_symbol) <> char '.'
         in mkSimpleDecorated $
         case sym of
           T.OperatorWhitespaceSymbol_PrefixPercent -> mk_prefix_msg "%" "LinearTypes" "a multiplicity annotation"
           T.OperatorWhitespaceSymbol_PrefixDollar -> mk_prefix_msg "$" "TemplateHaskell" "an untyped splice"
           T.OperatorWhitespaceSymbol_PrefixDollarDollar -> mk_prefix_msg "$$" "TemplateHaskell" "a typed splice"

    T.PsOperatorWhitespace sym occ_type
      -> let mk_msg occ_type_str =
                  text "The" <+> text occ_type_str <+> text "use of a" <+> quotes (ftext sym)
                    <+> text "might be repurposed as special syntax"
               $$ nest 2 (text "by a future language extension.")
               $$ text "Suggested fix: add whitespace around it."
         in mkSimpleDecorated $
         case occ_type of
           T.OperatorWhitespaceOccurrence_Prefix -> mk_msg "prefix"
           T.OperatorWhitespaceOccurrence_Suffix -> mk_msg "suffix"
           T.OperatorWhitespaceOccurrence_TightInfix -> mk_msg "tight infix"
    T.PsTypeAppWithoutSpace v e
      -> mkSimpleDecorated $
           sep [ text "@-pattern in expression context:"
               , nest 4 (pprPrefixOcc v <> text "@" <> ppr e)
               ]
           $$ text "Type application syntax requires a space before '@'"
    T.PsLazyPatWithoutSpace e
      -> mkSimpleDecorated $
           sep [ text "Lazy pattern in expression context:"
               , nest 4 (text "~" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '~'?"
    T.PsBangPatWithoutSpace e
      -> mkSimpleDecorated $
           sep [ text "Bang pattern in expression context:"
               , nest 4 (text "!" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '!'?"
    T.PsInvalidInfixHole
      -> mkSimpleDecorated $ text "Invalid infix hole, expected an infix operator"
    T.PsExpectedHyphen
      -> mkSimpleDecorated $ text "Expected a hyphen"
    T.PsSpaceInSCC
      -> mkSimpleDecorated $ text "Spaces are not allowed in SCCs"
    T.PsEmptyDoubleQuotes th_on
      -> mkSimpleDecorated $ if th_on then vcat (msg ++ th_msg) else vcat msg
         where
            msg    = [ text "Parser error on `''`"
                     , text "Character literals may not be empty"
                     ]
            th_msg = [ text "Or perhaps you intended to use quotation syntax of TemplateHaskell,"
                     , text "but the type variable or constructor is missing"
                     ]
  diagnosticReason = \case
    T.PsTabulationsFound{}
      -> WarningWithFlag Opt_WarnTabs
    T.PsTransitionalLayout{}
      -> WarningWithFlag Opt_WarnAlternativeLayoutRuleTransitional
    T.PsOperatorWhitespaceExtConflict{}
      -> WarningWithFlag Opt_WarnOperatorWhitespaceExtConflict
    T.PsOperatorWhitespace{}
      -> WarningWithFlag Opt_WarnOperatorWhitespace
    T.PsTypeAppWithoutSpace{}
      -> ErrorWithoutFlag
    T.PsLazyPatWithoutSpace{}
      -> ErrorWithoutFlag
    T.PsBangPatWithoutSpace{}
      -> ErrorWithoutFlag
    T.PsInvalidInfixHole
      -> ErrorWithoutFlag
    T.PsExpectedHyphen
      -> ErrorWithoutFlag
    T.PsSpaceInSCC
      -> ErrorWithoutFlag
    T.PsEmptyDoubleQuotes{}
      -> ErrorWithoutFlag


instance Diagnostic T.PsHaddockMessage where
  diagnosticMessage = \case
   T.PsHaddockInvalidPosition
      -> mkSimpleDecorated $ text "A Haddock comment cannot appear in this position and will be ignored."
   T.PsHaddockIgnoreMultipleComments
      -> mkSimpleDecorated $
           text "Multiple Haddock comments for a single entity are not allowed." $$
           text "The extraneous comment will be ignored."
  diagnosticReason = \case
    T.PsHaddockInvalidPosition
      -> WarningWithFlag Opt_WarnInvalidHaddock
    T.PsHaddockIgnoreMultipleComments
      -> WarningWithFlag Opt_WarnInvalidHaddock


instance Diagnostic T.PsExtensionMessage where
  diagnosticMessage = \case
    T.PsSyntaxUsedButNotEnabled Ext.LambdaCase _
      -> mkSimpleDecorated $ text "Illegal lambda-case (use LambdaCase)"
    T.PsSyntaxUsedButNotEnabled Ext.LinearTypes _
      -> mkSimpleDecorated $ text "Enable LinearTypes to allow linear functions"
    T.PsSyntaxUsedButNotEnabled ext hint
      -> mkSimpleDecorated $
           text "The" <+> ppr ext <+> text "syntax was used, but the extension was not enabled."
                       $$ pp_hint hint
    T.PsStarBinderWithStarIsType
      -> mkSimpleDecorated $
            text "Found binding occurrence of" <+> quotes (text "*")
            <+> text "yet StarIsType is enabled."
         $$ text "NB. To use (or export) this operator in"
            <+> text "modules with StarIsType,"
         $$ text "    including the definition module, you must qualify it."
    T.PsStarIsTypeWithoutExtension
      -> mkSimpleDecorated $
             text "Using" <+> quotes (text "*")
             <+> text "(or its Unicode variant) to mean"
             <+> quotes (text "Data.Kind.Type")
          $$ text "relies on the StarIsType extension, which will become"
          $$ text "deprecated in the future."
          $$ text "Suggested fix: use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."
    T.PsNumUnderscoresWithoutExtension reason
      -> mkSimpleDecorated $
           text $ case reason of
             T.NumUnderscore_Integral -> "Use NumericUnderscores to allow underscores in integer literals"
             T.NumUnderscore_Float    -> "Use NumericUnderscores to allow underscores in floating literals"
    T.PsIllegalBangPattern e
      -> mkSimpleDecorated $ text "Illegal bang-pattern (use BangPatterns):" $$ ppr e
    T.PsOverloadedRecordDotInvalid
      -> mkSimpleDecorated $
           text "Use of OverloadedRecordDot '.' not valid ('.' isn't allowed when constructing records or in record patterns)"
    T.PsIllegalPatSynExport
      -> mkSimpleDecorated $ text "Illegal export form (use PatternSynonyms to enable)"
    T.PsOverloadedRecordUpdateNoQualifiedFields
      -> mkSimpleDecorated $ text "Fields cannot be qualified when OverloadedRecordUpdate is enabled"
    T.PsExplicitForallWithoutExtension is_unicode
      -> mkSimpleDecorated $ vcat
           [ text "Illegal symbol" <+> quotes (forallSym is_unicode) <+> text "in type"
           , text "Perhaps you intended to use RankNTypes or a similar language"
           , text "extension to enable explicit-forall syntax:" <+>
             forallSym is_unicode <+> text "<tvs>. <type>"
           ]
         where
          forallSym True  = text "∀"
          forallSym False = text "forall"
    T.PsIllegalQualifiedDoWithoutExtension qdoDoc
      -> mkSimpleDecorated $ vcat
           [ text "Illegal qualified" <+> quotes qdoDoc <+> text "block"
           , text "Perhaps you intended to use QualifiedDo"
           ]
    T.PsQualifiedDoInCmd m
      -> mkSimpleDecorated $
           hang (text "Parse error in command:") 2 $
             text "Found a qualified" <+> ppr m <> text ".do block in a command, but"
             $$ text "qualified 'do' is not supported in commands."
    T.PsRecordSyntaxInPatSynDecl pat
      -> mkSimpleDecorated $
           text "record syntax not supported for pattern synonym declarations:"
           $$ ppr pat
    T.PsEmptyWhereInPatSynDecl patsyn_name
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause cannot be empty"
           $$ text "In the pattern synonym declaration for: "
              <+> ppr (patsyn_name)
    T.PsInvalidWhereBindInPatSynDecl patsyn_name decl
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause must bind the pattern synonym's name"
           <+> quotes (ppr patsyn_name) $$ ppr decl
    T.PsNoSingleWhereBindInPatSynDecl _patsyn_name decl
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause must contain a single binding:"
           $$ ppr decl
    T.PsDeclSpliceNotAtTopLevel d
      -> mkSimpleDecorated $
           hang (text "Declaration splices are allowed only"
                 <+> text "at the top level:")
             2 (ppr d)
    T.PsMultipleNamesInStandaloneKindSignature vs
      -> mkSimpleDecorated $
           vcat [ hang (text "Standalone kind signatures do not support multiple names at the moment:")
                  2 (pprWithCommas ppr vs)
                , text "See https://gitlab.haskell.org/ghc/ghc/issues/16754 for details."
                ]
  diagnosticReason = \case
    T.PsStarBinderWithStarIsType
      -> WarningWithFlag Opt_WarnStarBinder
    T.PsStarIsTypeWithoutExtension
      -> WarningWithFlag Opt_WarnStarIsType
    T.PsSyntaxUsedButNotEnabled{}
      -> ErrorWithoutFlag
    T.PsNumUnderscoresWithoutExtension{}
      -> ErrorWithoutFlag
    T.PsIllegalBangPattern{}
      -> ErrorWithoutFlag
    T.PsOverloadedRecordDotInvalid{}
      -> ErrorWithoutFlag
    T.PsIllegalPatSynExport
      -> ErrorWithoutFlag
    T.PsOverloadedRecordUpdateNoQualifiedFields
      -> ErrorWithoutFlag
    T.PsExplicitForallWithoutExtension{}
      -> ErrorWithoutFlag
    T.PsIllegalQualifiedDoWithoutExtension{}
      -> ErrorWithoutFlag
    T.PsQualifiedDoInCmd{}
      -> ErrorWithoutFlag
    T.PsRecordSyntaxInPatSynDecl{}
      -> ErrorWithoutFlag
    T.PsEmptyWhereInPatSynDecl{}
      -> ErrorWithoutFlag
    T.PsInvalidWhereBindInPatSynDecl{}
      -> ErrorWithoutFlag
    T.PsNoSingleWhereBindInPatSynDecl{}
      -> ErrorWithoutFlag
    T.PsDeclSpliceNotAtTopLevel{}
      -> ErrorWithoutFlag
    T.PsMultipleNamesInStandaloneKindSignature{}
      -> ErrorWithoutFlag


instance Diagnostic T.PsImportOrPragmaMessage where
  diagnosticMessage = \case
    T.PsUnrecognisedPragma
      -> mkSimpleDecorated $ text "Unrecognised pragma"
    T.PsUnallowedPragma prag
      -> mkSimpleDecorated $
           hang (text "A pragma is not allowed in this position:") 2
                (ppr prag)
    T.PsImportPreQualified
      -> mkSimpleDecorated $
            text "Found" <+> quotes (text "qualified")
             <+> text "in prepositive position"
         $$ text "Suggested fix: place " <+> quotes (text "qualified")
             <+> text "after the module name instead."
         $$ text "To allow this, enable language extension 'ImportQualifiedPost'"
    T.PsImportQualifiedTwice
      -> mkSimpleDecorated $ text "Multiple occurrences of 'qualified'"
    T.PsIllegalImportBundleForm
      -> mkSimpleDecorated $
           text "Illegal import form, this syntax can only be used to bundle"
           $+$ text "pattern synonyms with types in module exports."
  diagnosticReason = \case
    T.PsUnrecognisedPragma
      -> WarningWithFlag Opt_WarnUnrecognisedPragmas
    T.PsUnallowedPragma{}
      -> ErrorWithoutFlag
    T.PsImportPreQualified
      -> WarningWithFlag Opt_WarnPrepositiveQualifiedModule
    T.PsImportQualifiedTwice
      -> ErrorWithoutFlag
    T.PsIllegalImportBundleForm
      -> ErrorWithoutFlag


instance Diagnostic T.PsMalformedExprMessage where
  diagnosticMessage = \case
    T.PsMissingBlock
      -> mkSimpleDecorated $ text "Missing block"
    T.PsUnsupportedBoxedSumExpr s
      -> mkSimpleDecorated $
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
    T.PsUnsupportedBoxedSumPattern s
      -> mkSimpleDecorated $
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
    T.PsUnexpectedQualifiedConstructor v
      -> mkSimpleDecorated $
           hang (text "Expected an unqualified type constructor:") 2
                (ppr v)
    T.PsTupleSectionInPattern
      -> mkSimpleDecorated $ text "Tuple section in pattern context"
    T.PsOpTooFewArgs (T.StarIsType star_is_type) op
      -> mkSimpleDecorated $
           text "Operator applied to too few arguments:" <+> ppr op
           $$ starInfo star_is_type op
    T.PsVarForTyCon name
      -> mkSimpleDecorated $
           text "Expecting a type constructor but found a variable,"
             <+> quotes (ppr name) <> text "."
           $$ if isSymOcc $ rdrNameOcc name
              then text "If" <+> quotes (ppr name) <+> text "is a type constructor"
                    <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
              else empty
    T.PsMalformedEntityString
      -> mkSimpleDecorated $ text "Malformed entity string"
    T.PsDotsInRecordUpdate
      -> mkSimpleDecorated $ text "You cannot use `..' in a record update"
    T.PsInvalidDataCon t
      -> mkSimpleDecorated $
           hang (text "Cannot parse data constructor in a data/newtype declaration:") 2
                (ppr t)
    T.PsInvalidInfixDataCon lhs tc rhs
      -> mkSimpleDecorated $
           hang (text "Cannot parse an infix data constructor in a data/newtype declaration:") 2
                (ppr lhs <+> ppr tc <+> ppr rhs)
    T.PsUnpackDataCon
      -> mkSimpleDecorated $ text "{-# UNPACK #-} cannot be applied to a data constructor."
    T.PsUnexpectedKindAppInDataCon lhs ki
      -> mkSimpleDecorated $
           hang (text "Unexpected kind application in a data/newtype declaration:") 2
                (ppr lhs <+> text "@" <> ppr ki)
    T.PsInvalidRecordCon p
      -> mkSimpleDecorated $ text "Not a record constructor:" <+> ppr p
    T.PsIllegalUnboxedStringInPat lit
      -> mkSimpleDecorated $ text "Illegal unboxed string literal in pattern:" $$ ppr lit
    T.PsDoNotationInPat
      -> mkSimpleDecorated $ text "do-notation in pattern"
    T.PsIfThenElseInPat
      -> mkSimpleDecorated $ text "(if ... then ... else ...)-syntax in pattern"
    T.PsLambdaCaseInPat
      -> mkSimpleDecorated $ text "(\\case ...)-syntax in pattern"
    T.PsCaseInPat
      -> mkSimpleDecorated $ text "(case ... of ...)-syntax in pattern"
    T.PsLetInPat
      -> mkSimpleDecorated $ text "(let ... in ...)-syntax in pattern"
    T.PsLambdaInPat
      -> mkSimpleDecorated $
           text "Lambda-syntax in pattern."
           $$ text "Pattern matching on functions is not possible."
    T.PsArrowExprInPat e
      -> mkSimpleDecorated $ text "Expression syntax in pattern:" <+> ppr e
    T.PsArrowCmdInPat c
      -> mkSimpleDecorated $ text "Command syntax in pattern:" <+> ppr c
    T.PsArrowCmdInExpr c
      -> mkSimpleDecorated $
           vcat
           [ text "Arrow command found where an expression was expected:"
           , nest 2 (ppr c)
           ]
    T.PsViewPatInExpr a b
      -> mkSimpleDecorated $
           sep [ text "View pattern in expression context:"
               , nest 4 (ppr a <+> text "->" <+> ppr b)
               ]
    T.PsLambdaCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "lambda command") a
    T.PsCaseCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "case command") a
    T.PsIfCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "if command") a
    T.PsLetCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "let command") a
    T.PsDoCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "do command") a
    T.PsDoInFunAppExpr m a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (prependQualified m (text "do block")) a
    T.PsMDoInFunAppExpr m a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (prependQualified m (text "mdo block")) a
    T.PsLambdaInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "lambda expression") a
    T.PsCaseInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "case expression") a
    T.PsLambdaCaseInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "lambda-case expression") a
    T.PsLetInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "let expression") a
    T.PsIfInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "if expression") a
    T.PsProcInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "proc expression") a
    T.PsMalformedTyOrClDecl ty
      -> mkSimpleDecorated $
           text "Malformed head of type or class declaration:" <+> ppr ty
    T.PsIllegalWhereInDataDecl
      -> mkSimpleDecorated $
           vcat
              [ text "Illegal keyword 'where' in data declaration"
              , text "Perhaps you intended to use GADTs or a similar language"
              , text "extension to enable syntax: data T where"
              ]
    T.PsIllegalDataTypeContext c
      -> mkSimpleDecorated $
           text "Illegal datatype context (use DatatypeContexts):"
             <+> pprLHsContext (Just c)
    T.PsPrimStringInvalidChar
      -> mkSimpleDecorated $ text "primitive string literal must contain only characters <= \'\\xFF\'"
    T.PsSuffixAT
      -> mkSimpleDecorated $
           text "Suffix occurrence of @. For an as-pattern, remove the leading whitespace."
    T.PsPrecedenceOutOfRange i
      -> mkSimpleDecorated $ text "Precedence out of range: " <> int i
    T.PsSemiColonsInCondExpr c st t se e
      -> mkSimpleDecorated $
           text "Unexpected semi-colons in conditional:"
           $$ nest 4 expr
           $$ text "Perhaps you meant to use DoAndIfThenElse?"
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e
    T.PsSemiColonsInCondCmd c st t se e
      -> mkSimpleDecorated $
           text "Unexpected semi-colons in conditional:"
           $$ nest 4 expr
           $$ text "Perhaps you meant to use DoAndIfThenElse?"
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e
    T.PsAtInPatternPosition
      -> mkSimpleDecorated $
           text "Found a binding for the"
           <+> quotes (text "@")
           <+> text "operator in a pattern position."
           $$ perhaps_as_pat
    T.PsParseErrorOnInput occ
      -> mkSimpleDecorated $ text "parse error on input" <+> ftext (occNameFS occ)
    T.PsMalformedDecl what for
      -> mkSimpleDecorated $
           text "Malformed" <+> what
           <+> text "declaration for" <+> quotes (ppr for)
    T.PsUnexpectedTypeAppInDecl ki what for
      -> mkSimpleDecorated $
           vcat [ text "Unexpected type application"
                  <+> text "@" <> ppr ki
                , text "In the" <+> what
                  <+> text "declaration for"
                  <+> quotes (ppr for)
                ]
    T.PsNotADataCon name
      -> mkSimpleDecorated $ text "Not a data constructor:" <+> quotes (ppr name)
    T.PsInferredTypeVarNotAllowed
      -> mkSimpleDecorated $ text "Inferred type variables are not allowed here"
    T.PsIllegalTraditionalRecordSyntax s
      -> mkSimpleDecorated $
           text "Illegal record syntax (use TraditionalRecordSyntax):" <+> s
    T.PsParseErrorInCmd s
      -> mkSimpleDecorated $ hang (text "Parse error in command:") 2 s
    T.PsParseErrorInPat s
      -> mkSimpleDecorated $ text "Parse error in pattern:" <+> s
    T.PsIllegalRoleName role nearby
      -> mkSimpleDecorated $
           text "Illegal role name" <+> quotes (ppr role)
           $$ case nearby of
               []  -> empty
               [r] -> text "Perhaps you meant" <+> quotes (ppr r)
               -- will this last case ever happen??
               _   -> hang (text "Perhaps you meant one of these:")
                           2 (pprWithCommas (quotes . ppr) nearby)
    T.PsInvalidTypeSignature lhs
      -> mkSimpleDecorated $
           text "Invalid type signature:"
           <+> ppr lhs
           <+> text ":: ..."
           $$ text hint
         where
         hint | foreign_RDR `looks_like` lhs
              = "Perhaps you meant to use ForeignFunctionInterface?"
              | default_RDR `looks_like` lhs
              = "Perhaps you meant to use DefaultSignatures?"
              | pattern_RDR `looks_like` lhs
              = "Perhaps you meant to use PatternSynonyms?"
              | otherwise
              = "Should be of form <variable> :: <type>"

         -- A common error is to forget the ForeignFunctionInterface flag
         -- so check for that, and suggest.  cf #3805
         -- Sadly 'foreign import' still barfs 'parse error' because
         --  'import' is a keyword
         -- looks_like :: RdrName -> LHsExpr GhcPs -> Bool -- AZ
         looks_like s (L _ (HsVar _ (L _ v))) = v == s
         looks_like s (L _ (HsApp _ lhs _))   = looks_like s lhs
         looks_like _ _                       = False

         foreign_RDR = mkUnqual varName (fsLit "foreign")
         default_RDR = mkUnqual varName (fsLit "default")
         pattern_RDR = mkUnqual varName (fsLit "pattern")
    T.PsUnexpectedTypeInDecl t what tc tparms equals_or_where
      -> mkSimpleDecorated $
           vcat [ text "Unexpected type" <+> quotes (ppr t)
                , text "In the" <+> what
                  <+> ptext (sLit "declaration for") <+> quotes tc'
                , vcat[ (text "A" <+> what
                         <+> ptext (sLit "declaration should have form"))
                , nest 2
                  (what
                   <+> tc'
                   <+> hsep (map text (takeList tparms allNameStrings))
                   <+> equals_or_where) ] ]
          where
            -- Avoid printing a constraint tuple in the error message. Print
            -- a plain old tuple instead (since that's what the user probably
            -- wrote). See #14907
            tc' = ppr $ filterCTuple tc
    T.PsInvalidPackageName pkg
      -> mkSimpleDecorated $ vcat
            [ text "Parse error" <> colon <+> quotes (ftext pkg)
            , text "Version number or non-alphanumeric" <+>
              text "character in package name"
            ]
    T.PsInvalidRuleActivationMarker
      -> mkSimpleDecorated $ text "Invalid rule activation marker"
  diagnosticReason  = \case
    T.PsMissingBlock
      -> ErrorWithoutFlag
    T.PsUnsupportedBoxedSumExpr{}
      -> ErrorWithoutFlag
    T.PsUnsupportedBoxedSumPattern{}
      -> ErrorWithoutFlag
    T.PsUnexpectedQualifiedConstructor{}
      -> ErrorWithoutFlag
    T.PsTupleSectionInPattern{}
      -> ErrorWithoutFlag
    T.PsOpTooFewArgs{}
      -> ErrorWithoutFlag
    T.PsVarForTyCon{}
      -> ErrorWithoutFlag
    T.PsMalformedEntityString
      -> ErrorWithoutFlag
    T.PsDotsInRecordUpdate
      -> ErrorWithoutFlag
    T.PsInvalidDataCon{}
      -> ErrorWithoutFlag
    T.PsInvalidInfixDataCon{}
      -> ErrorWithoutFlag
    T.PsUnpackDataCon
      -> ErrorWithoutFlag
    T.PsUnexpectedKindAppInDataCon{}
      -> ErrorWithoutFlag
    T.PsInvalidRecordCon{}
      -> ErrorWithoutFlag
    T.PsIllegalUnboxedStringInPat{}
      -> ErrorWithoutFlag
    T.PsDoNotationInPat
      -> ErrorWithoutFlag
    T.PsIfThenElseInPat
      -> ErrorWithoutFlag
    T.PsLambdaCaseInPat
      -> ErrorWithoutFlag
    T.PsCaseInPat
      -> ErrorWithoutFlag
    T.PsLetInPat
      -> ErrorWithoutFlag
    T.PsLambdaInPat
      -> ErrorWithoutFlag
    T.PsArrowExprInPat{}
      -> ErrorWithoutFlag
    T.PsArrowCmdInPat{}
      -> ErrorWithoutFlag
    T.PsArrowCmdInExpr{}
      -> ErrorWithoutFlag
    T.PsViewPatInExpr{}
      -> ErrorWithoutFlag
    T.PsLambdaCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    T.PsCaseCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    T.PsIfCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    T.PsLetCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    T.PsDoCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    T.PsDoInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsMDoInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsLambdaInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsCaseInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsLambdaCaseInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsLetInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsIfInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsProcInFunAppExpr{}
      -> ErrorWithoutFlag
    T.PsMalformedTyOrClDecl{}
      -> ErrorWithoutFlag
    T.PsIllegalWhereInDataDecl
      -> ErrorWithoutFlag
    T.PsIllegalDataTypeContext{}
      -> ErrorWithoutFlag
    T.PsPrimStringInvalidChar
      -> ErrorWithoutFlag
    T.PsSuffixAT
      -> ErrorWithoutFlag
    T.PsPrecedenceOutOfRange{}
      -> ErrorWithoutFlag
    T.PsSemiColonsInCondExpr{}
      -> ErrorWithoutFlag
    T.PsSemiColonsInCondCmd{}
      -> ErrorWithoutFlag
    T.PsAtInPatternPosition
      -> ErrorWithoutFlag
    T.PsParseErrorOnInput{}
      -> ErrorWithoutFlag
    T.PsMalformedDecl{}
      -> ErrorWithoutFlag
    T.PsUnexpectedTypeAppInDecl{}
      -> ErrorWithoutFlag
    T.PsNotADataCon{}
      -> ErrorWithoutFlag
    T.PsInferredTypeVarNotAllowed
      -> ErrorWithoutFlag
    T.PsIllegalTraditionalRecordSyntax{}
      -> ErrorWithoutFlag
    T.PsParseErrorInCmd{}
      -> ErrorWithoutFlag
    T.PsParseErrorInPat{}
      -> ErrorWithoutFlag
    T.PsIllegalRoleName{}
      -> ErrorWithoutFlag
    T.PsInvalidTypeSignature{}
      -> ErrorWithoutFlag
    T.PsUnexpectedTypeInDecl{}
      -> ErrorWithoutFlag
    T.PsInvalidPackageName{}
      -> ErrorWithoutFlag
    T.PsInvalidRuleActivationMarker
      -> ErrorWithoutFlag


--
-- Message construction
--

mk_parser_err :: SrcSpan -> SDoc -> MsgEnvelope T.PsMessage
mk_parser_err span doc = MsgEnvelope
   { errMsgSpan        = span
   , errMsgContext     = alwaysQualify
   , errMsgDiagnostic  = T.PsUnknownMessage mempty (DiagnosticMessage (mkDecorated [doc]) ErrorWithoutFlag)
   , errMsgSeverity    = SevError
   }

mk_parser_warn :: DynFlags -> WarningFlag -> SrcSpan -> SDoc -> MsgEnvelope T.PsMessage
mk_parser_warn df flag span doc = MsgEnvelope
   { errMsgSpan        = span
   , errMsgContext     = alwaysQualify
   , errMsgDiagnostic  = T.PsUnknownMessage mempty (DiagnosticMessage (mkDecorated [doc]) reason)
   , errMsgSeverity    = diagReasonSeverity df reason
   }
  where
    reason :: DiagnosticReason
    reason = WarningWithFlag flag

mkParserWarn :: DynFlags -> PsWarning -> MsgEnvelope T.PsMessage
mkParserWarn df = \case
   PsWarnTab loc tc
      -> mk_parser_warn df Opt_WarnTabs loc $
          text "Tab character found here"
            <> (if tc == 1
                then text ""
                else text ", and in" <+> speakNOf (fromIntegral (tc - 1)) (text "further location"))
            <> text "."
            $+$ text "Please use spaces instead."

   PsWarnTransitionalLayout loc reason
      -> mk_parser_warn df Opt_WarnAlternativeLayoutRuleTransitional loc $
            text "transitional layout will not be accepted in the future:"
            $$ text (case reason of
               TransLayout_Where -> "`where' clause at the same depth as implicit layout block"
               TransLayout_Pipe  -> "`|' at the same depth as implicit layout block"
            )

   PsWarnUnrecognisedPragma loc
      -> mk_parser_warn df Opt_WarnUnrecognisedPragmas loc $
            text "Unrecognised pragma"

   PsWarnHaddockInvalidPos loc
      -> mk_parser_warn df Opt_WarnInvalidHaddock loc $
            text "A Haddock comment cannot appear in this position and will be ignored."

   PsWarnHaddockIgnoreMulti loc
      -> mk_parser_warn df Opt_WarnInvalidHaddock loc $
            text "Multiple Haddock comments for a single entity are not allowed." $$
            text "The extraneous comment will be ignored."

   PsWarnStarBinder loc
      -> mk_parser_warn df Opt_WarnStarBinder loc $
            text "Found binding occurrence of" <+> quotes (text "*")
            <+> text "yet StarIsType is enabled."
         $$ text "NB. To use (or export) this operator in"
            <+> text "modules with StarIsType,"
         $$ text "    including the definition module, you must qualify it."

   PsWarnStarIsType loc
      -> mk_parser_warn df Opt_WarnStarIsType loc $
             text "Using" <+> quotes (text "*")
             <+> text "(or its Unicode variant) to mean"
             <+> quotes (text "Data.Kind.Type")
          $$ text "relies on the StarIsType extension, which will become"
          $$ text "deprecated in the future."
          $$ text "Suggested fix: use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."

   PsWarnImportPreQualified loc
      -> mk_parser_warn df Opt_WarnPrepositiveQualifiedModule loc $
            text "Found" <+> quotes (text "qualified")
             <+> text "in prepositive position"
         $$ text "Suggested fix: place " <+> quotes (text "qualified")
             <+> text "after the module name instead."
         $$ text "To allow this, enable language extension 'ImportQualifiedPost'"

   PsWarnOperatorWhitespaceExtConflict loc sym
      -> mk_parser_warn df Opt_WarnOperatorWhitespaceExtConflict loc $
         let mk_prefix_msg operator_symbol extension_name syntax_meaning =
                  text "The prefix use of a" <+> quotes (text operator_symbol)
                    <+> text "would denote" <+> text syntax_meaning
               $$ nest 2 (text "were the" <+> text extension_name <+> text "extension enabled.")
               $$ text "Suggested fix: add whitespace after the"
                    <+> quotes (text operator_symbol) <> char '.'
         in
         case sym of
           OperatorWhitespaceSymbol_PrefixPercent -> mk_prefix_msg "%" "LinearTypes" "a multiplicity annotation"
           OperatorWhitespaceSymbol_PrefixDollar -> mk_prefix_msg "$" "TemplateHaskell" "an untyped splice"
           OperatorWhitespaceSymbol_PrefixDollarDollar -> mk_prefix_msg "$$" "TemplateHaskell" "a typed splice"


   PsWarnOperatorWhitespace loc sym occ_type
      -> mk_parser_warn df Opt_WarnOperatorWhitespace loc $
         let mk_msg occ_type_str =
                  text "The" <+> text occ_type_str <+> text "use of a" <+> quotes (ftext sym)
                    <+> text "might be repurposed as special syntax"
               $$ nest 2 (text "by a future language extension.")
               $$ text "Suggested fix: add whitespace around it."
         in
         case occ_type of
           OperatorWhitespaceOccurrence_Prefix -> mk_msg "prefix"
           OperatorWhitespaceOccurrence_Suffix -> mk_msg "suffix"
           OperatorWhitespaceOccurrence_TightInfix -> mk_msg "tight infix"

mkParserErr :: PsError -> MsgEnvelope T.PsMessage
mkParserErr err = mk_parser_err (errLoc err) $
                  pprPsError (errDesc err) (errHints err)

-- | Render a 'PsErrorDesc' into an 'SDoc', with its 'PsHint's.
pprPsError :: PsErrorDesc -> [PsHint] -> SDoc
pprPsError desc hints = vcat (pp_err desc : map pp_hint hints)

pp_err :: PsErrorDesc -> SDoc
pp_err = \case
   PsErrLambdaCase
      -> text "Illegal lambda-case (use LambdaCase)"

   PsErrNumUnderscores reason
      -> text $ case reason of
            NumUnderscore_Integral -> "Use NumericUnderscores to allow underscores in integer literals"
            NumUnderscore_Float    -> "Use NumericUnderscores to allow underscores in floating literals"

   PsErrPrimStringInvalidChar
      -> text "primitive string literal must contain only characters <= \'\\xFF\'"

   PsErrMissingBlock
      -> text "Missing block"

   PsErrLexer err kind
      -> hcat
         [ text $ case err of
            LexError               -> "lexical error"
            LexUnknownPragma       -> "unknown pragma"
            LexErrorInPragma       -> "lexical error in pragma"
            LexNumEscapeRange      -> "numeric escape sequence out of range"
            LexStringCharLit       -> "lexical error in string/character literal"
            LexStringCharLitEOF    -> "unexpected end-of-file in string/character literal"
            LexUnterminatedComment -> "unterminated `{-'"
            LexUnterminatedOptions -> "unterminated OPTIONS pragma"
            LexUnterminatedQQ      -> "unterminated quasiquotation"


         , text $ case kind of
            LexErrKind_EOF    -> " at end of input"
            LexErrKind_UTF8   -> " (UTF-8 decoding error)"
            LexErrKind_Char c -> " at character " ++ show c
         ]

   PsErrSuffixAT
      -> text "Suffix occurrence of @. For an as-pattern, remove the leading whitespace."

   PsErrParse token
      | null token
      -> text "parse error (possibly incorrect indentation or mismatched brackets)"

      | otherwise
      -> text "parse error on input" <+> quotes (text token)

   PsErrCmmLexer
      -> text "Cmm lexical error"

   PsErrUnsupportedBoxedSumExpr s
      -> hang (text "Boxed sums not supported:") 2
              (pprSumOrTuple Boxed s)

   PsErrUnsupportedBoxedSumPat s
      -> hang (text "Boxed sums not supported:") 2
              (pprSumOrTuple Boxed s)

   PsErrUnexpectedQualifiedConstructor v
      -> hang (text "Expected an unqualified type constructor:") 2
              (ppr v)

   PsErrTupleSectionInPat
      -> text "Tuple section in pattern context"

   PsErrIllegalBangPattern e
      -> text "Illegal bang-pattern (use BangPatterns):" $$ ppr e

   PsErrOpFewArgs (StarIsType star_is_type) op
      -> text "Operator applied to too few arguments:" <+> ppr op
         $$ starInfo star_is_type op

   PsErrImportQualifiedTwice
      -> text "Multiple occurrences of 'qualified'"

   PsErrImportPostQualified
      -> text "Found" <+> quotes (text "qualified")
          <+> text "in postpositive position. "
         $$ text "To allow this, enable language extension 'ImportQualifiedPost'"

   PsErrIllegalExplicitNamespace
      -> text "Illegal keyword 'type' (use ExplicitNamespaces to enable)"

   PsErrVarForTyCon name
      -> text "Expecting a type constructor but found a variable,"
           <+> quotes (ppr name) <> text "."
         $$ if isSymOcc $ rdrNameOcc name
            then text "If" <+> quotes (ppr name) <+> text "is a type constructor"
                  <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
            else empty

   PsErrIllegalPatSynExport
      -> text "Illegal export form (use PatternSynonyms to enable)"

   PsErrMalformedEntityString
      -> text "Malformed entity string"

   PsErrDotsInRecordUpdate
      -> text "You cannot use `..' in a record update"

   PsErrPrecedenceOutOfRange i
      -> text "Precedence out of range: " <> int i

   PsErrOverloadedRecordDotInvalid
      -> text "Use of OverloadedRecordDot '.' not valid ('.' isn't allowed when constructing records or in record patterns)"

   PsErrOverloadedRecordUpdateNoQualifiedFields
      -> text "Fields cannot be qualified when OverloadedRecordUpdate is enabled"

   PsErrOverloadedRecordUpdateNotEnabled
      -> text "OverloadedRecordUpdate needs to be enabled"

   PsErrInvalidDataCon t
      -> hang (text "Cannot parse data constructor in a data/newtype declaration:") 2
              (ppr t)

   PsErrInvalidInfixDataCon lhs tc rhs
      -> hang (text "Cannot parse an infix data constructor in a data/newtype declaration:")
            2 (ppr lhs <+> ppr tc <+> ppr rhs)

   PsErrUnpackDataCon
      -> text "{-# UNPACK #-} cannot be applied to a data constructor."

   PsErrUnexpectedKindAppInDataCon lhs ki
      -> hang (text "Unexpected kind application in a data/newtype declaration:") 2
              (ppr lhs <+> text "@" <> ppr ki)

   PsErrInvalidRecordCon p
      -> text "Not a record constructor:" <+> ppr p

   PsErrIllegalUnboxedStringInPat lit
      -> text "Illegal unboxed string literal in pattern:" $$ ppr lit

   PsErrDoNotationInPat
      -> text "do-notation in pattern"

   PsErrIfTheElseInPat
      -> text "(if ... then ... else ...)-syntax in pattern"

   PsErrLambdaCaseInPat
      -> text "(\\case ...)-syntax in pattern"

   PsErrCaseInPat
      -> text "(case ... of ...)-syntax in pattern"

   PsErrLetInPat
      -> text "(let ... in ...)-syntax in pattern"

   PsErrLambdaInPat
      -> text "Lambda-syntax in pattern."
         $$ text "Pattern matching on functions is not possible."

   PsErrArrowExprInPat e
      -> text "Expression syntax in pattern:" <+> ppr e

   PsErrArrowCmdInPat c
      -> text "Command syntax in pattern:" <+> ppr c

   PsErrArrowCmdInExpr c
      -> vcat
         [ text "Arrow command found where an expression was expected:"
         , nest 2 (ppr c)
         ]

   PsErrViewPatInExpr a b
      -> sep [ text "View pattern in expression context:"
             , nest 4 (ppr a <+> text "->" <+> ppr b)
             ]

   PsErrTypeAppWithoutSpace v e
      -> sep [ text "@-pattern in expression context:"
             , nest 4 (pprPrefixOcc v <> text "@" <> ppr e)
             ]
         $$ text "Type application syntax requires a space before '@'"


   PsErrLazyPatWithoutSpace e
      -> sep [ text "Lazy pattern in expression context:"
             , nest 4 (text "~" <> ppr e)
             ]
         $$ text "Did you mean to add a space after the '~'?"

   PsErrBangPatWithoutSpace e
      -> sep [ text "Bang pattern in expression context:"
             , nest 4 (text "!" <> ppr e)
             ]
         $$ text "Did you mean to add a space after the '!'?"

   PsErrUnallowedPragma prag
      -> hang (text "A pragma is not allowed in this position:") 2
              (ppr prag)

   PsErrQualifiedDoInCmd m
      -> hang (text "Parse error in command:") 2 $
            text "Found a qualified" <+> ppr m <> text ".do block in a command, but"
            $$ text "qualified 'do' is not supported in commands."

   PsErrParseErrorInCmd s
      -> hang (text "Parse error in command:") 2 s

   PsErrParseErrorInPat s
      -> text "Parse error in pattern:" <+> s


   PsErrInvalidInfixHole
      -> text "Invalid infix hole, expected an infix operator"

   PsErrSemiColonsInCondExpr c st t se e
      -> text "Unexpected semi-colons in conditional:"
         $$ nest 4 expr
         $$ text "Perhaps you meant to use DoAndIfThenElse?"
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e

   PsErrSemiColonsInCondCmd c st t se e
      -> text "Unexpected semi-colons in conditional:"
         $$ nest 4 expr
         $$ text "Perhaps you meant to use DoAndIfThenElse?"
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e


   PsErrAtInPatPos
      -> text "Found a binding for the"
         <+> quotes (text "@")
         <+> text "operator in a pattern position."
         $$ perhaps_as_pat

   PsErrLambdaCmdInFunAppCmd a
      -> pp_unexpected_fun_app (text "lambda command") a

   PsErrCaseCmdInFunAppCmd a
      -> pp_unexpected_fun_app (text "case command") a

   PsErrIfCmdInFunAppCmd a
      -> pp_unexpected_fun_app (text "if command") a

   PsErrLetCmdInFunAppCmd a
      -> pp_unexpected_fun_app (text "let command") a

   PsErrDoCmdInFunAppCmd a
      -> pp_unexpected_fun_app (text "do command") a

   PsErrDoInFunAppExpr m a
      -> pp_unexpected_fun_app (prependQualified m (text "do block")) a

   PsErrMDoInFunAppExpr m a
      -> pp_unexpected_fun_app (prependQualified m (text "mdo block")) a

   PsErrLambdaInFunAppExpr a
      -> pp_unexpected_fun_app (text "lambda expression") a

   PsErrCaseInFunAppExpr a
      -> pp_unexpected_fun_app (text "case expression") a

   PsErrLambdaCaseInFunAppExpr a
      -> pp_unexpected_fun_app (text "lambda-case expression") a

   PsErrLetInFunAppExpr a
      -> pp_unexpected_fun_app (text "let expression") a

   PsErrIfInFunAppExpr a
      -> pp_unexpected_fun_app (text "if expression") a

   PsErrProcInFunAppExpr a
      -> pp_unexpected_fun_app (text "proc expression") a

   PsErrMalformedTyOrClDecl ty
      -> text "Malformed head of type or class declaration:"
         <+> ppr ty

   PsErrIllegalWhereInDataDecl
      -> vcat
            [ text "Illegal keyword 'where' in data declaration"
            , text "Perhaps you intended to use GADTs or a similar language"
            , text "extension to enable syntax: data T where"
            ]

   PsErrIllegalTraditionalRecordSyntax s
      -> text "Illegal record syntax (use TraditionalRecordSyntax):"
         <+> s

   PsErrParseErrorOnInput occ
      -> text "parse error on input" <+> ftext (occNameFS occ)

   PsErrIllegalDataTypeContext c
      -> text "Illegal datatype context (use DatatypeContexts):"
         <+> pprLHsContext (Just c)

   PsErrMalformedDecl what for
      -> text "Malformed" <+> what
         <+> text "declaration for" <+> quotes (ppr for)

   PsErrUnexpectedTypeAppInDecl ki what for
      -> vcat [ text "Unexpected type application"
                <+> text "@" <> ppr ki
              , text "In the" <+> what
                <+> text "declaration for"
                <+> quotes (ppr for)
              ]

   PsErrNotADataCon name
      -> text "Not a data constructor:" <+> quotes (ppr name)

   PsErrRecordSyntaxInPatSynDecl pat
      -> text "record syntax not supported for pattern synonym declarations:"
         $$ ppr pat

   PsErrEmptyWhereInPatSynDecl patsyn_name
      -> text "pattern synonym 'where' clause cannot be empty"
         $$ text "In the pattern synonym declaration for: "
            <+> ppr (patsyn_name)

   PsErrInvalidWhereBindInPatSynDecl patsyn_name decl
      -> text "pattern synonym 'where' clause must bind the pattern synonym's name"
         <+> quotes (ppr patsyn_name) $$ ppr decl

   PsErrNoSingleWhereBindInPatSynDecl _patsyn_name decl
      -> text "pattern synonym 'where' clause must contain a single binding:"
         $$ ppr decl

   PsErrDeclSpliceNotAtTopLevel d
      -> hang (text "Declaration splices are allowed only"
               <+> text "at the top level:")
           2 (ppr d)

   PsErrInferredTypeVarNotAllowed
      -> text "Inferred type variables are not allowed here"

   PsErrIllegalRoleName role nearby
      -> text "Illegal role name" <+> quotes (ppr role)
         $$ case nearby of
             []  -> empty
             [r] -> text "Perhaps you meant" <+> quotes (ppr r)
             -- will this last case ever happen??
             _   -> hang (text "Perhaps you meant one of these:")
                         2 (pprWithCommas (quotes . ppr) nearby)

   PsErrMultipleNamesInStandaloneKindSignature vs
      -> vcat [ hang (text "Standalone kind signatures do not support multiple names at the moment:")
                2 (pprWithCommas ppr vs)
              , text "See https://gitlab.haskell.org/ghc/ghc/issues/16754 for details."
              ]

   PsErrIllegalImportBundleForm
      -> text "Illegal import form, this syntax can only be used to bundle"
         $+$ text "pattern synonyms with types in module exports."

   PsErrInvalidTypeSignature lhs
      -> text "Invalid type signature:"
         <+> ppr lhs
         <+> text ":: ..."
         $$ text hint
         where
         hint | foreign_RDR `looks_like` lhs
              = "Perhaps you meant to use ForeignFunctionInterface?"
              | default_RDR `looks_like` lhs
              = "Perhaps you meant to use DefaultSignatures?"
              | pattern_RDR `looks_like` lhs
              = "Perhaps you meant to use PatternSynonyms?"
              | otherwise
              = "Should be of form <variable> :: <type>"

         -- A common error is to forget the ForeignFunctionInterface flag
         -- so check for that, and suggest.  cf #3805
         -- Sadly 'foreign import' still barfs 'parse error' because
         --  'import' is a keyword
         -- looks_like :: RdrName -> LHsExpr GhcPs -> Bool -- AZ
         looks_like s (L _ (HsVar _ (L _ v))) = v == s
         looks_like s (L _ (HsApp _ lhs _))   = looks_like s lhs
         looks_like _ _                       = False

         foreign_RDR = mkUnqual varName (fsLit "foreign")
         default_RDR = mkUnqual varName (fsLit "default")
         pattern_RDR = mkUnqual varName (fsLit "pattern")

   PsErrUnexpectedTypeInDecl t what tc tparms equals_or_where
      -> vcat [ text "Unexpected type" <+> quotes (ppr t)
              , text "In the" <+> what
                <+> ptext (sLit "declaration for") <+> quotes tc'
              , vcat[ (text "A" <+> what
                       <+> ptext (sLit "declaration should have form"))
              , nest 2
                (what
                 <+> tc'
                 <+> hsep (map text (takeList tparms allNameStrings))
                 <+> equals_or_where) ] ]
          where
            -- Avoid printing a constraint tuple in the error message. Print
            -- a plain old tuple instead (since that's what the user probably
            -- wrote). See #14907
            tc' = ppr $ filterCTuple tc

   PsErrCmmParser cmm_err -> case cmm_err of
      CmmUnknownPrimitive name     -> text "unknown primitive" <+> ftext name
      CmmUnknownMacro fun          -> text "unknown macro" <+> ftext fun
      CmmUnknownCConv cconv        -> text "unknown calling convention:" <+> text cconv
      CmmUnrecognisedSafety safety -> text "unrecognised safety" <+> text safety
      CmmUnrecognisedHint hint     -> text "unrecognised hint:" <+> text hint

   PsErrExpectedHyphen
      -> text "Expected a hyphen"

   PsErrSpaceInSCC
      -> text "Spaces are not allowed in SCCs"

   PsErrEmptyDoubleQuotes th_on
      -> if th_on then vcat (msg ++ th_msg) else vcat msg
         where
            msg    = [ text "Parser error on `''`"
                     , text "Character literals may not be empty"
                     ]
            th_msg = [ text "Or perhaps you intended to use quotation syntax of TemplateHaskell,"
                     , text "but the type variable or constructor is missing"
                     ]

   PsErrInvalidPackageName pkg
      -> vcat
            [ text "Parse error" <> colon <+> quotes (ftext pkg)
            , text "Version number or non-alphanumeric" <+>
              text "character in package name"
            ]

   PsErrInvalidRuleActivationMarker
      -> text "Invalid rule activation marker"

   PsErrLinearFunction
      -> text "Enable LinearTypes to allow linear functions"

   PsErrMultiWayIf
      -> text "Multi-way if-expressions need MultiWayIf turned on"

   PsErrExplicitForall is_unicode
      -> vcat
         [ text "Illegal symbol" <+> quotes (forallSym is_unicode) <+> text "in type"
         , text "Perhaps you intended to use RankNTypes or a similar language"
         , text "extension to enable explicit-forall syntax:" <+>
           forallSym is_unicode <+> text "<tvs>. <type>"
         ]
         where
          forallSym True  = text "∀"
          forallSym False = text "forall"

   PsErrIllegalQualifiedDo qdoDoc
      -> vcat
         [ text "Illegal qualified" <+> quotes qdoDoc <+> text "block"
         , text "Perhaps you intended to use QualifiedDo"
         ]

pp_unexpected_fun_app :: Outputable a => SDoc -> a -> SDoc
pp_unexpected_fun_app e a =
   text "Unexpected " <> e <> text " in function application:"
    $$ nest 4 (ppr a)
    $$ text "You could write it with parentheses"
    $$ text "Or perhaps you meant to enable BlockArguments?"

pp_hint :: T.PsHint -> SDoc
pp_hint = \case
   SuggestExtension ext   -> text "Perhaps you intended to use" <+> ppr ext
   SuggestDo              -> text "Perhaps this statement should be within a 'do' block?"
   SuggestMissingDo       -> text "Possibly caused by a missing 'do'?"
   SuggestLetInDo         -> text "Perhaps you need a 'let' in a 'do' block?"
                             $$ text "e.g. 'let x = 5' instead of 'x = 5'"

   SuggestInfixBindMaybeAtPat fun
      -> text "In a function binding for the"
            <+> quotes (ppr fun)
            <+> text "operator."
         $$ if opIsAt fun
               then perhaps_as_pat
               else empty
   TypeApplicationsInPatternsOnlyDataCons ->
     text "Type applications in patterns are only allowed on data constructors."
   UseSpaces
     -> text "Please use spaces instead."

perhaps_as_pat :: SDoc
perhaps_as_pat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"
