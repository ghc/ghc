{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic PsMessage

module GHC.Parser.Errors.Ppr
   ( mkParserMessage
   , mkParserErrorMessage
   )
where

import GHC.Prelude
import GHC.Driver.Flags
import GHC.Parser.Errors.Types
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
import GHC.Utils.Error (mkMsgEnvelope, mkErrorMsgEnvelope)
import qualified GHC.LanguageExtensions as Ext

--
-- Diagnostic instances
--

instance Diagnostic PsMessage where
  diagnosticMessage = \case
    PsUnknownMessage _hints m
      -> diagnosticMessage m
    PsLayoutMessage layoutDiagnostic
      -> diagnosticMessage layoutDiagnostic
    PsHaddockMessage haddockDiagnostic
      -> diagnosticMessage haddockDiagnostic
    PsExtensionMessage extDiagnostic
      -> diagnosticMessage extDiagnostic
    PsImportOrPragmaMessage impDiagnostic
      -> diagnosticMessage impDiagnostic
    PsLexerError err kind
      -> mkSimpleDecorated $ hcat
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
    PsParseError token hints
      | null token
      -> mkSimpleDecorated $
           vcat ( text "parse error (possibly incorrect indentation or mismatched brackets)"
                : map pp_hint hints )
      | otherwise
      -> mkSimpleDecorated $
           vcat ( text "parse error on input" <+> quotes (text token)
                : map pp_hint hints)
    PsCmmLexerError
      -> mkSimpleDecorated $ text "Cmm lexical error"
    PsCmmParserError cmm_err -> mkSimpleDecorated $ case cmm_err of
      CmmUnknownPrimitive name     -> text "unknown primitive" <+> ftext name
      CmmUnknownMacro fun          -> text "unknown macro" <+> ftext fun
      CmmUnknownCConv cconv        -> text "unknown calling convention:" <+> text cconv
      CmmUnrecognisedSafety safety -> text "unrecognised safety" <+> text safety
      CmmUnrecognisedHint hint     -> text "unrecognised hint:" <+> text hint
    PsMalformedExprMessage malExpr
      -> diagnosticMessage malExpr
  diagnosticReason  = \case
    PsUnknownMessage _hints m
      -> diagnosticReason m
    PsLayoutMessage layoutDiagnostic
      -> diagnosticReason layoutDiagnostic
    PsHaddockMessage haddockDiagnostic
      -> diagnosticReason haddockDiagnostic
    PsExtensionMessage extDiagnostic
      -> diagnosticReason extDiagnostic
    PsImportOrPragmaMessage impDiagnostic
      -> diagnosticReason impDiagnostic
    PsLexerError{}
      -> ErrorWithoutFlag
    PsParseError{}
      -> ErrorWithoutFlag
    PsCmmLexerError
      -> ErrorWithoutFlag
    PsCmmParserError{}
      -> ErrorWithoutFlag
    PsMalformedExprMessage malExpr
      -> diagnosticReason malExpr


instance Diagnostic PsLayoutMessage where
  diagnosticMessage = \case
    PsTabulationsFound hint tc
      -> mkSimpleDecorated $
           text "Tab character found here"
             <> (if tc == 1
                 then text ""
                 else text ", and in" <+> speakNOf (fromIntegral (tc - 1)) (text "further location"))
             <> text "."
             $+$ pp_hint hint
    PsTransitionalLayout reason
      -> mkSimpleDecorated $
            text "transitional layout will not be accepted in the future:"
            $$ text (case reason of
               TransLayout_Where -> "`where' clause at the same depth as implicit layout block"
               TransLayout_Pipe  -> "`|' at the same depth as implicit layout block"
            )
    PsOperatorWhitespaceExtConflict sym
      -> let mk_prefix_msg operator_symbol extension_name syntax_meaning =
                  text "The prefix use of a" <+> quotes (text operator_symbol)
                    <+> text "would denote" <+> text syntax_meaning
               $$ nest 2 (text "were the" <+> text extension_name <+> text "extension enabled.")
               $$ text "Suggested fix: add whitespace after the"
                    <+> quotes (text operator_symbol) <> char '.'
         in mkSimpleDecorated $
         case sym of
           OperatorWhitespaceSymbol_PrefixPercent -> mk_prefix_msg "%" "LinearTypes" "a multiplicity annotation"
           OperatorWhitespaceSymbol_PrefixDollar -> mk_prefix_msg "$" "TemplateHaskell" "an untyped splice"
           OperatorWhitespaceSymbol_PrefixDollarDollar -> mk_prefix_msg "$$" "TemplateHaskell" "a typed splice"

    PsOperatorWhitespace sym occ_type
      -> let mk_msg occ_type_str =
                  text "The" <+> text occ_type_str <+> text "use of a" <+> quotes (ftext sym)
                    <+> text "might be repurposed as special syntax"
               $$ nest 2 (text "by a future language extension.")
               $$ text "Suggested fix: add whitespace around it."
         in mkSimpleDecorated $
         case occ_type of
           OperatorWhitespaceOccurrence_Prefix -> mk_msg "prefix"
           OperatorWhitespaceOccurrence_Suffix -> mk_msg "suffix"
           OperatorWhitespaceOccurrence_TightInfix -> mk_msg "tight infix"
    PsTypeAppWithoutSpace v e
      -> mkSimpleDecorated $
           sep [ text "@-pattern in expression context:"
               , nest 4 (pprPrefixOcc v <> text "@" <> ppr e)
               ]
           $$ text "Type application syntax requires a space before '@'"
    PsLazyPatWithoutSpace e
      -> mkSimpleDecorated $
           sep [ text "Lazy pattern in expression context:"
               , nest 4 (text "~" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '~'?"
    PsBangPatWithoutSpace e
      -> mkSimpleDecorated $
           sep [ text "Bang pattern in expression context:"
               , nest 4 (text "!" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '!'?"
    PsInvalidInfixHole
      -> mkSimpleDecorated $ text "Invalid infix hole, expected an infix operator"
    PsExpectedHyphen
      -> mkSimpleDecorated $ text "Expected a hyphen"
    PsSpaceInSCC
      -> mkSimpleDecorated $ text "Spaces are not allowed in SCCs"
    PsEmptyDoubleQuotes th_on
      -> mkSimpleDecorated $ if th_on then vcat (msg ++ th_msg) else vcat msg
         where
            msg    = [ text "Parser error on `''`"
                     , text "Character literals may not be empty"
                     ]
            th_msg = [ text "Or perhaps you intended to use quotation syntax of TemplateHaskell,"
                     , text "but the type variable or constructor is missing"
                     ]
  diagnosticReason = \case
    PsTabulationsFound{}
      -> WarningWithFlag Opt_WarnTabs
    PsTransitionalLayout{}
      -> WarningWithFlag Opt_WarnAlternativeLayoutRuleTransitional
    PsOperatorWhitespaceExtConflict{}
      -> WarningWithFlag Opt_WarnOperatorWhitespaceExtConflict
    PsOperatorWhitespace{}
      -> WarningWithFlag Opt_WarnOperatorWhitespace
    PsTypeAppWithoutSpace{}
      -> ErrorWithoutFlag
    PsLazyPatWithoutSpace{}
      -> ErrorWithoutFlag
    PsBangPatWithoutSpace{}
      -> ErrorWithoutFlag
    PsInvalidInfixHole
      -> ErrorWithoutFlag
    PsExpectedHyphen
      -> ErrorWithoutFlag
    PsSpaceInSCC
      -> ErrorWithoutFlag
    PsEmptyDoubleQuotes{}
      -> ErrorWithoutFlag


instance Diagnostic PsHaddockMessage where
  diagnosticMessage = \case
   PsHaddockInvalidPosition
      -> mkSimpleDecorated $ text "A Haddock comment cannot appear in this position and will be ignored."
   PsHaddockIgnoreMultipleComments
      -> mkSimpleDecorated $
           text "Multiple Haddock comments for a single entity are not allowed." $$
           text "The extraneous comment will be ignored."
  diagnosticReason = \case
    PsHaddockInvalidPosition
      -> WarningWithFlag Opt_WarnInvalidHaddock
    PsHaddockIgnoreMultipleComments
      -> WarningWithFlag Opt_WarnInvalidHaddock


instance Diagnostic PsExtensionMessage where
  diagnosticMessage = \case
    PsSyntaxUsedButNotEnabled Ext.LambdaCase _
      -> mkSimpleDecorated $ text "Illegal lambda-case (use LambdaCase)"
    PsSyntaxUsedButNotEnabled Ext.LinearTypes _
      -> mkSimpleDecorated $ text "Enable LinearTypes to allow linear functions"
    PsSyntaxUsedButNotEnabled Ext.OverloadedRecordUpdate _
      -> mkSimpleDecorated $ text "OverloadedRecordUpdate needs to be enabled"
    PsSyntaxUsedButNotEnabled Ext.MultiWayIf _
      -> mkSimpleDecorated $ text "Multi-way if-expressions need MultiWayIf turned on"
    PsSyntaxUsedButNotEnabled ext hint
      -> mkSimpleDecorated $
           text "The" <+> ppr ext <+> text "syntax was used, but the extension was not enabled."
                       $$ pp_hint hint
    PsStarBinderWithStarIsType
      -> mkSimpleDecorated $
            text "Found binding occurrence of" <+> quotes (text "*")
            <+> text "yet StarIsType is enabled."
         $$ text "NB. To use (or export) this operator in"
            <+> text "modules with StarIsType,"
         $$ text "    including the definition module, you must qualify it."
    PsStarIsTypeWithoutExtension
      -> mkSimpleDecorated $
             text "Using" <+> quotes (text "*")
             <+> text "(or its Unicode variant) to mean"
             <+> quotes (text "Data.Kind.Type")
          $$ text "relies on the StarIsType extension, which will become"
          $$ text "deprecated in the future."
          $$ text "Suggested fix: use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."
    PsNumUnderscoresWithoutExtension reason
      -> mkSimpleDecorated $
           text $ case reason of
             NumUnderscore_Integral -> "Use NumericUnderscores to allow underscores in integer literals"
             NumUnderscore_Float    -> "Use NumericUnderscores to allow underscores in floating literals"
    PsIllegalBangPattern e
      -> mkSimpleDecorated $ text "Illegal bang-pattern (use BangPatterns):" $$ ppr e
    PsOverloadedRecordDotInvalid
      -> mkSimpleDecorated $
           text "Use of OverloadedRecordDot '.' not valid ('.' isn't allowed when constructing records or in record patterns)"
    PsIllegalPatSynExport
      -> mkSimpleDecorated $ text "Illegal export form (use PatternSynonyms to enable)"
    PsOverloadedRecordUpdateNoQualifiedFields
      -> mkSimpleDecorated $ text "Fields cannot be qualified when OverloadedRecordUpdate is enabled"
    PsExplicitForallWithoutExtension is_unicode
      -> mkSimpleDecorated $ vcat
           [ text "Illegal symbol" <+> quotes (forallSym is_unicode) <+> text "in type"
           , text "Perhaps you intended to use RankNTypes or a similar language"
           , text "extension to enable explicit-forall syntax:" <+>
             forallSym is_unicode <+> text "<tvs>. <type>"
           ]
         where
          forallSym True  = text "∀"
          forallSym False = text "forall"
    PsIllegalQualifiedDoWithoutExtension qdoDoc
      -> mkSimpleDecorated $ vcat
           [ text "Illegal qualified" <+> quotes qdoDoc <+> text "block"
           , text "Perhaps you intended to use QualifiedDo"
           ]
    PsQualifiedDoInCmd m
      -> mkSimpleDecorated $
           hang (text "Parse error in command:") 2 $
             text "Found a qualified" <+> ppr m <> text ".do block in a command, but"
             $$ text "qualified 'do' is not supported in commands."
    PsRecordSyntaxInPatSynDecl pat
      -> mkSimpleDecorated $
           text "record syntax not supported for pattern synonym declarations:"
           $$ ppr pat
    PsEmptyWhereInPatSynDecl patsyn_name
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause cannot be empty"
           $$ text "In the pattern synonym declaration for: "
              <+> ppr (patsyn_name)
    PsInvalidWhereBindInPatSynDecl patsyn_name decl
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause must bind the pattern synonym's name"
           <+> quotes (ppr patsyn_name) $$ ppr decl
    PsNoSingleWhereBindInPatSynDecl _patsyn_name decl
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause must contain a single binding:"
           $$ ppr decl
    PsDeclSpliceNotAtTopLevel d
      -> mkSimpleDecorated $
           hang (text "Declaration splices are allowed only"
                 <+> text "at the top level:")
             2 (ppr d)
    PsMultipleNamesInStandaloneKindSignature vs
      -> mkSimpleDecorated $
           vcat [ hang (text "Standalone kind signatures do not support multiple names at the moment:")
                  2 (pprWithCommas ppr vs)
                , text "See https://gitlab.haskell.org/ghc/ghc/issues/16754 for details."
                ]
    PsIllegalExplicitNamespace
      -> mkSimpleDecorated $
           text "Illegal keyword 'type' (use ExplicitNamespaces to enable)"
  diagnosticReason = \case
    PsStarBinderWithStarIsType
      -> WarningWithFlag Opt_WarnStarBinder
    PsStarIsTypeWithoutExtension
      -> WarningWithFlag Opt_WarnStarIsType
    PsSyntaxUsedButNotEnabled{}
      -> ErrorWithoutFlag
    PsNumUnderscoresWithoutExtension{}
      -> ErrorWithoutFlag
    PsIllegalBangPattern{}
      -> ErrorWithoutFlag
    PsOverloadedRecordDotInvalid{}
      -> ErrorWithoutFlag
    PsIllegalPatSynExport
      -> ErrorWithoutFlag
    PsOverloadedRecordUpdateNoQualifiedFields
      -> ErrorWithoutFlag
    PsExplicitForallWithoutExtension{}
      -> ErrorWithoutFlag
    PsIllegalQualifiedDoWithoutExtension{}
      -> ErrorWithoutFlag
    PsQualifiedDoInCmd{}
      -> ErrorWithoutFlag
    PsRecordSyntaxInPatSynDecl{}
      -> ErrorWithoutFlag
    PsEmptyWhereInPatSynDecl{}
      -> ErrorWithoutFlag
    PsInvalidWhereBindInPatSynDecl{}
      -> ErrorWithoutFlag
    PsNoSingleWhereBindInPatSynDecl{}
      -> ErrorWithoutFlag
    PsDeclSpliceNotAtTopLevel{}
      -> ErrorWithoutFlag
    PsMultipleNamesInStandaloneKindSignature{}
      -> ErrorWithoutFlag
    PsIllegalExplicitNamespace
      -> ErrorWithoutFlag


instance Diagnostic PsImportOrPragmaMessage where
  diagnosticMessage = \case
    PsUnrecognisedPragma
      -> mkSimpleDecorated $ text "Unrecognised pragma"
    PsUnallowedPragma prag
      -> mkSimpleDecorated $
           hang (text "A pragma is not allowed in this position:") 2
                (ppr prag)
    PsImportPreQualified
      -> mkSimpleDecorated $
            text "Found" <+> quotes (text "qualified")
             <+> text "in prepositive position"
         $$ text "Suggested fix: place " <+> quotes (text "qualified")
             <+> text "after the module name instead."
         $$ text "To allow this, enable language extension 'ImportQualifiedPost'"
    PsImportPostQualified
      -> mkSimpleDecorated $
           text "Found" <+> quotes (text "qualified")
             <+> text "in postpositive position. "
           $$ text "To allow this, enable language extension 'ImportQualifiedPost'"
    PsImportQualifiedTwice
      -> mkSimpleDecorated $ text "Multiple occurrences of 'qualified'"
    PsIllegalImportBundleForm
      -> mkSimpleDecorated $
           text "Illegal import form, this syntax can only be used to bundle"
           $+$ text "pattern synonyms with types in module exports."
    PsInvalidRuleActivationMarker
      -> mkSimpleDecorated $ text "Invalid rule activation marker"
  diagnosticReason = \case
    PsUnrecognisedPragma
      -> WarningWithFlag Opt_WarnUnrecognisedPragmas
    PsUnallowedPragma{}
      -> ErrorWithoutFlag
    PsImportPreQualified
      -> WarningWithFlag Opt_WarnPrepositiveQualifiedModule
    PsImportPostQualified
      -> ErrorWithoutFlag
    PsImportQualifiedTwice
      -> ErrorWithoutFlag
    PsIllegalImportBundleForm
      -> ErrorWithoutFlag
    PsInvalidRuleActivationMarker
      -> ErrorWithoutFlag


instance Diagnostic PsMalformedExprMessage where
  diagnosticMessage = \case
    PsMissingBlock
      -> mkSimpleDecorated $ text "Missing block"
    PsUnsupportedBoxedSumExpr s
      -> mkSimpleDecorated $
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
    PsUnsupportedBoxedSumPattern s
      -> mkSimpleDecorated $
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
    PsUnexpectedQualifiedConstructor v
      -> mkSimpleDecorated $
           hang (text "Expected an unqualified type constructor:") 2
                (ppr v)
    PsTupleSectionInPattern
      -> mkSimpleDecorated $ text "Tuple section in pattern context"
    PsOpTooFewArgs (StarIsType star_is_type) op
      -> mkSimpleDecorated $
           text "Operator applied to too few arguments:" <+> ppr op
           $$ starInfo star_is_type op
    PsVarForTyCon name
      -> mkSimpleDecorated $
           text "Expecting a type constructor but found a variable,"
             <+> quotes (ppr name) <> text "."
           $$ if isSymOcc $ rdrNameOcc name
              then text "If" <+> quotes (ppr name) <+> text "is a type constructor"
                    <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
              else empty
    PsMalformedEntityString
      -> mkSimpleDecorated $ text "Malformed entity string"
    PsDotsInRecordUpdate
      -> mkSimpleDecorated $ text "You cannot use `..' in a record update"
    PsInvalidDataCon t
      -> mkSimpleDecorated $
           hang (text "Cannot parse data constructor in a data/newtype declaration:") 2
                (ppr t)
    PsInvalidInfixDataCon lhs tc rhs
      -> mkSimpleDecorated $
           hang (text "Cannot parse an infix data constructor in a data/newtype declaration:") 2
                (ppr lhs <+> ppr tc <+> ppr rhs)
    PsUnpackDataCon
      -> mkSimpleDecorated $ text "{-# UNPACK #-} cannot be applied to a data constructor."
    PsUnexpectedKindAppInDataCon lhs ki
      -> mkSimpleDecorated $
           hang (text "Unexpected kind application in a data/newtype declaration:") 2
                (ppr lhs <+> text "@" <> ppr ki)
    PsInvalidRecordCon p
      -> mkSimpleDecorated $ text "Not a record constructor:" <+> ppr p
    PsIllegalUnboxedStringInPat lit
      -> mkSimpleDecorated $ text "Illegal unboxed string literal in pattern:" $$ ppr lit
    PsDoNotationInPat
      -> mkSimpleDecorated $ text "do-notation in pattern"
    PsIfThenElseInPat
      -> mkSimpleDecorated $ text "(if ... then ... else ...)-syntax in pattern"
    PsLambdaCaseInPat
      -> mkSimpleDecorated $ text "(\\case ...)-syntax in pattern"
    PsCaseInPat
      -> mkSimpleDecorated $ text "(case ... of ...)-syntax in pattern"
    PsLetInPat
      -> mkSimpleDecorated $ text "(let ... in ...)-syntax in pattern"
    PsLambdaInPat
      -> mkSimpleDecorated $
           text "Lambda-syntax in pattern."
           $$ text "Pattern matching on functions is not possible."
    PsArrowExprInPat e
      -> mkSimpleDecorated $ text "Expression syntax in pattern:" <+> ppr e
    PsArrowCmdInPat c
      -> mkSimpleDecorated $ text "Command syntax in pattern:" <+> ppr c
    PsArrowCmdInExpr c
      -> mkSimpleDecorated $
           vcat
           [ text "Arrow command found where an expression was expected:"
           , nest 2 (ppr c)
           ]
    PsViewPatInExpr a b
      -> mkSimpleDecorated $
           sep [ text "View pattern in expression context:"
               , nest 4 (ppr a <+> text "->" <+> ppr b)
               ]
    PsLambdaCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "lambda command") a
    PsCaseCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "case command") a
    PsIfCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "if command") a
    PsLetCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "let command") a
    PsDoCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "do command") a
    PsDoInFunAppExpr m a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (prependQualified m (text "do block")) a
    PsMDoInFunAppExpr m a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (prependQualified m (text "mdo block")) a
    PsLambdaInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "lambda expression") a
    PsCaseInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "case expression") a
    PsLambdaCaseInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "lambda-case expression") a
    PsLetInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "let expression") a
    PsIfInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "if expression") a
    PsProcInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "proc expression") a
    PsMalformedTyOrClDecl ty
      -> mkSimpleDecorated $
           text "Malformed head of type or class declaration:" <+> ppr ty
    PsIllegalWhereInDataDecl
      -> mkSimpleDecorated $
           vcat
              [ text "Illegal keyword 'where' in data declaration"
              , text "Perhaps you intended to use GADTs or a similar language"
              , text "extension to enable syntax: data T where"
              ]
    PsIllegalDataTypeContext c
      -> mkSimpleDecorated $
           text "Illegal datatype context (use DatatypeContexts):"
             <+> pprLHsContext (Just c)
    PsPrimStringInvalidChar
      -> mkSimpleDecorated $ text "primitive string literal must contain only characters <= \'\\xFF\'"
    PsSuffixAT
      -> mkSimpleDecorated $
           text "Suffix occurrence of @. For an as-pattern, remove the leading whitespace."
    PsPrecedenceOutOfRange i
      -> mkSimpleDecorated $ text "Precedence out of range: " <> int i
    PsSemiColonsInCondExpr c st t se e
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
    PsSemiColonsInCondCmd c st t se e
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
    PsAtInPatternPosition
      -> mkSimpleDecorated $
           text "Found a binding for the"
           <+> quotes (text "@")
           <+> text "operator in a pattern position."
           $$ perhaps_as_pat
    PsParseErrorOnInput occ
      -> mkSimpleDecorated $ text "parse error on input" <+> ftext (occNameFS occ)
    PsMalformedDecl what for
      -> mkSimpleDecorated $
           text "Malformed" <+> what
           <+> text "declaration for" <+> quotes (ppr for)
    PsUnexpectedTypeAppInDecl ki what for
      -> mkSimpleDecorated $
           vcat [ text "Unexpected type application"
                  <+> text "@" <> ppr ki
                , text "In the" <+> what
                  <+> text "declaration for"
                  <+> quotes (ppr for)
                ]
    PsNotADataCon name
      -> mkSimpleDecorated $ text "Not a data constructor:" <+> quotes (ppr name)
    PsInferredTypeVarNotAllowed
      -> mkSimpleDecorated $ text "Inferred type variables are not allowed here"
    PsIllegalTraditionalRecordSyntax s
      -> mkSimpleDecorated $
           text "Illegal record syntax (use TraditionalRecordSyntax):" <+> s
    PsParseErrorInCmd s
      -> mkSimpleDecorated $ hang (text "Parse error in command:") 2 s
    PsParseErrorInPat s
      -> mkSimpleDecorated $ text "Parse error in pattern:" <+> s
    PsIllegalRoleName role nearby
      -> mkSimpleDecorated $
           text "Illegal role name" <+> quotes (ppr role)
           $$ case nearby of
               []  -> empty
               [r] -> text "Perhaps you meant" <+> quotes (ppr r)
               -- will this last case ever happen??
               _   -> hang (text "Perhaps you meant one of these:")
                           2 (pprWithCommas (quotes . ppr) nearby)
    PsInvalidTypeSignature lhs
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
    PsUnexpectedTypeInDecl t what tc tparms equals_or_where
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
    PsInvalidPackageName pkg
      -> mkSimpleDecorated $ vcat
            [ text "Parse error" <> colon <+> quotes (ftext pkg)
            , text "Version number or non-alphanumeric" <+>
              text "character in package name"
            ]
  diagnosticReason  = \case
    PsMissingBlock
      -> ErrorWithoutFlag
    PsUnsupportedBoxedSumExpr{}
      -> ErrorWithoutFlag
    PsUnsupportedBoxedSumPattern{}
      -> ErrorWithoutFlag
    PsUnexpectedQualifiedConstructor{}
      -> ErrorWithoutFlag
    PsTupleSectionInPattern{}
      -> ErrorWithoutFlag
    PsOpTooFewArgs{}
      -> ErrorWithoutFlag
    PsVarForTyCon{}
      -> ErrorWithoutFlag
    PsMalformedEntityString
      -> ErrorWithoutFlag
    PsDotsInRecordUpdate
      -> ErrorWithoutFlag
    PsInvalidDataCon{}
      -> ErrorWithoutFlag
    PsInvalidInfixDataCon{}
      -> ErrorWithoutFlag
    PsUnpackDataCon
      -> ErrorWithoutFlag
    PsUnexpectedKindAppInDataCon{}
      -> ErrorWithoutFlag
    PsInvalidRecordCon{}
      -> ErrorWithoutFlag
    PsIllegalUnboxedStringInPat{}
      -> ErrorWithoutFlag
    PsDoNotationInPat
      -> ErrorWithoutFlag
    PsIfThenElseInPat
      -> ErrorWithoutFlag
    PsLambdaCaseInPat
      -> ErrorWithoutFlag
    PsCaseInPat
      -> ErrorWithoutFlag
    PsLetInPat
      -> ErrorWithoutFlag
    PsLambdaInPat
      -> ErrorWithoutFlag
    PsArrowExprInPat{}
      -> ErrorWithoutFlag
    PsArrowCmdInPat{}
      -> ErrorWithoutFlag
    PsArrowCmdInExpr{}
      -> ErrorWithoutFlag
    PsViewPatInExpr{}
      -> ErrorWithoutFlag
    PsLambdaCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    PsCaseCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    PsIfCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    PsLetCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    PsDoCmdInFunAppCmd{}
      -> ErrorWithoutFlag
    PsDoInFunAppExpr{}
      -> ErrorWithoutFlag
    PsMDoInFunAppExpr{}
      -> ErrorWithoutFlag
    PsLambdaInFunAppExpr{}
      -> ErrorWithoutFlag
    PsCaseInFunAppExpr{}
      -> ErrorWithoutFlag
    PsLambdaCaseInFunAppExpr{}
      -> ErrorWithoutFlag
    PsLetInFunAppExpr{}
      -> ErrorWithoutFlag
    PsIfInFunAppExpr{}
      -> ErrorWithoutFlag
    PsProcInFunAppExpr{}
      -> ErrorWithoutFlag
    PsMalformedTyOrClDecl{}
      -> ErrorWithoutFlag
    PsIllegalWhereInDataDecl
      -> ErrorWithoutFlag
    PsIllegalDataTypeContext{}
      -> ErrorWithoutFlag
    PsPrimStringInvalidChar
      -> ErrorWithoutFlag
    PsSuffixAT
      -> ErrorWithoutFlag
    PsPrecedenceOutOfRange{}
      -> ErrorWithoutFlag
    PsSemiColonsInCondExpr{}
      -> ErrorWithoutFlag
    PsSemiColonsInCondCmd{}
      -> ErrorWithoutFlag
    PsAtInPatternPosition
      -> ErrorWithoutFlag
    PsParseErrorOnInput{}
      -> ErrorWithoutFlag
    PsMalformedDecl{}
      -> ErrorWithoutFlag
    PsUnexpectedTypeAppInDecl{}
      -> ErrorWithoutFlag
    PsNotADataCon{}
      -> ErrorWithoutFlag
    PsInferredTypeVarNotAllowed
      -> ErrorWithoutFlag
    PsIllegalTraditionalRecordSyntax{}
      -> ErrorWithoutFlag
    PsParseErrorInCmd{}
      -> ErrorWithoutFlag
    PsParseErrorInPat{}
      -> ErrorWithoutFlag
    PsIllegalRoleName{}
      -> ErrorWithoutFlag
    PsInvalidTypeSignature{}
      -> ErrorWithoutFlag
    PsUnexpectedTypeInDecl{}
      -> ErrorWithoutFlag
    PsInvalidPackageName{}
      -> ErrorWithoutFlag


--
-- Message construction
--

-- | Variation of 'mkParserMessage' which can be used when we are /sure/ the
-- input 'PsMessage' is an instrinsic error (i.e. an 'ErrorWithoutFlag' from which
-- we can't recover).
mkParserErrorMessage :: SrcSpan -> PsMessage -> MsgEnvelope PsMessage
mkParserErrorMessage span = mkErrorMsgEnvelope span alwaysQualify

mkParserMessage :: DynFlags -> SrcSpan -> PsMessage -> MsgEnvelope PsMessage
mkParserMessage df span = mkMsgEnvelope df span alwaysQualify

pp_unexpected_fun_app :: Outputable a => SDoc -> a -> SDoc
pp_unexpected_fun_app e a =
   text "Unexpected " <> e <> text " in function application:"
    $$ nest 4 (ppr a)
    $$ text "You could write it with parentheses"
    $$ text "Or perhaps you meant to enable BlockArguments?"

pp_hint :: PsHint -> SDoc
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
