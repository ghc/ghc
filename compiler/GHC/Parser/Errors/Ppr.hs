{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Parser.Errors.Ppr () where

import GHC.Prelude
import GHC.Parser.Errors.Types as Parser
import GHC.Parser.Types
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader (starInfo, rdrNameOcc, opIsAt, mkUnqual)
import GHC.Types.Name.Occurrence (isSymOcc, occNameFS, varName)
import GHC.Types.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Data.FastString
import GHC.Hs.Expr (prependQualified,HsExpr(..))
import GHC.Hs.Type (pprLHsContext)
import GHC.Builtin.Names (allNameStrings)
import GHC.Builtin.Types (filterCTuple)

instance RenderableDiagnostic Parser.Error where
  renderDiagnostic = pprError

instance RenderableDiagnostic Parser.Warning where
  renderDiagnostic = pprWarning

pprWarning :: Warning -> ErrDoc
pprWarning = \case
   WarnTab tc
      -> errDoc [
          text "Tab character found here"
            <> (if tc == 1
                then text ""
                else text ", and in" <+> speakNOf (fromIntegral (tc - 1)) (text "further location"))
            <> text "."
            $+$ text "Please use spaces instead."
            ] [] []

   WarnTransitionalLayout reason
      -> errDoc [
            text "transitional layout will not be accepted in the future:"
            $$ text (case reason of
               TransLayout_Where -> "`where' clause at the same depth as implicit layout block"
               TransLayout_Pipe  -> "`|' at the same depth as implicit layout block"
            )
            ] [] []

   WarnUnrecognisedPragma
      -> errDoc [ text "Unrecognised pragma" ] [] []

   WarnHaddockInvalidPos
      -> errDoc [ text "A Haddock comment cannot appear in this position and will be ignored." ] [] []

   WarnHaddockIgnoreMulti
      -> errDoc [
            text "Multiple Haddock comments for a single entity are not allowed." $$
            text "The extraneous comment will be ignored."
            ] [] []

   WarnStarBinder
      -> errDoc [
            text "Found binding occurrence of" <+> quotes (text "*")
            <+> text "yet StarIsType is enabled."
         $$ text "NB. To use (or export) this operator in"
            <+> text "modules with StarIsType,"
         $$ text "    including the definition module, you must qualify it."
         ] [] []

   WarnStarIsType
      -> errDoc [
             text "Using" <+> quotes (text "*")
             <+> text "(or its Unicode variant) to mean"
             <+> quotes (text "Data.Kind.Type")
          $$ text "relies on the StarIsType extension, which will become"
          $$ text "deprecated in the future."
          $$ text "Suggested fix: use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."
           ] [] []

   WarnImportPreQualified
      -> errDoc [
            text "Found" <+> quotes (text "qualified")
             <+> text "in prepositive position"
         $$ text "Suggested fix: place " <+> quotes (text "qualified")
             <+> text "after the module name instead."
         $$ text "To allow this, enable language extension 'ImportQualifiedPost'"
         ] [] []

   WarnOperatorWhitespaceExtConflict sym
      -> let mk_prefix_msg operator_symbol extension_name syntax_meaning =
                  text "The prefix use of a" <+> quotes (text operator_symbol)
                    <+> text "would denote" <+> text syntax_meaning
               $$ nest 2 (text "were the" <+> text extension_name <+> text "extension enabled.")
               $$ text "Suggested fix: add whitespace after the"
                    <+> quotes (text operator_symbol) <> char '.'
         in
         case sym of
           OperatorWhitespaceSymbol_PrefixPercent ->
             errDoc [ mk_prefix_msg "%" "LinearTypes" "a multiplicity annotation" ] [] []
           OperatorWhitespaceSymbol_PrefixDollar ->
             errDoc [ mk_prefix_msg "$" "TemplateHaskell" "an untyped splice" ] [] []
           OperatorWhitespaceSymbol_PrefixDollarDollar ->
             errDoc [ mk_prefix_msg "$$" "TemplateHaskell" "a typed splice" ] [] []


   WarnOperatorWhitespace sym occ_type
      -> let mk_msg occ_type_str =
                  text "The" <+> text occ_type_str <+> text "use of a" <+> quotes (ftext sym)
                    <+> text "might be repurposed as special syntax"
               $$ nest 2 (text "by a future language extension.")
               $$ text "Suggested fix: add whitespace around it."
         in
         case occ_type of
           OperatorWhitespaceOccurrence_Prefix -> errDoc [ mk_msg "prefix" ] [] []
           OperatorWhitespaceOccurrence_Suffix -> errDoc [ mk_msg "suffix" ] [] []
           OperatorWhitespaceOccurrence_TightInfix -> errDoc [ mk_msg "tight infix" ] [] []

{-
pprError err = mkParserErr (errLoc err) $ vcat
   (pp_err (errDesc err) : map pp_hint (errHints err))
-}

pprError :: Parser.Error -> ErrDoc
pprError = \case
   ErrParseRaw e
      -> e

   ErrLambdaCase
      -> errDoc [ text "Illegal lambda-case (use LambdaCase)" ] [] []

   ErrNumUnderscores reason
      -> errDoc [ text $ case reason of
            NumUnderscore_Integral -> "Use NumericUnderscores to allow underscores in integer literals"
            NumUnderscore_Float    -> "Use NumericUnderscores to allow underscores in floating literals"
            ] [] []

   ErrPrimStringInvalidChar
      -> errDoc [ text "primitive string literal must contain only characters <= \'\\xFF\'" ] [] []

   ErrMissingBlock
      -> errDoc [ text "Missing block" ] [] []

   ErrLexer err kind
      -> errDoc [ hcat
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
         ] [] []

   ErrSuffixAT
      -> errDoc [ text "Suffix occurrence of @. For an as-pattern, remove the leading whitespace." ] [] []

   ErrParse token hints
      | null token
      -> errDoc [ text "parse error (possibly incorrect indentation or mismatched brackets)" ] [] []

      | otherwise
      -> errDoc [
             vcat $
               (text "parse error on input" <+> quotes (text token)) : map pp_hint hints
           ] [] []

   ErrCmmLexer
      -> errDoc [ text "Cmm lexical error" ] [] []

   ErrUnsupportedBoxedSumExpr s
      -> errDoc [
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
           ] [] []

   ErrUnsupportedBoxedSumPat s
      -> errDoc [
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
           ] [] []

   ErrUnexpectedQualifiedConstructor v
      -> errDoc [
           hang (text "Expected an unqualified type constructor:") 2
                (ppr v)
           ] [] []

   ErrTupleSectionInPat
      -> errDoc [ text "Tuple section in pattern context" ] [] []

   ErrIllegalBangPattern e
      -> errDoc [ text "Illegal bang-pattern (use BangPatterns):" $$ ppr e ] [] []

   ErrOpFewArgs (StarIsType star_is_type) op
      -> errDoc [
           text "Operator applied to too few arguments:" <+> ppr op
           $$ starInfo star_is_type op
           ] [] []

   ErrImportQualifiedTwice
      -> errDoc [ text "Multiple occurrences of 'qualified'" ] [] []

   ErrImportPostQualified
      -> errDoc [ text "Found" <+> quotes (text "qualified")
                  <+> text "in postpositive position. "
                $$ text "To allow this, enable language extension 'ImportQualifiedPost'"
                ] [] []

   ErrIllegalExplicitNamespace
      -> errDoc [ text "Illegal keyword 'type' (use ExplicitNamespaces to enable)" ] [] []

   ErrVarForTyCon name
      -> errDoc [
             text "Expecting a type constructor but found a variable,"
               <+> quotes (ppr name) <> text "."
             $$ if isSymOcc $ rdrNameOcc name
                then text "If" <+> quotes (ppr name) <+> text "is a type constructor"
                      <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
                else empty
           ] [] []

   ErrIllegalPatSynExport
      -> errDoc [ text "Illegal export form (use PatternSynonyms to enable)" ] [] []

   ErrMalformedEntityString
      -> errDoc [ text "Malformed entity string" ] [] []

   ErrDotsInRecordUpdate
      -> errDoc [ text "You cannot use `..' in a record update" ] [] []

   ErrPrecedenceOutOfRange i
      -> errDoc [ text "Precedence out of range: " <> int i ] [] []

   ErrInvalidDataCon t
      -> errDoc [
           hang (text "Cannot parse data constructor in a data/newtype declaration:") 2
                (ppr t)
           ] [] []

   ErrInvalidInfixDataCon lhs tc rhs
      -> errDoc [
           hang (text "Cannot parse an infix data constructor in a data/newtype declaration:") 2
                (ppr lhs <+> ppr tc <+> ppr rhs)
           ] [] []

   ErrUnpackDataCon
      -> errDoc [ text "{-# UNPACK #-} cannot be applied to a data constructor." ] [] []

   ErrUnexpectedKindAppInDataCon lhs ki
      -> errDoc [
           hang (text "Unexpected kind application in a data/newtype declaration:") 2
                (ppr lhs <+> text "@" <> ppr ki)
           ] [] []

   ErrInvalidRecordCon p
      -> errDoc [ text "Not a record constructor:" <+> ppr p ] [] []

   ErrIllegalUnboxedStringInPat lit
      -> errDoc [ text "Illegal unboxed string literal in pattern:" $$ ppr lit ] [] []

   ErrDoNotationInPat
      -> errDoc [ text "do-notation in pattern" ] [] []

   ErrIfTheElseInPat
      -> errDoc [ text "(if ... then ... else ...)-syntax in pattern" ] [] []

   ErrTypeAppInPat
      -> errDoc [ text "Type applications in patterns are not yet supported" ] [] []

   ErrLambdaCaseInPat
      -> errDoc [ text "(\\case ...)-syntax in pattern" ] [] []

   ErrCaseInPat
      -> errDoc [ text "(case ... of ...)-syntax in pattern" ] [] []

   ErrLetInPat
      -> errDoc [ text "(let ... in ...)-syntax in pattern" ] [] []

   ErrLambdaInPat
      -> errDoc [
              text "Lambda-syntax in pattern."
           $$ text "Pattern matching on functions is not possible."
           ] [] []

   ErrArrowExprInPat e
      -> errDoc [ text "Expression syntax in pattern:" <+> ppr e ] [] []

   ErrArrowCmdInPat c
      -> errDoc [ text "Command syntax in pattern:" <+> ppr c ] [] []

   ErrArrowCmdInExpr c
      -> errDoc [
           vcat [ text "Arrow command found where an expression was expected:"
                , nest 2 (ppr c)
                ]
           ] [] []

   ErrViewPatInExpr a b
      -> errDoc [
           sep [ text "View pattern in expression context:"
               , nest 4 (ppr a <+> text "->" <+> ppr b)
               ]
           ] [] []

   ErrTypeAppWithoutSpace v e
      -> errDoc [
           sep [ text "@-pattern in expression context:"
               , nest 4 (pprPrefixOcc v <> text "@" <> ppr e)
               ]
           $$ text "Type application syntax requires a space before '@'"
           ] [] []


   ErrLazyPatWithoutSpace e
      -> errDoc [
           sep [ text "Lazy pattern in expression context:"
               , nest 4 (text "~" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '~'?"
           ] [] []

   ErrBangPatWithoutSpace e
      -> errDoc [
           sep [ text "Bang pattern in expression context:"
               , nest 4 (text "!" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '!'?"
           ] [] []

   ErrUnallowedPragma prag
      -> errDoc [
           hang (text "A pragma is not allowed in this position:") 2
                (ppr prag)
           ] [] []

   ErrQualifiedDoInCmd m
      -> errDoc [ hang (text "Parse error in command:") 2 $
                  text "Found a qualified" <+> ppr m <> text ".do block in a command, but"
                  $$ text "qualified 'do' is not supported in commands."
           ] [] []

   ErrParseErrorInCmd s
      -> errDoc [
           hang (text "Parse error in command:") 2 s
           ] [] []

   ErrParseErrorInPat s hints
      -> errDoc [
             vcat $ (text "Parse error in pattern:" <+> s) : map pp_hint hints
           ] [] []

   ErrInvalidInfixHole
      -> errDoc [ text "Invalid infix hole, expected an infix operator" ] [] []

   ErrSemiColonsInCondExpr c st t se e
      -> errDoc [
             text "Unexpected semi-colons in conditional:"
             $$ nest 4 expr
             $$ text "Perhaps you meant to use DoAndIfThenElse?"
           ] [] []
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e

   ErrSemiColonsInCondCmd c st t se e
      -> errDoc [
             text "Unexpected semi-colons in conditional:"
             $$ nest 4 expr
             $$ text "Perhaps you meant to use DoAndIfThenElse?"
           ] [] []
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e


   ErrAtInPatPos
      -> errDoc [
             text "Found a binding for the"
             <+> quotes (text "@")
             <+> text "operator in a pattern position."
             $$ perhaps_as_pat
            ] [] []

   ErrLambdaCmdInFunAppCmd a
      -> errDoc [ pp_unexpected_fun_app (text "lambda command") a ] [] []

   ErrCaseCmdInFunAppCmd a
      -> errDoc [ pp_unexpected_fun_app (text "case command") a ] [] []

   ErrIfCmdInFunAppCmd a
      -> errDoc [ pp_unexpected_fun_app (text "if command") a ] [] []

   ErrLetCmdInFunAppCmd a
      -> errDoc [ pp_unexpected_fun_app (text "let command") a ] [] []

   ErrDoCmdInFunAppCmd a
      -> errDoc [ pp_unexpected_fun_app (text "do command") a ] [] []

   ErrDoInFunAppExpr m a
      -> errDoc [ pp_unexpected_fun_app (prependQualified m (text "do block")) a ] [] []

   ErrMDoInFunAppExpr m a
      -> errDoc [ pp_unexpected_fun_app (prependQualified m (text "mdo block")) a ] [] []

   ErrLambdaInFunAppExpr a
      -> errDoc [ pp_unexpected_fun_app (text "lambda expression") a ] [] []

   ErrCaseInFunAppExpr a
      -> errDoc [ pp_unexpected_fun_app (text "case expression") a ] [] []

   ErrLambdaCaseInFunAppExpr a
      -> errDoc [ pp_unexpected_fun_app (text "lambda-case expression") a ] [] []

   ErrLetInFunAppExpr a
      -> errDoc [ pp_unexpected_fun_app (text "let expression") a ] [] []

   ErrIfInFunAppExpr a
      -> errDoc [ pp_unexpected_fun_app (text "if expression") a ] [] []

   ErrProcInFunAppExpr a
      -> errDoc [ pp_unexpected_fun_app (text "proc expression") a ] [] []

   ErrMalformedTyOrClDecl ty
      -> errDoc [
             text "Malformed head of type or class declaration:"
             <+> ppr ty
           ] [] []

   ErrIllegalWhereInDataDecl
      -> errDoc [
             vcat
               [ text "Illegal keyword 'where' in data declaration"
               , text "Perhaps you intended to use GADTs or a similar language"
               , text "extension to enable syntax: data T where"
               ]
          ] [] []

   ErrIllegalTraditionalRecordSyntax s
      -> errDoc [
             text "Illegal record syntax (use TraditionalRecordSyntax):"
             <+> s
           ] [] []

   ErrParseErrorOnInput occ
      -> errDoc [ text "parse error on input" <+> ftext (occNameFS occ) ] [] []

   ErrIllegalDataTypeContext c
      -> errDoc [
            text "Illegal datatype context (use DatatypeContexts):"
            <+> pprLHsContext c
           ] [] []

   ErrMalformedDecl what for
      -> errDoc [
             text "Malformed" <+> what
             <+> text "declaration for" <+> quotes (ppr for)
           ] [] []

   ErrUnexpectedTypeAppInDecl ki what for
      -> errDoc [
           vcat [ text "Unexpected type application"
                  <+> text "@" <> ppr ki
                , text "In the" <+> what
                  <+> text "declaration for"
                  <+> quotes (ppr for)
                ]
           ] [] []

   ErrNotADataCon name
      -> errDoc [ text "Not a data constructor:" <+> quotes (ppr name) ] [] []

   ErrRecordSyntaxInPatSynDecl pat
      -> errDoc [
             text "record syntax not supported for pattern synonym declarations:"
             $$ ppr pat
           ] [] []

   ErrEmptyWhereInPatSynDecl patsyn_name
      -> errDoc [
             text "pattern synonym 'where' clause cannot be empty"
             $$ text "In the pattern synonym declaration for: "
                <+> ppr (patsyn_name)
           ] [] []

   ErrInvalidWhereBindInPatSynDecl patsyn_name decl
      -> errDoc [
             text "pattern synonym 'where' clause must bind the pattern synonym's name"
             <+> quotes (ppr patsyn_name) $$ ppr decl
           ] [] []

   ErrNoSingleWhereBindInPatSynDecl _patsyn_name decl
      -> errDoc [
             text "pattern synonym 'where' clause must contain a single binding:"
             $$ ppr decl
           ] [] []

   ErrDeclSpliceNotAtTopLevel d
      -> errDoc [
             hang (text "Declaration splices are allowed only"
               <+> text "at the top level:")
             2 (ppr d)
           ] [] []

   ErrInferredTypeVarNotAllowed
      -> errDoc [ text "Inferred type variables are not allowed here" ] [] []

   ErrIllegalRoleName role nearby
      -> errDoc [ text "Illegal role name" <+> quotes (ppr role)
                  $$ case nearby of
                      []  -> empty
                      [r] -> text "Perhaps you meant" <+> quotes (ppr r)
                      -- will this last case ever happen??
                      _   -> hang (text "Perhaps you meant one of these:")
                                  2 (pprWithCommas (quotes . ppr) nearby)
                ] [] []

   ErrMultipleNamesInStandaloneKindSignature vs
      -> errDoc [
           vcat [ hang (text "Standalone kind signatures do not support multiple names at the moment:")
                  2 (pprWithCommas ppr vs)
                , text "See https://gitlab.haskell.org/ghc/ghc/issues/16754 for details."
                ]
           ] [] []

   ErrIllegalImportBundleForm
      -> errDoc [
             text "Illegal import form, this syntax can only be used to bundle"
             $+$ text "pattern synonyms with types in module exports."
           ] [] []

   ErrInvalidTypeSignature lhs
      -> errDoc [
             text "Invalid type signature:"
             <+> ppr lhs
             <+> text ":: ..."
             $$ text hint
           ] [] []
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
         looks_like s (L _ (HsVar _ (L _ v))) = v == s
         looks_like s (L _ (HsApp _ lhs _))   = looks_like s lhs
         looks_like _ _                       = False

         foreign_RDR = mkUnqual varName (fsLit "foreign")
         default_RDR = mkUnqual varName (fsLit "default")
         pattern_RDR = mkUnqual varName (fsLit "pattern")

   ErrUnexpectedTypeInDecl t what tc tparms equals_or_where
      -> errDoc [
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
            ] [] []
          where
            -- Avoid printing a constraint tuple in the error message. Print
            -- a plain old tuple instead (since that's what the user probably
            -- wrote). See #14907
            tc' = ppr $ filterCTuple tc

   ErrCmmParser cmm_err -> let mk msg = errDoc [msg] [] [] in mk $ case cmm_err of
      CmmUnknownPrimitive name     -> text "unknown primitive" <+> ftext name
      CmmUnknownMacro fun          -> text "unknown macro" <+> ftext fun
      CmmUnknownCConv cconv        -> text "unknown calling convention:" <+> text cconv
      CmmUnrecognisedSafety safety -> text "unrecognised safety" <+> text safety
      CmmUnrecognisedHint hint     -> text "unrecognised hint:" <+> text hint

   ErrExpectedHyphen
      -> errDoc [ text "Expected a hyphen" ] [] []

   ErrSpaceInSCC
      -> errDoc [ text "Spaces are not allowed in SCCs" ] [] []

   ErrEmptyDoubleQuotes th_on
      -> errDoc [ if th_on then vcat (msg ++ th_msg) else vcat msg ] [] []
         where
            msg    = [ text "Parser error on `''`"
                     , text "Character literals may not be empty"
                     ]
            th_msg = [ text "Or perhaps you intended to use quotation syntax of TemplateHaskell,"
                     , text "but the type variable or constructor is missing"
                     ]

   ErrInvalidPackageName pkg
      -> errDoc [
             vcat
                [ text "Parse error" <> colon <+> quotes (ftext pkg)
                , text "Version number or non-alphanumeric" <+>
                  text "character in package name"
                ]
          ] [] []

   ErrInvalidRuleActivationMarker
      -> errDoc [ text "Invalid rule activation marker" ] [] []

   ErrLinearFunction
      -> errDoc [ text "Enable LinearTypes to allow linear functions" ] [] []

   ErrMultiWayIf
      -> errDoc [ text "Multi-way if-expressions need MultiWayIf turned on" ] [] []

   ErrExplicitForall is_unicode
      -> errDoc [
           vcat [ text "Illegal symbol" <+> quotes (forallSym is_unicode) <+> text "in type"
                , text "Perhaps you intended to use RankNTypes or a similar language"
                , text "extension to enable explicit-forall syntax:" <+>
                  forallSym is_unicode <+> text "<tvs>. <type>"
                ]
           ] [] []
         where
          forallSym True  = text "∀"
          forallSym False = text "forall"

   ErrIllegalQualifiedDo qdoDoc
      -> errDoc [
           vcat [ text "Illegal qualified" <+> quotes qdoDoc <+> text "block"
                , text "Perhaps you intended to use QualifiedDo"
                ]
           ] [] []

   ErrUnsupportedExtension ext sug
       -> errDoc [ text "Unsupported extension: " <> text ext $$ suggestions ] [] []
          where
            suggestions
              | null sug  = GHC.Utils.Outputable.empty
              | otherwise = text "Perhaps you meant" <+>
                  quotedListWithOr (map text sug)

   ErrUnknownOptionsFlag flag
       -> errDoc [ text "unknown flag in {-# OPTIONS_GHC #-} pragma:" <+> text flag ] [] []

   ErrLanguagePragmaParseError
       -> errDoc [
            vcat [ text "Cannot parse LANGUAGE pragma"
                 , text "Expecting comma-separated list of language options,"
                 , text "each starting with a capital letter"
                 , nest 2 (text "E.g. {-# LANGUAGE TemplateHaskell, GADTs #-}") ]
            ] [] []

   ErrOptionsGhcParseError inp
       -> errDoc [
            vcat [ text "Error while parsing OPTIONS_GHC pragma."
                 , text "Expecting whitespace-separated list of GHC options."
                 , text "  E.g. {-# OPTIONS_GHC -Wall -O2 #-}"
                 , text ("Input was: " ++ show inp)
                 ]
            ] [] []

pp_unexpected_fun_app :: Outputable a => SDoc -> a -> SDoc
pp_unexpected_fun_app e a =
   text "Unexpected " <> e <> text " in function application:"
    $$ nest 4 (ppr a)
    $$ text "You could write it with parentheses"
    $$ text "Or perhaps you meant to enable BlockArguments?"

pp_hint :: Hint -> SDoc
pp_hint = \case
   SuggestTH              -> text "Perhaps you intended to use TemplateHaskell"
   SuggestDo              -> text "Perhaps this statement should be within a 'do' block?"
   SuggestMissingDo       -> text "Possibly caused by a missing 'do'?"
   SuggestRecursiveDo     -> text "Perhaps you intended to use RecursiveDo"
   SuggestLetInDo         -> text "Perhaps you need a 'let' in a 'do' block?"
                             $$ text "e.g. 'let x = 5' instead of 'x = 5'"
   SuggestPatternSynonyms -> text "Perhaps you intended to use PatternSynonyms"

   SuggestInfixBindMaybeAtPat fun
      -> text "In a function binding for the"
            <+> quotes (ppr fun)
            <+> text "operator."
         $$ if opIsAt fun
               then perhaps_as_pat
               else empty

perhaps_as_pat :: SDoc
perhaps_as_pat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"

