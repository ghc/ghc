{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module GHC.Parser.Errors.Ppr
   ( pprWarning
   , pprError
   )
where

import GHC.Prelude
import GHC.Driver.Flags
import GHC.Parser.Errors
import GHC.Parser.Types
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader (starInfo, rdrNameOcc, opIsAt, mkUnqual)
import GHC.Types.Name.Occurrence (isSymOcc, occNameFS, varName)
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Data.FastString
import GHC.Hs.Expr (prependQualified,HsExpr(..))
import GHC.Hs.Type (pprLHsContext)
import GHC.Builtin.Names (allNameStrings)
import GHC.Builtin.Types (filterCTuple)
import Data.List.NonEmpty (NonEmpty((:|)))

mkParserErr :: SrcSpan -> SDoc -> MsgEnvelope DecoratedSDoc
mkParserErr span doc = MsgEnvelope
   { errMsgSpan        = span
   , errMsgContext     = alwaysQualify
   , errMsgDiagnostic  = mkDecorated [doc]
   , errMsgSeverity    = SevError
   , errMsgReason      = NoReason
   }

mkParserWarn :: WarningFlag -> SrcSpan -> SDoc -> MsgEnvelope DecoratedSDoc
mkParserWarn flag span doc = MsgEnvelope
   { errMsgSpan        = span
   , errMsgContext     = alwaysQualify
   , errMsgDiagnostic  = mkDecorated [doc]
   , errMsgSeverity    = SevWarning
   , errMsgReason      = Reason flag
   }

pprWarning :: PsWarning -> MsgEnvelope DecoratedSDoc
pprWarning = \case
   PsWarnTab loc tc
      -> mkParserWarn Opt_WarnTabs loc $
          text "Tab character found here"
            <> (if tc == 1
                then text ""
                else text ", and in" <+> speakNOf (fromIntegral (tc - 1)) (text "further location"))
            <> text "."
            $+$ text "Please use spaces instead."

   PsWarnTransitionalLayout loc reason
      -> mkParserWarn Opt_WarnAlternativeLayoutRuleTransitional loc $
            text "transitional layout will not be accepted in the future:"
            $$ text (case reason of
               TransLayout_Where -> "`where' clause at the same depth as implicit layout block"
               TransLayout_Pipe  -> "`|' at the same depth as implicit layout block"
            )

   PsWarnBidirectionalFormatChars ((loc,_,desc) :| xs)
      -> mkParserWarn Opt_WarnUnicodeBidirectionalFormatCharacters (RealSrcSpan (realSrcLocSpan $ psRealLoc loc) Nothing) $
            text "A unicode bidirectional formatting character" <+> parens (text desc)
         $$ text "was found at offset" <+> ppr (bufPos (psBufPos loc)) <+> text "in the file"
         $$ (case xs of
           [] -> empty
           xs -> text "along with further bidirectional formatting characters at" <+> pprChars xs
            where
              pprChars [] = empty
              pprChars ((loc,_,desc):xs) = text "offset" <+> ppr (bufPos (psBufPos loc)) <> text ":" <+> text desc
                                       $$ pprChars xs
              )
         $$ text "Bidirectional formatting characters may be rendered misleadingly in certain editors"

   PsWarnUnrecognisedPragma loc
      -> mkParserWarn Opt_WarnUnrecognisedPragmas loc $
            text "Unrecognised pragma"

   PsWarnHaddockInvalidPos loc
      -> mkParserWarn Opt_WarnInvalidHaddock loc $
            text "A Haddock comment cannot appear in this position and will be ignored."

   PsWarnHaddockIgnoreMulti loc
      -> mkParserWarn Opt_WarnInvalidHaddock loc $
            text "Multiple Haddock comments for a single entity are not allowed." $$
            text "The extraneous comment will be ignored."

   PsWarnStarBinder loc
      -> mkParserWarn Opt_WarnStarBinder loc $
            text "Found binding occurrence of" <+> quotes (text "*")
            <+> text "yet StarIsType is enabled."
         $$ text "NB. To use (or export) this operator in"
            <+> text "modules with StarIsType,"
         $$ text "    including the definition module, you must qualify it."

   PsWarnStarIsType loc
      -> mkParserWarn Opt_WarnStarIsType loc $
             text "Using" <+> quotes (text "*")
             <+> text "(or its Unicode variant) to mean"
             <+> quotes (text "Data.Kind.Type")
          $$ text "relies on the StarIsType extension, which will become"
          $$ text "deprecated in the future."
          $$ text "Suggested fix: use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."

   PsWarnImportPreQualified loc
      -> mkParserWarn Opt_WarnPrepositiveQualifiedModule loc $
            text "Found" <+> quotes (text "qualified")
             <+> text "in prepositive position"
         $$ text "Suggested fix: place " <+> quotes (text "qualified")
             <+> text "after the module name instead."
         $$ text "To allow this, enable language extension 'ImportQualifiedPost'"

   PsWarnOperatorWhitespaceExtConflict loc sym
      -> mkParserWarn Opt_WarnOperatorWhitespaceExtConflict loc $
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
      -> mkParserWarn Opt_WarnOperatorWhitespace loc $
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

pprError :: PsError -> MsgEnvelope DecoratedSDoc
pprError err = mkParserErr (errLoc err) $ vcat
   (pp_err (errDesc err) : map pp_hint (errHints err))

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
   TypeApplicationsInPatternsOnlyDataCons ->
     text "Type applications in patterns are only allowed on data constructors."

perhaps_as_pat :: SDoc
perhaps_as_pat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"
