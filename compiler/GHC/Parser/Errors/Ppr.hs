{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic PsMessage

module GHC.Parser.Errors.Ppr where

import GHC.Prelude
import GHC.Driver.Flags
import GHC.Parser.Errors.Basic
import GHC.Parser.Errors.Types
import GHC.Parser.Types
import GHC.Types.Basic
import GHC.Types.Hint
import GHC.Types.Error
import GHC.Types.Hint.Ppr (perhapsAsPat)
import GHC.Types.SrcLoc
import GHC.Types.Error.Codes
import GHC.Types.Name.Reader ( opIsAt, rdrNameOcc, mkUnqual )
import GHC.Types.Name.Occurrence (isSymOcc, occNameFS, varName)
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Data.FastString
import GHC.Data.Maybe (catMaybes)
import GHC.Hs.Expr (prependQualified, HsExpr(..), HsLamVariant(..), lamCaseKeyword)
import GHC.Hs.Type (pprLHsContext, pprHsArrow, pprHsForAll)
import GHC.Builtin.Names (allNameStringList)
import qualified GHC.LanguageExtensions as LangExt
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Hs.Pat (Pat(..), LPat)
import GHC.Hs.Extension
import GHC.Parser.Annotation (noAnn)


instance Diagnostic PsMessage where
  type DiagnosticOpts PsMessage = NoDiagnosticOpts
  diagnosticMessage opts = \case
    PsUnknownMessage (UnknownDiagnostic f _ m)
      -> diagnosticMessage (f opts) m

    PsHeaderMessage m
      -> psHeaderMessageDiagnostic m

    PsWarnHaddockInvalidPos
       -> mkSimpleDecorated $ text "A Haddock comment cannot appear in this position and will be ignored."
    PsWarnHaddockIgnoreMulti
       -> mkSimpleDecorated $
            text "Multiple Haddock comments for a single entity are not allowed." $$
            text "The extraneous comment will be ignored."
    PsWarnBidirectionalFormatChars ((loc,_,desc) :| xs)
      -> mkSimpleDecorated $
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

    PsWarnTab tc
      -> mkSimpleDecorated $
           text "Tab character found here"
             <> (if tc == 1
                 then text ""
                 else text ", and in" <+> speakNOf (fromIntegral (tc - 1)) (text "further location"))
             <> text "."
    PsWarnTransitionalLayout reason
      -> mkSimpleDecorated $
            text "transitional layout will not be accepted in the future:"
            $$ (case reason of
               TransLayout_Where -> text "`where' clause at the same depth as implicit layout block"
               TransLayout_Pipe  -> text "`|' at the same depth as implicit layout block"
            )
    PsWarnOperatorWhitespaceExtConflict sym
      -> let mk_prefix_msg extension_name syntax_meaning =
                  text "The prefix use of a" <+> quotes (pprOperatorWhitespaceSymbol sym)
                    <+> text "would denote" <+> syntax_meaning
               $$ nest 2 (text "were the" <+> extension_name <+> text "extension enabled.")
         in mkSimpleDecorated $
         case sym of
           OperatorWhitespaceSymbol_PrefixPercent -> mk_prefix_msg (text "LinearTypes") (text "a multiplicity annotation")
           OperatorWhitespaceSymbol_PrefixDollar -> mk_prefix_msg (text "TemplateHaskell") (text "an untyped splice")
           OperatorWhitespaceSymbol_PrefixDollarDollar -> mk_prefix_msg (text "TemplateHaskell") (text "a typed splice")
    PsWarnOperatorWhitespace sym occ_type
      -> let mk_msg occ_type_str =
                  text "The" <+> text occ_type_str <+> text "use of a" <+> quotes (ftext sym)
                    <+> text "might be repurposed as special syntax"
               $$ nest 2 (text "by a future language extension.")
         in mkSimpleDecorated $
         case occ_type of
           OperatorWhitespaceOccurrence_Prefix -> mk_msg "prefix"
           OperatorWhitespaceOccurrence_Suffix -> mk_msg "suffix"
           OperatorWhitespaceOccurrence_TightInfix -> mk_msg "tight infix"
    PsWarnStarBinder
      -> mkSimpleDecorated $
            text "Found binding occurrence of" <+> quotes (text "*")
            <+> text "yet StarIsType is enabled."
    PsWarnStarIsType
      -> mkSimpleDecorated $
             text "Using" <+> quotes (text "*")
             <+> text "(or its Unicode variant) to mean"
             <+> quotes (text "Data.Kind.Type")
          $$ text "relies on the StarIsType extension, which will become"
          $$ text "deprecated in the future."
    PsWarnUnrecognisedPragma prag _
      -> mkSimpleDecorated $ text "Unrecognised pragma"
                          <> if null prag then empty else text ":" <+> text prag
    PsWarnMisplacedPragma prag
      -> mkSimpleDecorated $ text "Misplaced" <+> pprFileHeaderPragmaType prag <+> text "pragma"
    PsWarnImportPreQualified
      -> mkSimpleDecorated $
            text "Found" <+> quotes (text "qualified")
             <+> text "in prepositive position"
    PsWarnViewPatternSignatures old new
      -> mkDecorated $
          [ text "Found an unparenthesized pattern signature on the RHS of a view pattern."
          , vcat [ text "This code might stop working in a future GHC release"
                 , text "due to a planned change to the precedence of view patterns,"
                 , text "unless the view function is an endofunction." ]
          , nest 2 $
            vcat [ text "Current parse:" <+> quotes (ppr (add_parens_sig old))
                 , text "Future parse:" <+> quotes (ppr (add_parens_view new)) ]
          ]
      where
        add_parens_sig :: LPat GhcPs -> LPat GhcPs
        add_parens_sig = go
          where go (L l (ViewPat x e p)) = L l (ViewPat x e (go p))
                go (L l (SigPat x p sig)) = par_pat (L l (SigPat x p sig))
                go p = p

        add_parens_view :: LPat GhcPs -> LPat GhcPs
        add_parens_view = go
          where go (L l (ViewPat x e p)) = par_pat (L l (ViewPat x e p))
                go (L l (SigPat x p sig)) = L l (SigPat x (go p) sig)
                go p = p

        par_pat :: LPat GhcPs -> LPat GhcPs
        par_pat p = L noAnn (ParPat noAnn p)

    PsErrLexer err kind
      -> mkSimpleDecorated $ hcat
           [ case err of
              LexError               -> text "lexical error"
              LexUnknownPragma       -> text "unknown pragma"
              LexErrorInPragma       -> text "lexical error in pragma"
              LexNumEscapeRange      -> text "numeric escape sequence out of range"
              LexUnterminatedComment -> text "unterminated `{-'"
              LexUnterminatedOptions -> text "unterminated OPTIONS pragma"
              LexUnterminatedQQ      -> text "unterminated quasiquotation"

           , case kind of
              LexErrKind_EOF    -> text " at end of input"
              LexErrKind_UTF8   -> text " (UTF-8 decoding error)"
              LexErrKind_Char c -> text $ " at character " ++ show c
           ]
    PsErrParse token _details
      | null token
      -> mkSimpleDecorated $ text "parse error (possibly incorrect indentation or mismatched brackets)"
      | otherwise
      -> mkSimpleDecorated $ text "parse error on input" <+> quotes (text token)
    PsErrCmmLexer
      -> mkSimpleDecorated $ text "Cmm lexical error"
    PsErrCmmParser cmm_err -> mkSimpleDecorated $ case cmm_err of
      CmmUnknownPrimitive name     -> text "unknown primitive" <+> ftext name
      CmmUnknownMacro fun          -> text "unknown macro" <+> ftext fun
      CmmUnknownCConv cconv        -> text "unknown calling convention:" <+> text cconv
      CmmUnrecognisedSafety safety -> text "unrecognised safety" <+> text safety
      CmmUnrecognisedHint hint     -> text "unrecognised hint:" <+> text hint

    PsErrTypeAppWithoutSpace v e
      -> mkSimpleDecorated $
           sep [ text "@-pattern in expression context:"
               , nest 4 (pprPrefixOcc v <> text "@" <> ppr e)
               ]
           $$ text "Type application syntax requires a space before '@'"
    PsErrLazyPatWithoutSpace e
      -> mkSimpleDecorated $
           sep [ text "Lazy pattern in expression context:"
               , nest 4 (text "~" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '~'?"
    PsErrBangPatWithoutSpace e
      -> mkSimpleDecorated $
           sep [ text "Bang pattern in expression context:"
               , nest 4 (text "!" <> ppr e)
               ]
           $$ text "Did you mean to add a space after the '!'?"
    PsErrInvalidInfixHole
      -> mkSimpleDecorated $ text "Invalid infix hole, expected an infix operator"
    PsErrExpectedHyphen
      -> mkSimpleDecorated $ text "Expected a hyphen"
    PsErrSpaceInSCC
      -> mkSimpleDecorated $ text "Spaces are not allowed in SCCs"
    PsErrEmptyDoubleQuotes _th_on
      -> mkSimpleDecorated $ vcat msg
         where
            msg    = [ text "Parser error on `''`"
                     , text "Character literals may not be empty"
                     ]
    PsErrLambdaCase
      -- we can't get this error for \cases, since without -XLambdaCase, that's
      -- just a regular lambda expression
      -> mkSimpleDecorated $ text "Illegal" <+> lamCaseKeyword LamCase
    PsErrEmptyLambda
      -> mkSimpleDecorated $ text "A lambda requires at least one parameter"
    PsErrLinearFunction
      -> mkSimpleDecorated $ text "Illegal use of linear functions"
    PsErrOverloadedRecordUpdateNotEnabled
      -> mkSimpleDecorated $ text "Illegal overloaded record update"
    PsErrMultiWayIf
      -> mkSimpleDecorated $ text "Illegal multi-way if-expression"
    PsErrNumUnderscores reason
      -> mkSimpleDecorated $
           text $ case reason of
             NumUnderscore_Integral -> "Illegal underscores in integer literals"
             NumUnderscore_Float    -> "Illegal underscores in floating literals"
    PsErrIllegalBangPattern e
      -> mkSimpleDecorated $ text "Illegal bang-pattern or strict binding" $$ ppr e
    PsErrOverloadedRecordDotInvalid
      -> mkSimpleDecorated $
           text "Use of OverloadedRecordDot '.' not valid ('.' isn't allowed when constructing records or in record patterns)"
    PsErrIllegalPatSynExport
      -> mkSimpleDecorated $ text "Illegal export form"
    PsErrOverloadedRecordUpdateNoQualifiedFields
      -> mkSimpleDecorated $ text "Fields cannot be qualified when OverloadedRecordUpdate is enabled"
    PsErrExplicitForall is_unicode
      -> mkSimpleDecorated $ text "Illegal symbol" <+> quotes (forallSym is_unicode) <+> text "in type"
    PsErrIllegalQualifiedDo qdoDoc
      -> mkSimpleDecorated $
           text "Illegal qualified" <+> quotes qdoDoc <+> text "block"
    PsErrQualifiedDoInCmd m
      -> mkSimpleDecorated $
           hang (text "Parse error in command:") 2 $
             text "Found a qualified" <+> ppr m <> text ".do block in a command, but"
             $$ text "qualified 'do' is not supported in commands."
    PsErrRecordSyntaxInPatSynDecl pat
      -> mkSimpleDecorated $
           text "record syntax not supported for pattern synonym declarations:"
           $$ ppr pat
    PsErrEmptyWhereInPatSynDecl patsyn_name
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause cannot be empty"
           $$ text "In the pattern synonym declaration for: "
              <+> ppr (patsyn_name)
    PsErrInvalidWhereBindInPatSynDecl patsyn_name decl
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause must bind the pattern synonym's name"
           <+> quotes (ppr patsyn_name) $$ ppr decl
    PsErrNoSingleWhereBindInPatSynDecl _patsyn_name decl
      -> mkSimpleDecorated $
           text "pattern synonym 'where' clause must contain a single binding:"
           $$ ppr decl
    PsErrDeclSpliceNotAtTopLevel d
      -> mkSimpleDecorated $
           hang (text "Declaration splices are allowed only"
                 <+> text "at the top level:")
             2 (ppr d)
    PsErrMultipleNamesInStandaloneKindSignature vs
      -> mkSimpleDecorated $
           vcat [ hang (text "Standalone kind signatures do not support multiple names at the moment:")
                  2 (pprWithCommas ppr vs)
                , text "See https://gitlab.haskell.org/ghc/ghc/issues/16754 for details."
                ]
    PsErrIllegalExplicitNamespace
      -> mkSimpleDecorated $
           text "Illegal keyword 'type'"

    PsErrUnallowedPragma prag
      -> mkSimpleDecorated $
           hang (text "A pragma is not allowed in this position:") 2
                (ppr prag)
    PsErrImportPostQualified
      -> mkSimpleDecorated $
           text "Found" <+> quotes (text "qualified")
             <+> text "in postpositive position. "
    PsErrImportQualifiedTwice
      -> mkSimpleDecorated $ text "Multiple occurrences of 'qualified'"
    PsErrIllegalImportBundleForm
      -> mkSimpleDecorated $
           text "Illegal import form, this syntax can only be used to bundle"
           $+$ text "pattern synonyms with types in module exports."
    PsErrInvalidRuleActivationMarker
      -> mkSimpleDecorated $ text "Invalid rule activation marker"

    PsErrMissingBlock
      -> mkSimpleDecorated $ text "Missing block"
    PsErrUnsupportedBoxedSumExpr s
      -> mkSimpleDecorated $
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
    PsErrUnsupportedBoxedSumPat s
      -> mkSimpleDecorated $
           hang (text "Boxed sums not supported:") 2
                (pprSumOrTuple Boxed s)
    PsErrUnexpectedQualifiedConstructor v
      -> mkSimpleDecorated $
           hang (text "Expected an unqualified type constructor:") 2
                (ppr v)
    PsErrTupleSectionInPat
      -> mkSimpleDecorated $ text "Tuple section in pattern context"
    PsErrOpFewArgs _ op
      -> mkSimpleDecorated $
           text "Operator applied to too few arguments:" <+> ppr op
    PsErrVarForTyCon name
      -> mkSimpleDecorated $
           text "Expecting a type constructor but found a variable,"
             <+> quotes (ppr name) <> text "."
           $$ if isSymOcc $ rdrNameOcc name
              then text "If" <+> quotes (ppr name) <+> text "is a type constructor"
                    <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
              else empty
    PsErrMalformedEntityString
      -> mkSimpleDecorated $ text "Malformed entity string"
    PsErrDotsInRecordUpdate
      -> mkSimpleDecorated $ text "You cannot use `..' in a record update"
    PsErrInvalidDataCon t
      -> mkSimpleDecorated $
           hang (text "Cannot parse data constructor in a data/newtype declaration:") 2
                (ppr t)
    PsErrInvalidInfixDataCon lhs tc rhs
      -> mkSimpleDecorated $
           hang (text "Cannot parse an infix data constructor in a data/newtype declaration:") 2
                (ppr lhs <+> ppr tc <+> ppr rhs)
    PsErrIllegalPromotionQuoteDataCon name
      -> mkSimpleDecorated $
           text "Illegal promotion quote mark in the declaration of" $$
           text "data/newtype constructor" <+> pprPrefixOcc name
    PsErrUnpackDataCon
      -> mkSimpleDecorated $ text "{-# UNPACK #-} cannot be applied to a data constructor."
    PsErrUnexpectedKindAppInDataCon lhs ki
      -> mkSimpleDecorated $
           hang (text "Unexpected kind application in a data/newtype declaration:") 2
                (ppr lhs <+> text "@" <> ppr ki)
    PsErrInvalidRecordCon p
      -> mkSimpleDecorated $ text "Not a record constructor:" <+> ppr p
    PsErrIllegalUnboxedStringInPat lit
      -> mkSimpleDecorated $ text "Illegal unboxed string literal in pattern:" $$ ppr lit
    PsErrIllegalUnboxedFloatingLitInPat lit
      -> mkSimpleDecorated $ text "Illegal unboxed floating point literal in pattern:" $$ ppr lit
    PsErrDoNotationInPat
      -> mkSimpleDecorated $ text "do-notation in pattern"
    PsErrIfThenElseInPat
      -> mkSimpleDecorated $ text "(if ... then ... else ...)-syntax in pattern"
    PsErrCaseInPat
      -> mkSimpleDecorated $ text "(case ... of ...)-syntax in pattern"
    PsErrLetInPat
      -> mkSimpleDecorated $ text "(let ... in ...)-syntax in pattern"
    PsErrLambdaInPat lam_variant
      -> mkSimpleDecorated $ text "Illegal" <+> lamCaseKeyword lam_variant <> text "-syntax in pattern"
    PsErrArrowExprInPat e
      -> mkSimpleDecorated $ text "Expression syntax in pattern:" <+> ppr e
    PsErrArrowCmdInPat c
      -> mkSimpleDecorated $ text "Command syntax in pattern:" <+> ppr c
    PsErrArrowCmdInExpr c
      -> mkSimpleDecorated $
           vcat
           [ text "Arrow command found where an expression was expected:"
           , nest 2 (ppr c)
           ]
    PsErrOrPatInExpr p
      -> mkSimpleDecorated $
           sep [ text "Or pattern in expression context:"
               , nest 4 (ppr p)
               ]
    PsErrCaseCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "case command") a
    PsErrLambdaCmdInFunAppCmd lam_variant a
      -> mkSimpleDecorated $
           pp_unexpected_fun_app (lamCaseKeyword lam_variant <+> text "command") a
    PsErrIfCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "if command") a
    PsErrLetCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "let command") a
    PsErrDoCmdInFunAppCmd a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "do command") a
    PsErrDoInFunAppExpr m a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (prependQualified m (text "do block")) a
    PsErrMDoInFunAppExpr m a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (prependQualified m (text "mdo block")) a
    PsErrCaseInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "case expression") a
    PsErrLambdaInFunAppExpr lam_variant a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (lamCaseKeyword lam_variant <+> text "expression") a
    PsErrLetInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "let expression") a
    PsErrIfInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "if expression") a
    PsErrProcInFunAppExpr a
      -> mkSimpleDecorated $ pp_unexpected_fun_app (text "proc expression") a
    PsErrMalformedTyOrClDecl ty
      -> mkSimpleDecorated $
           text "Malformed head of type or class declaration:" <+> ppr ty
    PsErrIllegalWhereInDataDecl
      -> mkSimpleDecorated $ text "Illegal keyword 'where' in data declaration"
    PsErrIllegalDataTypeContext c
      -> mkSimpleDecorated $
           text "Illegal datatype context:"
             <+> pprLHsContext (Just c)
    PsErrPrimStringInvalidChar
      -> mkSimpleDecorated $ text "primitive string literal must contain only characters <= \'\\xFF\'"
    PsErrSuffixAT
      -> mkSimpleDecorated $
           text "The symbol '@' occurs as a suffix." $$
           text "For an as-pattern, there must not be any whitespace surrounding '@'."
    PsErrPrecedenceOutOfRange i
      -> mkSimpleDecorated $ text "Precedence out of range: " <> int i
    PsErrSemiColonsInCondExpr c st t se e
      -> mkSimpleDecorated $
           text "Unexpected semi-colons in conditional:"
           $$ nest 4 expr
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e
    PsErrSemiColonsInCondCmd c st t se e
      -> mkSimpleDecorated $
           text "Unexpected semi-colons in conditional:"
           $$ nest 4 expr
         where
            pprOptSemi True  = semi
            pprOptSemi False = empty
            expr = text "if"   <+> ppr c <> pprOptSemi st <+>
                   text "then" <+> ppr t <> pprOptSemi se <+>
                   text "else" <+> ppr e
    PsErrAtInPatPos
      -> mkSimpleDecorated $
           text "Found a binding for the"
           <+> quotes (text "@")
           <+> text "operator in a pattern position."
           $$ perhapsAsPat
    PsErrParseErrorOnInput occ
      -> mkSimpleDecorated $ text "parse error on input" <+> ftext (occNameFS occ)
    PsErrMalformedDecl what for
      -> mkSimpleDecorated $
           text "Malformed" <+> what
           <+> text "declaration for" <+> quotes (ppr for)
    PsErrNotADataCon name
      -> mkSimpleDecorated $ text "Not a data constructor:" <+> quotes (ppr name)
    PsErrInferredTypeVarNotAllowed
      -> mkSimpleDecorated $ text "Inferred type variables are not allowed here"
    PsErrIllegalTraditionalRecordSyntax s
      -> mkSimpleDecorated $
           text "Illegal record syntax:" <+> s
    PsErrParseErrorInCmd s
      -> mkSimpleDecorated $ hang (text "Parse error in command:") 2 s
    PsErrInPat s details
      -> let msg  = parse_error_in_pat
             body = case details of
                 PEIP_NegApp -> text "-" <> ppr s
                 PEIP_OtherPatDetails (ParseContext (Just fun) _)
                  -> ppr s <+> text "In a function binding for the"
                                     <+> quotes (ppr fun)
                                     <+> text "operator."
                                  $$ if opIsAt fun
                                        then perhapsAsPat
                                        else empty
                 _  -> ppr s
         in mkSimpleDecorated $ msg <+> body
    PsErrParseRightOpSectionInPat infixOcc s
      -> mkSimpleDecorated $ parse_error_in_pat <+> pprInfixOcc infixOcc <> ppr s
    PsErrIllegalRoleName role _nearby
      -> mkSimpleDecorated $
           text "Illegal role name" <+> quotes (ppr role)
    PsErrInvalidTypeSignature reason lhs
      -> mkSimpleDecorated $ case reason of
           PsErrInvalidTypeSig_DataCon   -> text "Invalid data constructor" <+> quotes (ppr lhs) <+>
                                            text "in type signature" <> colon $$
                                            text "You can only define data constructors in data type declarations."
           PsErrInvalidTypeSig_Qualified -> text "Invalid qualified name in type signature."
           PsErrInvalidTypeSig_Other     -> text "Invalid type signature" <> colon $$
                                            text "A type signature should be of form" <+>
                                            placeHolder "variables" <+> dcolon <+> placeHolder "type" <>
                                            dot
            where placeHolder = angleBrackets . text
    PsErrUnexpectedTypeInDecl t what tc tparms equals_or_where
       -> mkSimpleDecorated $
            vcat [ text "Unexpected type" <+> quotes (ppr t)
                 , text "In the" <+> what
                   <+> text "declaration for" <+> quotes (ppr tc)
                 , vcat[ (text "A" <+> what
                          <+> text "declaration should have form")
                 , nest 2
                   (what
                    <+> ppr tc
                    <+> hsep (map text (takeList tparms allNameStringList))
                    <+> equals_or_where) ] ]
    PsErrInvalidPackageName pkg
      -> mkSimpleDecorated $ vcat
            [ text "Parse error" <> colon <+> quotes (ftext pkg)
            , text "Version number or non-alphanumeric" <+>
              text "character in package name"
            ]

    PsErrIllegalGadtRecordMultiplicity arr
      -> mkSimpleDecorated $ vcat
            [ text "Parse error" <> colon <+> quotes (ppr arr)
            , text "Record constructors in GADTs must use an ordinary, non-linear arrow."
            ]
    PsErrInvalidCApiImport {} -> mkSimpleDecorated $ vcat [ text "Wrapper stubs can't be used with CApiFFI."]

    PsErrMultipleConForNewtype tycon n -> mkSimpleDecorated $ vcat
      [ sep
          [ text "A newtype must have exactly one constructor,"
          , nest 2 $ text "but" <+> quotes (ppr tycon) <+> text "has" <+> speakN n ]
      , text "In the newtype declaration for" <+> quotes (ppr tycon) ]

    PsErrUnicodeCharLooksLike bad_char looks_like_char looks_like_char_name
      -> mkSimpleDecorated $
           hsep [ text "Unicode character"
                -- purposefully not using `quotes (text [bad_char])`, because the quotes function adds smart quotes,
                -- and smart quotes may be the topic of this error message
                , text "'" <> text [bad_char] <> text "' (" <> text (show bad_char) <> text ")"
                , text "looks like"
                , text "'" <> text [looks_like_char] <> text "' (" <> text looks_like_char_name <> text ")" <> comma
                , text "but it is not" ]

    PsErrInvalidPun PEP_QuoteDisambiguation
      -> mkSimpleDecorated $ vcat
        [ text "Disambiguating data constructors of tuples and lists is disabled."
        , text "Remove the quote to use the data constructor."
        ]

    PsErrInvalidPun PEP_TupleSyntaxType
      -> mkSimpleDecorated $ vcat
        [ text "Unboxed tuple data constructors are not supported in types."
        , text "Use" <+> quotes (text "Tuple<n># a b c ...") <+> text "to refer to the type constructor."
        ]

    PsErrInvalidPun PEP_SumSyntaxType
      -> mkSimpleDecorated $ vcat
        [ text "Unboxed sum data constructors are not supported in types."
        , text "Use" <+> quotes (text "Sum<n># a b c ...") <+> text "to refer to the type constructor."
        ]
    PsErrTypeSyntaxInPat ctx
      -> mkSimpleDecorated $ vcat
        [ text "Illegal" <+> text what <+> "in pattern:" <+> quotes ctx'
        , text "Type syntax in patterns isn't supported at the time"]
        where
          (what, ctx') = case ctx of
            PETS_FunctionArrow arg arr res -> ("function arrow", ppr arg <+> pprHsArrow arr <+> ppr res)
            PETS_Multiplicity tok p        -> ("multiplicity annotation", ppr tok <> ppr p)
            PETS_ForallTelescope tele body -> ("forall telescope", pprHsForAll tele Nothing <+> ppr body)
            PETS_ConstraintContext ctx     -> ("constraint context", ppr ctx)

    PsErrIllegalOrPat pat
      -> mkSimpleDecorated $ vcat [text "Illegal or-pattern:" <+> ppr (unLoc pat)]

  diagnosticReason  = \case
    PsUnknownMessage m                            -> diagnosticReason m
    PsHeaderMessage  m                            -> psHeaderMessageReason m
    PsWarnBidirectionalFormatChars{}              -> WarningWithFlag Opt_WarnUnicodeBidirectionalFormatCharacters
    PsWarnTab{}                                   -> WarningWithFlag Opt_WarnTabs
    PsWarnTransitionalLayout{}                    -> WarningWithFlag Opt_WarnAlternativeLayoutRuleTransitional
    PsWarnOperatorWhitespaceExtConflict{}         -> WarningWithFlag Opt_WarnOperatorWhitespaceExtConflict
    PsWarnOperatorWhitespace{}                    -> WarningWithFlag Opt_WarnOperatorWhitespace
    PsWarnHaddockInvalidPos                       -> WarningWithFlag Opt_WarnInvalidHaddock
    PsWarnHaddockIgnoreMulti                      -> WarningWithFlag Opt_WarnInvalidHaddock
    PsWarnStarBinder                              -> WarningWithFlag Opt_WarnStarBinder
    PsWarnStarIsType                              -> WarningWithFlag Opt_WarnStarIsType
    PsWarnUnrecognisedPragma{}                    -> WarningWithFlag Opt_WarnUnrecognisedPragmas
    PsWarnMisplacedPragma{}                       -> WarningWithFlag Opt_WarnMisplacedPragmas
    PsWarnImportPreQualified                      -> WarningWithFlag Opt_WarnPrepositiveQualifiedModule
    PsWarnViewPatternSignatures{}                 -> WarningWithFlag Opt_WarnViewPatternSignatures
    PsErrLexer{}                                  -> ErrorWithoutFlag
    PsErrCmmLexer                                 -> ErrorWithoutFlag
    PsErrCmmParser{}                              -> ErrorWithoutFlag
    PsErrParse{}                                  -> ErrorWithoutFlag
    PsErrTypeAppWithoutSpace{}                    -> ErrorWithoutFlag
    PsErrLazyPatWithoutSpace{}                    -> ErrorWithoutFlag
    PsErrBangPatWithoutSpace{}                    -> ErrorWithoutFlag
    PsErrInvalidInfixHole                         -> ErrorWithoutFlag
    PsErrExpectedHyphen                           -> ErrorWithoutFlag
    PsErrSpaceInSCC                               -> ErrorWithoutFlag
    PsErrEmptyDoubleQuotes{}                      -> ErrorWithoutFlag
    PsErrLambdaCase{}                             -> ErrorWithoutFlag
    PsErrEmptyLambda{}                            -> ErrorWithoutFlag
    PsErrLinearFunction{}                         -> ErrorWithoutFlag
    PsErrMultiWayIf{}                             -> ErrorWithoutFlag
    PsErrOverloadedRecordUpdateNotEnabled{}       -> ErrorWithoutFlag
    PsErrNumUnderscores{}                         -> ErrorWithoutFlag
    PsErrIllegalBangPattern{}                     -> ErrorWithoutFlag
    PsErrOverloadedRecordDotInvalid{}             -> ErrorWithoutFlag
    PsErrIllegalPatSynExport                      -> ErrorWithoutFlag
    PsErrOverloadedRecordUpdateNoQualifiedFields  -> ErrorWithoutFlag
    PsErrExplicitForall{}                         -> ErrorWithoutFlag
    PsErrIllegalQualifiedDo{}                     -> ErrorWithoutFlag
    PsErrQualifiedDoInCmd{}                       -> ErrorWithoutFlag
    PsErrRecordSyntaxInPatSynDecl{}               -> ErrorWithoutFlag
    PsErrEmptyWhereInPatSynDecl{}                 -> ErrorWithoutFlag
    PsErrInvalidWhereBindInPatSynDecl{}           -> ErrorWithoutFlag
    PsErrNoSingleWhereBindInPatSynDecl{}          -> ErrorWithoutFlag
    PsErrDeclSpliceNotAtTopLevel{}                -> ErrorWithoutFlag
    PsErrMultipleNamesInStandaloneKindSignature{} -> ErrorWithoutFlag
    PsErrIllegalExplicitNamespace                 -> ErrorWithoutFlag
    PsErrUnallowedPragma{}                        -> ErrorWithoutFlag
    PsErrImportPostQualified                      -> ErrorWithoutFlag
    PsErrImportQualifiedTwice                     -> ErrorWithoutFlag
    PsErrIllegalImportBundleForm                  -> ErrorWithoutFlag
    PsErrInvalidRuleActivationMarker              -> ErrorWithoutFlag
    PsErrMissingBlock                             -> ErrorWithoutFlag
    PsErrUnsupportedBoxedSumExpr{}                -> ErrorWithoutFlag
    PsErrUnsupportedBoxedSumPat{}                 -> ErrorWithoutFlag
    PsErrUnexpectedQualifiedConstructor{}         -> ErrorWithoutFlag
    PsErrTupleSectionInPat{}                      -> ErrorWithoutFlag
    PsErrOpFewArgs{}                              -> ErrorWithoutFlag
    PsErrVarForTyCon{}                            -> ErrorWithoutFlag
    PsErrMalformedEntityString                    -> ErrorWithoutFlag
    PsErrDotsInRecordUpdate                       -> ErrorWithoutFlag
    PsErrInvalidDataCon{}                         -> ErrorWithoutFlag
    PsErrInvalidInfixDataCon{}                    -> ErrorWithoutFlag
    PsErrIllegalPromotionQuoteDataCon{}           -> ErrorWithoutFlag
    PsErrUnpackDataCon                            -> ErrorWithoutFlag
    PsErrUnexpectedKindAppInDataCon{}             -> ErrorWithoutFlag
    PsErrInvalidRecordCon{}                       -> ErrorWithoutFlag
    PsErrIllegalUnboxedStringInPat{}              -> ErrorWithoutFlag
    PsErrIllegalUnboxedFloatingLitInPat{}         -> ErrorWithoutFlag
    PsErrDoNotationInPat{}                        -> ErrorWithoutFlag
    PsErrIfThenElseInPat                          -> ErrorWithoutFlag
    PsErrCaseInPat                                -> ErrorWithoutFlag
    PsErrLetInPat                                 -> ErrorWithoutFlag
    PsErrLambdaInPat{}                            -> ErrorWithoutFlag
    PsErrArrowExprInPat{}                         -> ErrorWithoutFlag
    PsErrArrowCmdInPat{}                          -> ErrorWithoutFlag
    PsErrArrowCmdInExpr{}                         -> ErrorWithoutFlag
    PsErrOrPatInExpr{}                            -> ErrorWithoutFlag
    PsErrCaseCmdInFunAppCmd{}                     -> ErrorWithoutFlag
    PsErrLambdaCmdInFunAppCmd{}                   -> ErrorWithoutFlag
    PsErrIfCmdInFunAppCmd{}                       -> ErrorWithoutFlag
    PsErrLetCmdInFunAppCmd{}                      -> ErrorWithoutFlag
    PsErrDoCmdInFunAppCmd{}                       -> ErrorWithoutFlag
    PsErrDoInFunAppExpr{}                         -> ErrorWithoutFlag
    PsErrMDoInFunAppExpr{}                        -> ErrorWithoutFlag
    PsErrLambdaInFunAppExpr{}                     -> ErrorWithoutFlag
    PsErrCaseInFunAppExpr{}                       -> ErrorWithoutFlag
    PsErrLetInFunAppExpr{}                        -> ErrorWithoutFlag
    PsErrIfInFunAppExpr{}                         -> ErrorWithoutFlag
    PsErrProcInFunAppExpr{}                       -> ErrorWithoutFlag
    PsErrMalformedTyOrClDecl{}                    -> ErrorWithoutFlag
    PsErrIllegalWhereInDataDecl                   -> ErrorWithoutFlag
    PsErrIllegalDataTypeContext{}                 -> ErrorWithoutFlag
    PsErrPrimStringInvalidChar                    -> ErrorWithoutFlag
    PsErrSuffixAT                                 -> ErrorWithoutFlag
    PsErrPrecedenceOutOfRange{}                   -> ErrorWithoutFlag
    PsErrSemiColonsInCondExpr{}                   -> ErrorWithoutFlag
    PsErrSemiColonsInCondCmd{}                    -> ErrorWithoutFlag
    PsErrAtInPatPos                               -> ErrorWithoutFlag
    PsErrParseErrorOnInput{}                      -> ErrorWithoutFlag
    PsErrMalformedDecl{}                          -> ErrorWithoutFlag
    PsErrNotADataCon{}                            -> ErrorWithoutFlag
    PsErrInferredTypeVarNotAllowed                -> ErrorWithoutFlag
    PsErrIllegalTraditionalRecordSyntax{}         -> ErrorWithoutFlag
    PsErrParseErrorInCmd{}                        -> ErrorWithoutFlag
    PsErrInPat{}                                  -> ErrorWithoutFlag
    PsErrIllegalRoleName{}                        -> ErrorWithoutFlag
    PsErrInvalidTypeSignature{}                   -> ErrorWithoutFlag
    PsErrUnexpectedTypeInDecl{}                   -> ErrorWithoutFlag
    PsErrInvalidPackageName{}                     -> ErrorWithoutFlag
    PsErrParseRightOpSectionInPat{}               -> ErrorWithoutFlag
    PsErrIllegalGadtRecordMultiplicity{}          -> ErrorWithoutFlag
    PsErrInvalidCApiImport {}                     -> ErrorWithoutFlag
    PsErrMultipleConForNewtype {}                 -> ErrorWithoutFlag
    PsErrUnicodeCharLooksLike{}                   -> ErrorWithoutFlag
    PsErrInvalidPun {}                            -> ErrorWithoutFlag
    PsErrIllegalOrPat{}                           -> ErrorWithoutFlag
    PsErrTypeSyntaxInPat{}                        -> ErrorWithoutFlag

  diagnosticHints = \case
    PsUnknownMessage m                            -> diagnosticHints m
    PsHeaderMessage  m                            -> psHeaderMessageHints m
    PsWarnBidirectionalFormatChars{}              -> noHints
    PsWarnTab{}                                   -> [SuggestUseSpaces]
    PsWarnTransitionalLayout{}                    -> noHints
    PsWarnOperatorWhitespaceExtConflict sym       -> [SuggestUseWhitespaceAfter sym]
    PsWarnOperatorWhitespace sym occ              -> [SuggestUseWhitespaceAround (unpackFS sym) occ]
    PsWarnHaddockInvalidPos                       -> noHints
    PsWarnHaddockIgnoreMulti                      -> noHints
    PsWarnStarBinder                              -> [SuggestQualifyStarOperator]
    PsWarnStarIsType                              -> [SuggestUseTypeFromDataKind Nothing]
    PsWarnUnrecognisedPragma ""  _                -> noHints
    PsWarnUnrecognisedPragma p   avail            ->
      let suggestions = fuzzyMatch p avail
       in if null suggestions
          then noHints
          else [SuggestCorrectPragmaName suggestions]
    PsWarnMisplacedPragma{}                       -> [SuggestPlacePragmaInHeader]
    PsWarnImportPreQualified                      -> [ SuggestQualifiedAfterModuleName
                                                     , suggestExtension LangExt.ImportQualifiedPost]
    PsWarnViewPatternSignatures{}                 -> [SuggestParenthesizePatternRHS]
    PsErrLexer{}                                  -> noHints
    PsErrCmmLexer                                 -> noHints
    PsErrCmmParser{}                              -> noHints
    PsErrParse token PsErrParseDetails{..}        -> case token of
      ""                         -> []
      "$"  | not ped_th_enabled  -> [suggestExtension LangExt.TemplateHaskell]   -- #7396
      "$$" | not ped_th_enabled  -> [suggestExtension LangExt.TemplateHaskell]   -- #20157
      "<-" | ped_mdo_in_last_100 -> [suggestExtension LangExt.RecursiveDo]
           | otherwise           -> [SuggestMissingDo]
      "="  | ped_do_in_last_100  -> [SuggestLetInDo]                             -- #15849
      _    | not ped_pat_syn_enabled
           , ped_pattern_parsed  -> [suggestExtension LangExt.PatternSynonyms]   -- #12429
           | otherwise           -> []
    PsErrTypeAppWithoutSpace{}                    -> noHints
    PsErrLazyPatWithoutSpace{}                    -> noHints
    PsErrBangPatWithoutSpace{}                    -> noHints
    PsErrInvalidInfixHole                         -> noHints
    PsErrExpectedHyphen                           -> noHints
    PsErrSpaceInSCC                               -> noHints
    PsErrEmptyDoubleQuotes th_on | th_on          -> [SuggestThQuotationSyntax]
                                 | otherwise      -> noHints
    PsErrLambdaCase{}                             -> [suggestExtension LangExt.LambdaCase]
    PsErrEmptyLambda{}                            -> noHints
    PsErrLinearFunction{}                         -> [suggestExtension LangExt.LinearTypes]
    PsErrMultiWayIf{}                             -> [suggestExtension LangExt.MultiWayIf]
    PsErrOverloadedRecordUpdateNotEnabled{}       -> [suggestExtension LangExt.OverloadedRecordUpdate]
    PsErrNumUnderscores{}                         -> [suggestExtension LangExt.NumericUnderscores]
    PsErrIllegalBangPattern{}                     -> [suggestExtension LangExt.BangPatterns]
    PsErrOverloadedRecordDotInvalid{}             -> noHints
    PsErrIllegalPatSynExport                      -> [suggestExtension LangExt.PatternSynonyms]
    PsErrOverloadedRecordUpdateNoQualifiedFields  -> noHints
    PsErrExplicitForall is_unicode                -> [useExtensionInOrderTo info LangExt.ExplicitForAll]
      where info = "to enable syntax:" <+> forallSym is_unicode <+> angleBrackets "tvs" <> dot <+> angleBrackets "type"
    PsErrIllegalQualifiedDo{}                     -> [suggestExtension LangExt.QualifiedDo]
    PsErrQualifiedDoInCmd{}                       -> noHints
    PsErrRecordSyntaxInPatSynDecl{}               -> noHints
    PsErrEmptyWhereInPatSynDecl{}                 -> noHints
    PsErrInvalidWhereBindInPatSynDecl{}           -> noHints
    PsErrNoSingleWhereBindInPatSynDecl{}          -> noHints
    PsErrDeclSpliceNotAtTopLevel{}                -> noHints
    PsErrMultipleNamesInStandaloneKindSignature{} -> noHints
    PsErrIllegalExplicitNamespace                 -> [suggestExtension LangExt.ExplicitNamespaces]
    PsErrUnallowedPragma{}                        -> noHints
    PsErrImportPostQualified                      -> [suggestExtension LangExt.ImportQualifiedPost]
    PsErrImportQualifiedTwice                     -> noHints
    PsErrIllegalImportBundleForm                  -> noHints
    PsErrInvalidRuleActivationMarker              -> noHints
    PsErrMissingBlock                             -> noHints
    PsErrUnsupportedBoxedSumExpr{}                -> noHints
    PsErrUnsupportedBoxedSumPat{}                 -> noHints
    PsErrUnexpectedQualifiedConstructor{}         -> noHints
    PsErrTupleSectionInPat{}                      -> noHints
    PsErrOpFewArgs star_is_type op
      -> noStarIsTypeHints star_is_type op
    PsErrVarForTyCon{}                            -> noHints
    PsErrMalformedEntityString                    -> noHints
    PsErrDotsInRecordUpdate                       -> noHints
    PsErrInvalidDataCon{}                         -> noHints
    PsErrInvalidInfixDataCon{}                    -> noHints
    PsErrIllegalPromotionQuoteDataCon{}           -> noHints
    PsErrUnpackDataCon                            -> noHints
    PsErrUnexpectedKindAppInDataCon{}             -> noHints
    PsErrInvalidRecordCon{}                       -> noHints
    PsErrIllegalUnboxedStringInPat{}              -> noHints
    PsErrIllegalUnboxedFloatingLitInPat{}         -> noHints
    PsErrDoNotationInPat{}                        -> noHints
    PsErrIfThenElseInPat                          -> noHints
    PsErrCaseInPat                                -> noHints
    PsErrLetInPat                                 -> noHints
    PsErrLambdaInPat{}                            -> noHints
    PsErrArrowExprInPat{}                         -> noHints
    PsErrArrowCmdInPat{}                          -> noHints
    PsErrArrowCmdInExpr{}                         -> noHints
    PsErrOrPatInExpr{}                            -> noHints
    PsErrLambdaCmdInFunAppCmd{}                   -> suggestParensAndBlockArgs
    PsErrCaseCmdInFunAppCmd{}                     -> suggestParensAndBlockArgs
    PsErrIfCmdInFunAppCmd{}                       -> suggestParensAndBlockArgs
    PsErrLetCmdInFunAppCmd{}                      -> suggestParensAndBlockArgs
    PsErrDoCmdInFunAppCmd{}                       -> suggestParensAndBlockArgs
    PsErrDoInFunAppExpr{}                         -> suggestParensAndBlockArgs
    PsErrMDoInFunAppExpr{}                        -> suggestParensAndBlockArgs
    PsErrLambdaInFunAppExpr{}                     -> suggestParensAndBlockArgs
    PsErrCaseInFunAppExpr{}                       -> suggestParensAndBlockArgs
    PsErrLetInFunAppExpr{}                        -> suggestParensAndBlockArgs
    PsErrIfInFunAppExpr{}                         -> suggestParensAndBlockArgs
    PsErrProcInFunAppExpr{}                       -> suggestParensAndBlockArgs
    PsErrMalformedTyOrClDecl{}                    -> noHints
    PsErrIllegalWhereInDataDecl                   -> [useExtensionInOrderTo "to enable syntax: data T where" LangExt.GADTSyntax]
    PsErrIllegalDataTypeContext{}                 -> [suggestExtension LangExt.DatatypeContexts]
    PsErrPrimStringInvalidChar                    -> noHints
    PsErrSuffixAT                                 -> noHints
    PsErrPrecedenceOutOfRange{}                   -> noHints
    PsErrSemiColonsInCondExpr{}                   -> [suggestExtension LangExt.DoAndIfThenElse]
    PsErrSemiColonsInCondCmd{}                    -> [suggestExtension LangExt.DoAndIfThenElse]
    PsErrAtInPatPos                               -> noHints
    PsErrParseErrorOnInput{}                      -> noHints
    PsErrMalformedDecl{}                          -> noHints
    PsErrNotADataCon{}                            -> noHints
    PsErrInferredTypeVarNotAllowed                -> noHints
    PsErrIllegalTraditionalRecordSyntax{}         -> [suggestExtension LangExt.TraditionalRecordSyntax]
    PsErrParseErrorInCmd{}                        -> noHints
    PsErrInPat _ details                          -> case details of
      PEIP_RecPattern args YesPatIsRecursive ctx
       | length args /= 0 -> catMaybes [sug_recdo, sug_missingdo ctx]
       | otherwise        -> catMaybes [sug_missingdo ctx]
      PEIP_OtherPatDetails ctx -> catMaybes [sug_missingdo ctx]
      _                        -> []
      where
        sug_recdo                                           = Just (suggestExtension LangExt.RecursiveDo)
        sug_missingdo (ParseContext _ YesIncompleteDoBlock) = Just SuggestMissingDo
        sug_missingdo _                                     = Nothing
    PsErrParseRightOpSectionInPat{}               -> noHints
    PsErrIllegalRoleName _ nearby                 -> [SuggestRoles nearby]
    PsErrInvalidTypeSignature reason lhs          ->
        if | foreign_RDR `looks_like` lhs
           -> [suggestExtension LangExt.ForeignFunctionInterface]
           | default_RDR `looks_like` lhs
           -> [suggestExtension LangExt.DefaultSignatures]
           | pattern_RDR `looks_like` lhs
           -> [suggestExtension LangExt.PatternSynonyms]
           | PsErrInvalidTypeSig_Qualified <- reason
           -> [SuggestTypeSignatureRemoveQualifier]
           | otherwise
           -> []
      where
        -- A common error is to forget the ForeignFunctionInterface flag
        -- so check for that, and suggest.  cf #3805
        -- Sadly 'foreign import' still barfs 'parse error' because
        --  'import' is a keyword
        -- looks_like :: RdrName -> LHsExpr GhcPsErr -> Bool -- AZ
        looks_like s (L _ (HsVar _ (L _ v))) = v == s
        looks_like s (L _ (HsApp _ lhs _))   = looks_like s lhs
        looks_like _ _                       = False

        foreign_RDR = mkUnqual varName (fsLit "foreign")
        default_RDR = mkUnqual varName (fsLit "default")
        pattern_RDR = mkUnqual varName (fsLit "pattern")
    PsErrUnexpectedTypeInDecl{}                   -> noHints
    PsErrInvalidPackageName{}                     -> noHints
    PsErrIllegalGadtRecordMultiplicity{}          -> noHints
    PsErrInvalidCApiImport {}                     -> noHints
    PsErrMultipleConForNewtype {}                 -> noHints
    PsErrUnicodeCharLooksLike{}                   -> noHints
    PsErrInvalidPun {}                            -> [suggestExtension LangExt.ListTuplePuns]
    PsErrIllegalOrPat{}                           -> [suggestExtension LangExt.OrPatterns]
    PsErrTypeSyntaxInPat{}                        -> noHints

  diagnosticCode = constructorCode @GHC

psHeaderMessageDiagnostic :: PsHeaderMessage -> DecoratedSDoc
psHeaderMessageDiagnostic = \case
  PsErrParseLanguagePragma
    -> mkSimpleDecorated $
         vcat [ text "Cannot parse LANGUAGE pragma"
              , text "Expecting comma-separated list of language options,"
              , text "each starting with a capital letter"
              , nest 2 (text "E.g. {-# LANGUAGE TemplateHaskell, GADTs #-}") ]
  PsErrUnsupportedExt unsup _
    -> mkSimpleDecorated $ text "Unsupported extension: " <> text unsup
  PsErrParseOptionsPragma str
    -> mkSimpleDecorated $
         vcat [ text "Error while parsing OPTIONS_GHC pragma."
              , text "Expecting whitespace-separated list of GHC options."
              , text "  E.g. {-# OPTIONS_GHC -Wall -O2 #-}"
              , text ("Input was: " ++ show str) ]
  PsErrUnknownOptionsPragma flag
    -> mkSimpleDecorated $ text "Unknown flag in  {-# OPTIONS_GHC #-} pragma:" <+> text flag

psHeaderMessageReason :: PsHeaderMessage -> DiagnosticReason
psHeaderMessageReason = \case
  PsErrParseLanguagePragma
    -> ErrorWithoutFlag
  PsErrUnsupportedExt{}
    -> ErrorWithoutFlag
  PsErrParseOptionsPragma{}
    -> ErrorWithoutFlag
  PsErrUnknownOptionsPragma{}
    -> ErrorWithoutFlag

psHeaderMessageHints :: PsHeaderMessage -> [GhcHint]
psHeaderMessageHints = \case
  PsErrParseLanguagePragma
    -> noHints
  PsErrUnsupportedExt unsup supported
    -> if null suggestions
          then noHints
          -- FIXME(adn) To fix the compiler crash in #19923 we just rewrap this into an
          -- UnknownHint, but we should have here a proper hint, but that would require
          -- changing 'supportedExtensions' to emit a list of 'Extension'.
          else [UnknownHint $ text "Perhaps you meant" <+> quotedListWithOr (map text suggestions)]
       where
         suggestions :: [String]
         suggestions = fuzzyMatch unsup supported
  PsErrParseOptionsPragma{}
    -> noHints
  PsErrUnknownOptionsPragma{}
    -> noHints


suggestParensAndBlockArgs :: [GhcHint]
suggestParensAndBlockArgs =
  [SuggestParentheses, suggestExtension LangExt.BlockArguments]

pp_unexpected_fun_app :: Outputable a => SDoc -> a -> SDoc
pp_unexpected_fun_app e a =
   text "Unexpected " <> e <> text " in function application:"
    $$ nest 4 (ppr a)

parse_error_in_pat :: SDoc
parse_error_in_pat = text "Parse error in pattern:"

forallSym :: Bool -> SDoc
forallSym True  = text "∀"
forallSym False = text "forall"

pprFileHeaderPragmaType :: FileHeaderPragmaType -> SDoc
pprFileHeaderPragmaType OptionsPrag    = text "OPTIONS"
pprFileHeaderPragmaType IncludePrag    = text "INCLUDE"
pprFileHeaderPragmaType LanguagePrag   = text "LANGUAGE"
pprFileHeaderPragmaType DocOptionsPrag = text "OPTIONS_HADDOCK"
