{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}   -- instance Outputable GhcHint

module GHC.Types.Hint.Ppr (
  perhapsAsPat
  -- also, and more interesting: instance Outputable GhcHint
  ) where

import GHC.Prelude

import GHC.Parser.Errors.Basic
import GHC.Types.Hint

import GHC.Hs.Expr ()   -- instance Outputable
import GHC.Types.Id
import GHC.Unit.Types
import GHC.Utils.Outputable

import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE

instance Outputable GhcHint where
  ppr = \case
    UnknownHint m
      -> ppr m
    SuggestExtension extHint
      -> case extHint of
          SuggestSingleExtension extraUserInfo ext ->
            (text "Perhaps you intended to use" <+> ppr ext) $$ extraUserInfo
          SuggestAnyExtension extraUserInfo exts ->
            let header = text "Enable any of the following extensions:"
            in  header <+> hcat (intersperse (text ", ") (map ppr exts)) $$ extraUserInfo
          SuggestExtensions extraUserInfo exts ->
            let header = text "Enable all of the following extensions:"
            in  header <+> hcat (intersperse (text ", ") (map ppr exts)) $$ extraUserInfo
          SuggestExtensionInOrderTo extraUserInfo ext ->
            (text "Use" <+> ppr ext) $$ extraUserInfo
    SuggestMissingDo
      -> text "Possibly caused by a missing 'do'?"
    SuggestLetInDo
      -> text "Perhaps you need a 'let' in a 'do' block?"
           $$ text "e.g. 'let x = 5' instead of 'x = 5'"
    SuggestAddSignatureCabalFile pi_mod_name
      -> text "Try adding" <+> quotes (ppr pi_mod_name)
           <+> text "to the"
           <+> quotes (text "signatures")
           <+> text "field in your Cabal file."
    SuggestSignatureInstantiations pi_mod_name suggestions
      -> let suggested_instantiated_with =
               hcat (punctuate comma $
                   [ ppr k <> text "=" <> ppr v
                   | InstantiationSuggestion k v <- suggestions
                   ])
         in text "Try passing -instantiated-with=\"" <>
              suggested_instantiated_with <> text "\"" $$
                text "replacing <" <> ppr pi_mod_name <> text "> as necessary."
    SuggestUseSpaces
      -> text "Please use spaces instead."
    SuggestUseWhitespaceAfter sym
      -> text "Add whitespace after the"
           <+> quotes (pprOperatorWhitespaceSymbol sym) <> char '.'
    SuggestUseWhitespaceAround sym _occurrence
      -> text "Add whitespace around" <+> quotes (text sym) <> char '.'
    SuggestParentheses
      -> text "Use parentheses."
    SuggestIncreaseMaxPmCheckModels
      -> text "Increase the limit or resolve the warnings to suppress this message."
    SuggestAddTypeSignatures bindings
      -> case bindings of
          -- This might happen when we have bindings which are /too complicated/,
          -- see for example 'DsCannotMixPolyAndUnliftedBindings' in 'GHC.HsToCore.Errors.Types'.
          -- In this case, we emit a generic message.
          UnnamedBinding   -> text "Add a type signature."
          NamedBindings (x NE.:| xs) ->
            let nameList = case xs of
                  [] -> quotes . ppr $ x
                  _  -> pprWithCommas (quotes . ppr) xs <+> text "and" <+> quotes (ppr x)
            in hsep [ text "Consider giving"
                    , nameList
                    , text "a type signature"]
    SuggestBindToWildcard rhs
      -> hang (text "Suppress this warning by saying") 2 (quotes $ text "_ <-" <+> ppr rhs)
    SuggestAddInlineOrNoInlinePragma lhs_id rule_act
      -> vcat [ text "Add an INLINE[n] or NOINLINE[n] pragma for" <+> quotes (ppr lhs_id)
              , whenPprDebug (ppr (idInlineActivation lhs_id) $$ ppr rule_act)
              ]
    SuggestAddPhaseToCompetingRule bad_rule
      -> vcat [ text "Add phase [n] or [~n] to the competing rule"
              , whenPprDebug (ppr bad_rule) ]
    SuggestIncreaseSimplifierIterations
      -> text "Set limit with -fconstraint-solver-iterations=n; n=0 for no limit"
    SuggestUseTypeFromDataKind
      -> text "Use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."
    SuggestQualifiedAfterModuleName
      -> text "Place" <+> quotes (text "qualified")
          <+> text "after the module name."
    SuggestThQuotationSyntax
      -> vcat [ text "Perhaps you intended to use quotation syntax of TemplateHaskell,"
              , text "but the type variable or constructor is missing"
              ]
    SuggestRoles nearby
      -> case nearby of
               []  -> empty
               [r] -> text "Perhaps you meant" <+> quotes (ppr r)
               -- will this last case ever happen??
               _   -> hang (text "Perhaps you meant one of these:")
                           2 (pprWithCommas (quotes . ppr) nearby)
    SuggestQualifyStarOperator
      -> text "To use (or export) this operator in"
            <+> text "modules with StarIsType,"
         $$ text "    including the definition module, you must qualify it."
    SuggestTypeSignatureForm
      -> text "A type signature should be of form <variables> :: <type>"
    SuggestAddToHSigExportList _name mb_mod
      -> let header = text "Try adding it to the export list of"
         in case mb_mod of
              Nothing -> header <+> text "the hsig file."
              Just mod -> header <+> ppr (moduleName mod) <> text "'s hsig file."
    SuggestFixOrphanInstance
      -> vcat [ text "Move the instance declaration to the module of the class or of the type, or"
              , text "wrap the type with a newtype and declare the instance on the new type."
              ]
    SuggestAddStandaloneDerivation
      -> text "Use a standalone deriving declaration instead"
    SuggestFillInWildcardConstraint
      -> text "Fill in the wildcard constraint yourself"
    SuggestRenameForall
      -> vcat [ text "Consider using another name, such as"
              , quotes (text "forAll") <> comma <+>
                quotes (text "for_all") <> comma <+> text "or" <+>
                quotes (text "forall_") <> dot ]

perhapsAsPat :: SDoc
perhapsAsPat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"
