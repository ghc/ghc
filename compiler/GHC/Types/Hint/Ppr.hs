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
import GHC.Types.Name (NameSpace, pprDefinedAt, occNameSpace, pprNameSpace, isValNameSpace)
import GHC.Types.Name.Reader (RdrName,ImpDeclSpec (..), rdrNameOcc, rdrNameSpace)
import GHC.Types.SrcLoc (SrcSpan(..), srcSpanStartLine)
import GHC.Unit.Module.Imported (ImportedModsVal(..))
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
    SuggestUseTypeFromDataKind mb_rdr_name
      -> text "Use" <+> quotes (text "Type")
         <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."
         $$
           maybe empty
           (\rdr_name ->
             text "NB: with NoStarIsType, " <> quotes (ppr rdr_name)
             <+> text "is treated as a regular type operator.")
           mb_rdr_name

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
    SuggestAppropriateTHTick ns
      -> text "Perhaps use a" <+> how_many <+> text "tick"
        where
          how_many
            | isValNameSpace ns = text "single"
            | otherwise         = text "double"
    SuggestDumpSlices
      -> vcat [ text "If you bound a unique Template Haskell name (NameU)"
              , text "perhaps via newName,"
              , text "then -ddump-splices might be useful." ]
    SuggestAddTick (UntickedConstructor fixity name)
      -> hsep [ text "Use"
              , char '\'' <> con
              , text "instead of"
              , con <> mb_dot ]
        where
          con = pprUntickedConstructor fixity name
          mb_dot
            | isBareSymbol fixity name
            -- A final dot can be confusing for a symbol without parens, e.g.
            --
            --  * Use ': instead of :.
            = empty
            | otherwise
            = dot

    SuggestAddTick UntickedExplicitList
      -> text "Add a promotion tick, e.g." <+> text "'[x,y,z]" <> dot
    SuggestMoveToDeclarationSite what rdr_name
      -> text "Move the" <+> what <+> text "to the declaration site of"
         <+> quotes (ppr rdr_name) <> dot
    SuggestSimilarNames tried_rdr_name similar_names
      -> case similar_names of
            n NE.:| [] -> text "Perhaps use" <+> pp_item n
            _          -> sep [ text "Perhaps use one of these:"
                              , nest 2 (pprWithCommas pp_item $ NE.toList similar_names) ]
        where
          tried_ns = occNameSpace $ rdrNameOcc tried_rdr_name
          pp_item = pprSimilarName tried_ns
    RemindFieldSelectorSuppressed rdr_name parents
      -> text "Notice that" <+> quotes (ppr rdr_name)
         <+> text "is a field selector" <+> whose
         $$ text "that has been suppressed by NoFieldSelectors."
      where
        -- parents may be empty if this is a pattern synonym field without a selector
        whose | null parents = empty
              | otherwise    = text "belonging to the type" <> plural parents
                                 <+> pprQuotedList parents
    ImportSuggestion import_suggestion
      -> pprImportSuggestion import_suggestion
    SuggestImportingDataCon
      -> text "Import the data constructor to bring it into scope"
    SuggestPlacePragmaInHeader
      -> text "Perhaps you meant to place it in the module header?"
      $$ text "The module header is the section at the top of the file, before the" <+> quotes (text "module") <+> text "keyword"

perhapsAsPat :: SDoc
perhapsAsPat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"

-- | Pretty-print an 'ImportSuggestion'.
pprImportSuggestion :: ImportSuggestion -> SDoc
pprImportSuggestion (CouldImportFrom mods occ_name)
  | (mod, imv) NE.:| [] <- mods
  = fsep
      [ text "Perhaps you want to add"
      , quotes (ppr occ_name)
      , text "to the import list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (ppr (imv_span imv)) <> dot
      ]
  | otherwise
  = fsep
      [ text "Perhaps you want to add"
      , quotes (ppr occ_name)
      , text "to one of these import lists:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (ppr (imv_span imv))
        | (mod,imv) <- NE.toList mods
        ])
pprImportSuggestion (CouldUnhideFrom mods occ_name)
  | (mod, imv) NE.:| [] <- mods
  = fsep
      [ text "Perhaps you want to remove"
      , quotes (ppr occ_name)
      , text "from the explicit hiding list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (ppr (imv_span imv)) <> dot
      ]
  | otherwise
  = fsep
      [ text "Perhaps you want to remove"
      , quotes (ppr occ_name)
      , text "from the hiding clauses"
      , text "in one of these imports:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (ppr (imv_span imv))
        | (mod,imv) <- NE.toList mods
        ])

-- | Pretty-print a 'SimilarName'.
pprSimilarName :: NameSpace -> SimilarName -> SDoc
pprSimilarName _ (SimilarName name)
  = quotes (ppr name) <+> parens (pprDefinedAt name)
pprSimilarName tried_ns (SimilarRdrName rdr_name how_in_scope)
  = case how_in_scope of
      LocallyBoundAt loc ->
        pp_ns rdr_name <+> quotes (ppr rdr_name) <+> loc'
          where
            loc' = case loc of
              UnhelpfulSpan l -> parens (ppr l)
              RealSrcSpan l _ -> parens (text "line" <+> int (srcSpanStartLine l))
      ImportedBy is ->
        pp_ns rdr_name <+> quotes (ppr rdr_name) <+>
        parens (text "imported from" <+> ppr (is_mod is))

  where
    pp_ns :: RdrName -> SDoc
    pp_ns rdr | ns /= tried_ns = pprNameSpace ns
              | otherwise      = empty
      where ns = rdrNameSpace rdr
