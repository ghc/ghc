{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}   {- instance Outputable GhcHint -}

module GHC.Types.Hint.Ppr (
  perhapsAsPat
  -- also, and more interesting: instance Outputable GhcHint
  ) where

import GHC.Prelude

import GHC.Parser.Errors.Basic
import GHC.Types.Hint

import GHC.Core.FamInstEnv (FamFlavor(..))
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep     ( mkVisFunTyMany )
import GHC.Hs.Expr ()   -- instance Outputable
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader (RdrName,ImpDeclSpec (..), rdrNameOcc, rdrNameSpace)
import GHC.Types.SrcLoc (SrcSpan(..), srcSpanStartLine)
import GHC.Unit.Module.Imported (ImportedModsVal(..))
import GHC.Unit.Types
import GHC.Utils.Outputable

import GHC.Driver.Flags

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified GHC.LanguageExtensions as LangExt

instance Outputable GhcHint where
  ppr = \case
    UnknownHint m
      -> ppr m
    SuggestExtension extHint
      -> case extHint of
          SuggestSingleExtension extraUserInfo ext ->
            ("Perhaps you intended to use" <+> extension_with_implied ext)
            $$ extraUserInfo
          SuggestAnyExtension extraUserInfo exts ->
            (enable "any" <+> unquotedListWith "or" (map implied exts))
            $$ extraUserInfo
          SuggestExtensions extraUserInfo exts ->
            (enable "all" <+> unquotedListWith "and" (map implied exts))
            $$ extraUserInfo
          SuggestExtensionInOrderTo extraUserInfo ext ->
            ("Use" <+> extension_with_implied ext)
            $$ extraUserInfo
      where extension_with_implied ext = "the" <+> quotes (ppr ext) <+> "extension" <+> pprImpliedExtensions ext
            implied ext = quotes (ppr ext) <+> pprImpliedExtensions ext
            enable any_or_all = "Enable" <+> any_or_all <+> "of the following extensions" <> colon
    SuggestCorrectPragmaName suggestions
      -> text "Perhaps you meant" <+> quotedListWithOr (map text suggestions)
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
    SuggestTypeSignatureRemoveQualifier
      -> text "Perhaps you meant to omit the qualifier"
    SuggestAddToHSigExportList _name mb_mod
      -> let header = text "Try adding it to the export list of"
         in case mb_mod of
              Nothing -> header <+> text "the hsig file."
              Just mod -> header <+> ppr (moduleName mod) <> text "'s hsig file."
    SuggestFixOrphanInst { isFamilyInstance = mbFamFlavor }
      -> vcat [ text "Move the instance declaration to the module of the" <+> what <+> text "or of the type, or"
              , text "wrap the type with a newtype and declare the instance on the new type."
              ]
      where
        what = case mbFamFlavor of
          Nothing                  -> text "class"
          Just  SynFamilyInst      -> text "type family"
          Just (DataFamilyInst {}) -> text "data family"
    SuggestAddStandaloneDerivation
      -> text "Use a standalone deriving declaration instead"
    SuggestAddStandaloneKindSignature name
      -> text "Add a standalone kind signature for" <+> quotes (ppr name)
    SuggestFillInWildcardConstraint
      -> text "Fill in the wildcard constraint yourself"
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
    ImportSuggestion occ_name import_suggestion
      -> pprImportSuggestion occ_name import_suggestion
    SuggestPlacePragmaInHeader
      -> text "Perhaps you meant to place it in the module header?"
      $$ text "The module header is the section at the top of the file, before the" <+> quotes (text "module") <+> text "keyword"
    SuggestPatternMatchingSyntax
      -> text "Use pattern-matching syntax instead"
    SuggestSpecialiseVisibilityHints name
      -> text "Make sure" <+> ppr mod <+> text "is compiled with -O and that"
           <+> quotes (ppr name) <+> text "has an INLINABLE pragma"
         where
           mod = nameModule name
    SuggestRenameTypeVariable
      -> text "Consider renaming the type variable."
    SuggestExplicitBidiPatSyn name pat args
      -> hang (text "Instead use an explicitly bidirectional"
               <+> text "pattern synonym, e.g.")
            2 (hang (text "pattern" <+> pp_name <+> pp_args <+> larrow
                     <+> ppr pat <+> text "where")
                  2 (pp_name <+> pp_args <+> equals <+> text "..."))
         where
           pp_name = ppr name
           pp_args = hsep (map ppr args)
    SuggestSafeHaskell
      -> text "Enable Safe Haskell through either Safe, Trustworthy or Unsafe."
    SuggestRemoveRecordWildcard
      -> text "Omit the" <+> quotes (text "..")
    SuggestIncreaseReductionDepth ->
      vcat
        [ text "Use -freduction-depth=0 to disable this check"
        , text "(any upper bound you could choose might fail unpredictably with"
        , text " minor updates to GHC, so disabling the check is recommended if"
        , text " you're sure that type checking should terminate)" ]
    SuggestMoveNonCanonicalDefinition lhs rhs refURL ->
      text "Move definition from" <+>
      quotes (pprPrefixUnqual rhs) <+>
      text "to" <+> quotes (pprPrefixUnqual lhs) $$
      text "See also:" <+> text refURL
    SuggestRemoveNonCanonicalDefinition lhs rhs refURL ->
      text "Either remove definition for" <+>
      quotes (pprPrefixUnqual lhs) <+> text "(recommended)" <+>
      text "or define as" <+>
      quotes (pprPrefixUnqual lhs <+> text "=" <+> pprPrefixUnqual rhs) $$
      text "See also:" <+> text refURL
    SuggestEtaReduceAbsDataTySyn tc
      -> text "If possible, eta-reduce the type synonym" <+> ppr_tc <+> text "so that it is nullary."
        where ppr_tc = quotes (ppr $ tyConName tc)
    RemindRecordMissingField x r a ->
      text "NB: There is no field selector" <+> ppr_sel
        <+> text "in scope for record type" <+> ppr_r
      where ppr_sel = quotes (ftext x <+> dcolon <+> ppr_arr_r_a)
            ppr_arr_r_a = ppr $ mkVisFunTyMany r a
            ppr_r = quotes $ ppr r
    SuggestBindTyVarOnLhs tv
      -> text "Bind" <+> quotes (ppr tv) <+> text "on the LHS of the type declaration"
    SuggestAnonymousWildcard
      -> text "Use an anonymous wildcard" <+> quotes (text "_")
    SuggestExplicitQuantification tv
      -> hsep [ text "Use an explicit", quotes (text "forall")
              , text "to quantify over", quotes (ppr tv) ]
    SuggestBindTyVarExplicitly tv
      -> text "bind" <+> quotes (ppr tv)
         <+> text "explicitly with" <+> quotes (char '@' <> ppr tv)
    SuggestExplicitDerivingClauseStrategies assumed_derivings ->
      hang
        (text "Use explicit deriving strategies:")
        2
        (vcat $ map pp_derivings (Map.toList assumed_derivings))
      where
        pp_derivings (strat, preds) =
          hsep [text "deriving", ppr strat, parens (pprWithCommas ppr preds)]
    SuggestExplicitStandaloneDerivingStrategy strat deriv_sig ->
      hang
        (text "Use an explicit deriving strategy:")
        2
        (hsep [text "deriving", ppr strat, text "instance", ppr deriv_sig])

perhapsAsPat :: SDoc
perhapsAsPat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"

-- | Pretty-print an 'ImportSuggestion'.
pprImportSuggestion :: OccName -> ImportSuggestion -> SDoc
pprImportSuggestion occ_name (CouldImportFrom mods)
  | (mod, imv) NE.:| [] <- mods
  = fsep
      [ text "Add"
      , quotes (ppr occ_name)
      , text "to the import list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (text "at" <+> ppr (imv_span imv)) <> dot
      ]
  | otherwise
  = fsep
      [ text "Add"
      , quotes (ppr occ_name)
      , text "to one of these import lists:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (text "at" <+> ppr (imv_span imv))
        | (mod,imv) <- NE.toList mods
        ])
pprImportSuggestion occ_name (CouldUnhideFrom mods)
  | (mod, imv) NE.:| [] <- mods
  = fsep
      [ text "Remove"
      , quotes (ppr occ_name)
      , text "from the explicit hiding list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (text "at" <+> ppr (imv_span imv)) <> dot
      ]
  | otherwise
  = fsep
      [ text "Remove"
      , quotes (ppr occ_name)
      , text "from the hiding clauses"
      , text "in one of these imports:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (text "at" <+> ppr (imv_span imv))
        | (mod,imv) <- NE.toList mods
        ])
pprImportSuggestion occ_name (CouldAddTypeKeyword mod)
  = vcat [ text "Add the" <+> quotes (text "type")
          <+> text "keyword to the import statement:"
         , nest 2 $ text "import"
            <+> ppr mod
            <+> parens_sp (text "type" <+> pprPrefixOcc occ_name)
         ]
  where
    parens_sp d = parens (space <> d <> space)
pprImportSuggestion occ_name (CouldRemoveTypeKeyword mod)
  = vcat [ text "Remove the" <+> quotes (text "type")
             <+> text "keyword from the import statement:"
         , nest 2 $ text "import"
             <+> ppr mod
             <+> parens_sp (pprPrefixOcc occ_name) ]
  where
    parens_sp d = parens (space <> d <> space)
pprImportSuggestion dc_occ (ImportDataCon Nothing parent_occ)
  = text "Import the data constructor" <+> quotes (ppr dc_occ) <+>
    text "of" <+> quotes (ppr parent_occ)
pprImportSuggestion dc_occ (ImportDataCon (Just (mod, patsyns_enabled)) parent_occ)
  = vcat $ [ text "Use"
           , nest 2 $ text "import"
               <+> ppr mod
               <+> parens_sp (pprPrefixOcc parent_occ <> parens_sp (pprPrefixOcc dc_occ))
           , text "or"
           , nest 2 $ text "import"
               <+> ppr mod
               <+> parens_sp (pprPrefixOcc parent_occ <> text "(..)")
           ] ++ if patsyns_enabled
                then [ text "or"
                     , nest 2 $ text "import"
                         <+> ppr mod
                         <+> parens_sp (text "pattern" <+> pprPrefixOcc dc_occ)
                     ]
                else []
  where
    parens_sp d = parens (space <> d <> space)

-- | Pretty-print a 'SimilarName'.
pprSimilarName :: NameSpace -> SimilarName -> SDoc
pprSimilarName _ (SimilarName name)
  = quotes (ppr name) <+> parens (pprDefinedAt name)
pprSimilarName tried_ns (SimilarRdrName rdr_name how_in_scope)
  = pp_ns rdr_name <+> quotes (ppr rdr_name) <+> loc
  where
    loc = case how_in_scope of
      Nothing -> empty
      Just scope -> case scope of
        LocallyBoundAt loc ->
          case loc of
            UnhelpfulSpan l -> parens (ppr l)
            RealSrcSpan l _ -> parens (text "line" <+> int (srcSpanStartLine l))
        ImportedBy is ->
          parens (text "imported from" <+> ppr (moduleName $ is_mod is))
    pp_ns :: RdrName -> SDoc
    pp_ns rdr | ns /= tried_ns = pprNameSpace ns
              | otherwise      = empty
      where ns = rdrNameSpace rdr

pprImpliedExtensions :: LangExt.Extension -> SDoc
pprImpliedExtensions extension = case implied of
    [] -> empty
    xs -> parens $ "implied by" <+> unquotedListWith "and" xs
  where implied = map (quotes . ppr)
                . filter (\ext -> extensionDeprecation ext == ExtensionNotDeprecated)
                . map (\(impl, _, _) -> impl)
                . filter (\(_, t, orig) -> orig == extension && t == turnOn)
                $ impliedXFlags

pprPrefixUnqual :: Name -> SDoc
pprPrefixUnqual name =
  pprPrefixOcc (getOccName name)
