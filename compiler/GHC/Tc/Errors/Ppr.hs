{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Tc.Errors.Ppr (
    notInScopeErr
  , exactNameErr
  ) where

import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Rep
import GHC.Core.Type ( isTyVarTy )
import GHC.Data.FastString ( sLit )
import GHC.Prelude
import GHC.Tc.Errors.Types as TcRn
import GHC.Tc.Types.Origin ( RenderableTyVarBndr(..) )
import GHC.Types.Error
import GHC.Types.Name ( Name )
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Unit.State ( pprWithUnitState )
import GHC.Utils.Outputable

instance RenderableDiagnostic TcRnMessage where
  renderDiagnostic = pprTcRnMessage

notInScopeErr :: RdrName -> SDoc
notInScopeErr rdr_name
  = case isExact_maybe rdr_name of
      Just name -> exactNameErr name
      Nothing -> hang (text "Not in scope:")
                  2 (what <+> quotes (ppr rdr_name))
  where
    what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))

{- Note [When to show/hide the module-not-imported line]           -- #15611
For the error message:
    Not in scope X.Y
    Module X does not export Y
    No module named ‘X’ is imported:
there are 2 cases, where we hide the last "no module is imported" line:
1. If the module X has been imported.
2. If the module X is the current module. There are 2 subcases:
   2.1 If the unknown module name is in a input source file,
       then we can use the getModule function to get the current module name.
       (See test T15611a)
   2.2 If the unknown module name has been entered by the user in GHCi,
       then the getModule function returns something like "interactive:Ghci1",
       and we have to check the current module in the last added entry of
       the HomePackageTable. (See test T15611b)
-}

exactNameErr :: Name -> SDoc
exactNameErr name =
  hang (text "The exact Name" <+> quotes (ppr name) <+> ptext (sLit "is not in scope"))
    2 (vcat [ text "Probable cause: you used a unique Template Haskell name (NameU), "
            , text "perhaps via newName, but did not bind it"
            , text "If that's it, then -ddump-splices might be useful" ])

pprTcRnMessage :: TcRnMessage -> DecoratedSDoc
pprTcRnMessage = \case
  TcRnUnknownMessage d ->
    d

  TcRnMessageWithUnitState unit_state msg ->
    mapDecorated (pprWithUnitState unit_state) $ pprTcRnMessage msg

  TcRnBadTelescope telescope sorted_tvs context ->
    mkDecorated $ [m, context]
      where m = hang (text "These kind and type variables:"
                  <+> sep (map (\(RenderableTyVarBndr b) -> ppr b) telescope)
                   $$ text "are out of dependency order. Perhaps try this ordering:")
                2 (pprTyVars sorted_tvs)

  TcRnOutOfScope tried_rdr_name suggs contextlines extra ->
    mkDecorated [notInScopeErr tried_rdr_name $$ suggestions $$ extra, contextlines]
      where suggestions = pprOutOfScopeSuggestions (rdrNameOcc tried_rdr_name) suggs

  TcRnOutOfScopeHole occ ty suggs ->
    mkDecorated [m, suggestions]
      where herald | isDataOcc occ = text "Data constructor not in scope:"
                   | otherwise     = text "Variable not in scope:"
            m | isTyVarTy ty = hang herald 2 (ppr occ)
              | otherwise    = hang herald 2 (pp_occ_with_type occ ty)

            pp_occ_with_type :: OccName -> Type -> SDoc
            pp_occ_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

            suggestions = pprOutOfScopeSuggestions occ suggs

--
-- Pretty-printing suggestions
--

pprOutOfScopeSuggestions
  :: OccName -> OutOfScopeSuggestions -> SDoc
pprOutOfScopeSuggestions name oos =
  maybe empty (pprNameSuggestions name) (oosSimilarNames oos) $$
  maybe empty ppr (oosImports oos) $$
  maybe empty ppr (oosExtensions oos)

pprNameSuggestions :: OccName -> NameSuggestions -> SDoc
pprNameSuggestions tried_occ (NameSuggestions names) = case names of
    [] -> empty
    [p] -> perhaps <+> pp_item p
    ps -> sep [ perhaps <+> text "one of these:"
              , nest 2 (pprWithCommas pp_item ps) ]

    where
      perhaps = text "Perhaps you meant"

      pp_item :: (RdrName, HowInScope) -> SDoc
      pp_item (rdr, Left loc) = pp_ns rdr <+> quotes (ppr rdr) <+> loc' -- Locally defined
        where loc' = case loc of
                UnhelpfulSpan l -> parens (ppr l)
                RealSrcSpan l _ -> parens (text "line" <+> int (srcSpanStartLine l))
      pp_item (rdr, Right is) = pp_ns rdr <+> quotes (ppr rdr) <+>   -- Imported
                                parens (text "imported from" <+> ppr (is_mod is))

      pp_ns :: RdrName -> SDoc
      pp_ns rdr | ns /= tried_ns = pprNameSpace ns
                | otherwise      = empty
        where ns = rdrNameSpace rdr

      tried_ns = occNameSpace tried_occ

instance Outputable ImportSuggestion where
  ppr is = case is of
    SuggestNoModuleImported mname ->
      hsep [ text "No module named"
           , quotes (ppr mname)
           , text "is imported."
           ]
    SuggestModulesDoNotExport mods occ
      | [mod] <- mods ->
          hsep [ text "Module"
               , quotes (ppr mod)
               , text "does not export"
               , quotes (ppr occ) <> dot
               ]
      | otherwise ->
          hsep [ text "Neither"
               , quotedListWithNor (map ppr mods)
               , text "exports"
               , quotes (ppr occ) <> dot
               ]
    SuggestAddNameToImportLists occ mods
      | [(mod, imvspan)] <- mods ->
          fsep [ text "Perhaps you want to add"
               , quotes (ppr occ)
               , text "to the import list"
               , text "in the import of"
               , quotes (ppr mod)
               , parens (ppr imvspan) <> dot
               ]
      | otherwise ->
          fsep [ text "Perhaps you want to add"
               , quotes (ppr occ)
               , text "to one of these import lists:"
               ]
          $$
          nest 2 (vcat
              [ quotes (ppr mod) <+> parens (ppr imvspan)
              | (mod, imvspan) <- mods
              ])
    SuggestRemoveNameFromHidingLists occ mods
      | [(mod, imvspan)] <- mods ->
          fsep [ text "Perhaps you want to remove"
               , quotes (ppr occ)
               , text "from the explicit hiding list"
               , text "in the import of"
               , quotes (ppr mod)
               , parens (ppr imvspan) <> dot
               ]
      | otherwise ->
          fsep [ text "Perhaps you want to remove"
               , quotes (ppr occ)
               , text "from the hiding clauses"
               , text "in one of these imports:"
               ]
          $$
          nest 2 (vcat
              [ quotes (ppr mod) <+> parens (ppr imvspan)
              | (mod, imvspan) <- mods
              ])

instance Outputable ExtensionSuggestion where
  ppr SuggestRecursiveDo = text "Perhaps you meant to use RecursiveDo"
