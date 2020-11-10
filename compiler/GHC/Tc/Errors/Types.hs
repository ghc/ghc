module GHC.Tc.Errors.Types where

import GHC.Core.TyCo.Ppr
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Prelude
import GHC.Tc.Utils.TcType
import GHC.Types.Error
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit.Module.Name
import GHC.Unit.Types
import GHC.Utils.Outputable

-- NOTE(adinapoli) Investigate this.
type DsError = TcRnError

-- FIXME(adinapoli) Turn into a proper data type.
type TcRnWarning = ErrDoc

data TcRnError
  = TcRnErrorDoc ErrDoc

  -- Errors thrown in GHC.Tc.Errors
  | TcRnBadTelescope
      [LHsTyVarBndr Specificity GhcRn] -- telescope
      [TyCoVar] -- sorted tyvars (in a correct order)
      SDoc      -- context
  | TcRnOutOfScope
      RdrName -- name tried
      OutOfScopeSuggestions -- similar name, import, etc suggestions
      SDoc -- extra contents (see 'unboundNameX')
      SDoc -- context lines
  | TcRnOutOfScopeHole
      OccName -- out of scope name
      TcType  -- type of the hole
      OutOfScopeSuggestions -- similar name, import, etc suggestions

tcRnErrorDoc :: TcRnError -> ErrDoc
tcRnErrorDoc (TcRnErrorDoc d) = d
tcRnErrorDoc (TcRnBadTelescope telescope sorted_tvs context) =
  errDoc [m] [context] []

  where m = hang (text "These kind and type variables:" <+> sep (map ppr telescope)
               $$ text "are out of dependency order. Perhaps try this ordering:")
            2 (pprTyVars sorted_tvs)
tcRnErrorDoc (TcRnOutOfScope tried_rdr_name suggs contextlines extra) =
  errDoc [m $$ suggestions $$ extra] [] [contextlines]

  where m = hang (text "Not in scope:")
               2 (what <+> quotes (ppr tried_rdr_name))

        what = pprNonVarNameSpace (occNameSpace (rdrNameOcc tried_rdr_name))

        suggestions = pprOutOfScopeSuggestions (rdrNameOcc tried_rdr_name) suggs
tcRnErrorDoc (TcRnOutOfScopeHole occ ty suggs) =
  errDoc [m] [] [suggestions]

  where herald | isDataOcc occ = text "Data constructor not in scope:"
               | otherwise     = text "Variable not in scope:"
        m | isTyVarTy ty = hang herald 2 (ppr occ)
          | otherwise    = hang herald 2 (pp_occ_with_type occ ty)

        pp_occ_with_type :: OccName -> Type -> SDoc
        pp_occ_with_type occ hole_ty = hang (pprPrefixOcc occ) 2 (dcolon <+> pprType hole_ty)

        suggestions = pprOutOfScopeSuggestions occ suggs

instance RenderableDiagnostic TcRnError where
  renderDiagnostic = tcRnErrorDoc

type HowInScope = Either SrcSpan ImpDeclSpec
     -- Left loc    =>  locally bound at loc
     -- Right ispec =>  imported as specified by ispec

data OutOfScopeSuggestions = OutOfScopeSuggestions
  { oosSimilarNames :: Maybe NameSuggestions
  , oosImports      :: Maybe ImportSuggestion
  , oosExtensions   :: Maybe ExtensionSuggestion
  }

noOutOfScopeSuggestions :: OutOfScopeSuggestions
noOutOfScopeSuggestions = OutOfScopeSuggestions Nothing Nothing Nothing

pprOutOfScopeSuggestions
  :: OccName -> OutOfScopeSuggestions -> SDoc
pprOutOfScopeSuggestions name oos =
  maybe empty (pprNameSuggestions name) (oosSimilarNames oos) $$
  maybe empty ppr (oosImports oos) $$
  maybe empty ppr (oosExtensions oos)

newtype NameSuggestions = NameSuggestions [(RdrName, HowInScope)]

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

data ImportSuggestion
  = SuggestNoModuleImported ModuleName
  | SuggestModulesDoNotExport [Module] OccName
  | SuggestAddNameToImportLists OccName [(Module, SrcSpan)]
  | SuggestRemoveNameFromHidingLists OccName [(Module, SrcSpan)]

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

data ExtensionSuggestion = SuggestRecursiveDo

instance Outputable ExtensionSuggestion where
  ppr SuggestRecursiveDo = text "Perhaps you meant to use RecursiveDo"
