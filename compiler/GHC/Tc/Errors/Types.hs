module GHC.Tc.Errors.Types where

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

-- FIXME(adinapoli) Untangle this.
type DsError   = Error
type DsWarning = Warning

-- FIXME(adinapoli) Convert free-text warnings into structured values.
data Warning
  = WarnTcRnRaw ErrDoc
  -- | WarnTypedHoles
  -- | WarnPartialTypeSignatures
  -- | WarnDeferredOutOfScopeVariables
  -- | WarnInaccessibleCode
  -- | WarnRedundantConstraints

data Error
  = ErrTcRnRaw ErrDoc

  -- Errors thrown in GHC.Tc.Errors
  | ErrBadTelescope
      [LHsTyVarBndr Specificity GhcRn] -- telescope
      [TyCoVar] -- sorted tyvars (in a correct order)
      SDoc      -- context
  | ErrOutOfScope
      RdrName -- name tried
      OutOfScopeSuggestions -- similar name, import, etc suggestions
      SDoc -- extra contents (see 'unboundNameX')
      SDoc -- context lines
  | ErrOutOfScopeHole
      OccName -- out of scope name
      TcType  -- type of the hole
      OutOfScopeSuggestions -- similar name, import, etc suggestions


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
