{-# LANGUAGE PatternSynonyms #-}

{-

This module contains helper functions for reporting and creating
unbound variables.

-}
module GHC.Rename.Unbound
   ( mkUnboundName
   , mkUnboundNameRdr
   , mkUnboundGRE
   , mkUnboundGRERdr
   , isUnboundName
   , reportUnboundName
   , reportUnboundName'
   , unknownNameSuggestions
   , unknownNameSuggestionsMessage
   , similarNameSuggestions
   , fieldSelectorSuggestions
   , WhatLooking(..)
   , WhereLooking(..)
   , LookingFor(..)
   , unboundName
   , unboundNameX
   , unboundTermNameInTypes
   , IsTermInTypes(..)
   , notInScopeErr
   , nameSpacesRelated
   , termNameInType
   )
where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Driver.Env (hsc_units)
import GHC.Driver.Env.Types

import {-# SOURCE #-} GHC.Tc.Errors.Hole ( getHoleFitDispConfig )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Builtin.Names ( mkUnboundName, isUnboundName )
import GHC.Utils.Misc
import GHC.Utils.Panic (panic)

import GHC.Data.Maybe
import GHC.Data.FastString

import qualified GHC.LanguageExtensions as LangExt

import GHC.Types.Hint
  ( GhcHint (SuggestExtension, RemindFieldSelectorSuppressed, ImportSuggestion, SuggestSimilarNames)
  , LanguageExtensionHint (SuggestSingleExtension)
  , ImportSuggestion(..), SimilarName(..), HowInScope(..) )
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader

import GHC.Unit.Module
import GHC.Unit.Module.Imported
import GHC.Utils.Outputable
import GHC.Runtime.Context

import GHC.Data.Bag
import Language.Haskell.Syntax.ImpExp

import Data.List (sortBy, partition)
import Data.List.NonEmpty ( pattern (:|), NonEmpty )
import qualified Data.List.NonEmpty as NE ( nonEmpty )
import Data.Function ( on )
import qualified Data.Semigroup as S
import qualified Data.Map as M

{-
************************************************************************
*                                                                      *
               What to do when a lookup fails
*                                                                      *
************************************************************************
-}

data WhereLooking = WL_Anywhere   -- Any binding
                  | WL_Global     -- Any top-level binding (local or imported)
                  | WL_LocalTop   -- Any top-level binding in this module
                  | WL_LocalOnly
                        -- Only local bindings
                        -- (pattern synonyms declarations,
                        -- see Note [Renaming pattern synonym variables]
                        -- in GHC.Rename.Bind)

data LookingFor = LF { lf_which :: WhatLooking
                     , lf_where :: WhereLooking
                     }

data IsTermInTypes = UnknownTermInTypes RdrName | TermInTypes RdrName | NoTermInTypes

mkUnboundNameRdr :: RdrName -> Name
mkUnboundNameRdr rdr = mkUnboundName (rdrNameOcc rdr)

mkUnboundGRE :: OccName -> GlobalRdrElt
mkUnboundGRE occ = mkLocalGRE UnboundGRE NoParent $ mkUnboundName occ

mkUnboundGRERdr :: RdrName -> GlobalRdrElt
mkUnboundGRERdr rdr = mkLocalGRE UnboundGRE NoParent $ mkUnboundNameRdr rdr

reportUnboundName' :: WhatLooking -> RdrName -> RnM Name
reportUnboundName' what_look rdr = unboundName (LF what_look WL_Anywhere) rdr

reportUnboundName :: RdrName -> RnM Name
reportUnboundName = reportUnboundName' WL_Anything

unboundName :: LookingFor -> RdrName -> RnM Name
unboundName lf rdr = unboundNameX lf rdr []

unboundNameX :: LookingFor -> RdrName -> [GhcHint] -> RnM Name
unboundNameX looking_for rdr_name hints
  = unboundNameOrTermInType NoTermInTypes looking_for rdr_name hints

unboundTermNameInTypes :: LookingFor -> RdrName -> RdrName  -> RnM Name
unboundTermNameInTypes looking_for rdr_name demoted_rdr_name
  = unboundNameOrTermInType (UnknownTermInTypes demoted_rdr_name) looking_for rdr_name []

-- Catches imported qualified terms in type signatures
-- with proper error message and suggestions
termNameInType :: LookingFor -> RdrName -> RdrName -> [GhcHint] -> RnM Name
termNameInType looking_for rdr_name demoted_rdr_name external_hints
  = unboundNameOrTermInType (TermInTypes demoted_rdr_name) looking_for rdr_name external_hints

unboundNameOrTermInType :: IsTermInTypes -> LookingFor -> RdrName -> [GhcHint] -> RnM Name
unboundNameOrTermInType if_term_in_type looking_for rdr_name hints
  = do  { dflags <- getDynFlags
        ; let show_helpful_errors = gopt Opt_HelpfulErrors dflags
        ; if not show_helpful_errors
          then addErr =<< make_error [] hints
          else do { local_env  <- getLocalRdrEnv
                  ; global_env <- getGlobalRdrEnv
                  ; impInfo <- getImports
                  ; currmod <- getModule
                  ; ic <- hsc_IC <$> getTopEnv
                  ; let (imp_errs, suggs) =
                          unknownNameSuggestions_ looking_for
                            dflags ic currmod global_env local_env impInfo
                            rdr_name
                  ; traceTc "unboundNameOrTermInType" $
                     vcat [ text "rdr_name:" <+> ppr rdr_name
                          , text "what_looking:" <+> text (show $ lf_which looking_for)
                          , text "imp_errs:" <+> ppr imp_errs
                          , text "suggs:" <+> ppr suggs
                          ]
                  ; addErr =<<
                      make_error imp_errs (hints ++ suggs) }
        ; return (mkUnboundNameRdr rdr_name) }
    where
      name_to_search = case if_term_in_type of
        NoTermInTypes                   -> rdr_name
        UnknownTermInTypes demoted_name -> demoted_name
        TermInTypes demoted_name        -> demoted_name

      err = notInScopeErr (lf_where looking_for) name_to_search

      make_error imp_errs hints =
        case if_term_in_type of
          TermInTypes demoted_name ->
            unknownNameSuggestionsMessage (TcRnTermNameInType demoted_name)
              [] -- no import errors
              hints
          _ -> unknownNameSuggestionsMessage (TcRnNotInScope err name_to_search)
                 imp_errs hints

unknownNameSuggestionsMessage :: TcRnMessage -> [ImportError] -> [GhcHint] -> RnM TcRnMessage
unknownNameSuggestionsMessage msg imp_errs hints
  = do { unit_state <- hsc_units <$> getTopEnv
       ; hfdc <- getHoleFitDispConfig
       ; let supp = case NE.nonEmpty imp_errs of
                       Nothing -> Nothing
                       Just ne_imp_errs ->
                         (Just (hfdc, [SupplementaryImportErrors ne_imp_errs]))
       ; return $
           TcRnMessageWithInfo unit_state $
             mkDetailedMessage (ErrInfo [] supp hints) msg
       }

notInScopeErr :: WhereLooking -> RdrName -> NotInScopeError
notInScopeErr where_look rdr_name
  | Just name <- isExact_maybe rdr_name
  = NoExactName name
  | WL_LocalTop <- where_look
  = NoTopLevelBinding
  | otherwise
  = NotInScope

-- | Called from the typechecker ("GHC.Tc.Errors") when we find an unbound variable
unknownNameSuggestions :: LocalRdrEnv -> WhatLooking -> RdrName -> RnM ([ImportError], [GhcHint])
unknownNameSuggestions lcl_env what_look tried_rdr_name =
  do { dflags  <- getDynFlags
     ; rdr_env <- getGlobalRdrEnv
     ; imp_info <- getImports
     ; curr_mod <- getModule
     ; interactive_context <- hsc_IC <$> getTopEnv
     ; return $
        unknownNameSuggestions_
          (LF what_look WL_Anywhere)
          dflags interactive_context curr_mod rdr_env lcl_env imp_info tried_rdr_name }

unknownNameSuggestions_ :: LookingFor -> DynFlags -> InteractiveContext
                       -> Module
                       -> GlobalRdrEnv -> LocalRdrEnv -> ImportAvails
                       -> RdrName -> ([ImportError], [GhcHint])
unknownNameSuggestions_ looking_for dflags hpt curr_mod global_env local_env
                          imports tried_rdr_name = (imp_errs, suggs)
  where
    suggs = mconcat
      [ if_ne (SuggestSimilarNames tried_rdr_name) $
          similarNameSuggestions looking_for dflags global_env local_env tried_rdr_name
      , map (ImportSuggestion $ rdrNameOcc tried_rdr_name) imp_suggs
      , extensionSuggestions tried_rdr_name
      , fieldSelectorSuggestions global_env tried_rdr_name ]
    (imp_errs, imp_suggs) = importSuggestions looking_for hpt curr_mod imports tried_rdr_name

    if_ne :: (NonEmpty a -> b) -> [a] -> [b]
    if_ne _ []       = []
    if_ne f (a : as) = [f (a :| as)]

-- | When the name is in scope as field whose selector has been suppressed by
-- NoFieldSelectors, display a helpful message explaining this.
fieldSelectorSuggestions :: GlobalRdrEnv -> RdrName -> [GhcHint]
fieldSelectorSuggestions global_env tried_rdr_name
  | null gres = []
  | otherwise = [RemindFieldSelectorSuppressed tried_rdr_name parents]
  where
    gres = filter isNoFieldSelectorGRE
         $ lookupGRE global_env (LookupRdrName tried_rdr_name AllRelevantGREs)
    parents = [ parent | ParentIs parent <- map greParent gres ]

similarNameSuggestions :: LookingFor -> DynFlags
                       -> GlobalRdrEnv -> LocalRdrEnv
                       -> RdrName -> [SimilarName]
similarNameSuggestions looking_for@(LF what_look where_look) dflags global_env
                       local_env tried_rdr_name
  = fuzzyLookup (showPpr dflags tried_rdr_name) all_possibilities
  where
    all_possibilities :: [(String, SimilarName)]
    all_possibilities = case what_look of
      WL_None -> []
      _ -> [ (showPpr dflags r, SimilarRdrName r (Just $ LocallyBoundAt loc))
           | (r,loc) <- local_possibilities local_env ]
        ++ [ (showPpr dflags r, rp) | (r, rp) <- global_possibilities global_env ]

    tried_occ     = rdrNameOcc tried_rdr_name
    tried_is_sym  = isSymOcc tried_occ
    tried_ns      = occNameSpace tried_occ
    tried_is_qual = isQual tried_rdr_name

    correct_name_space occ =
      (nameSpacesRelated dflags what_look tried_ns (occNameSpace occ))
      && isSymOcc occ == tried_is_sym
        -- Treat operator and non-operators as non-matching
        -- This heuristic avoids things like
        --      Not in scope 'f'; perhaps you meant '+' (from Prelude)

    local_ok = case where_look of { WL_Anywhere  -> True
                                  ; WL_LocalOnly -> True
                                  ; _            -> False }

    local_possibilities :: LocalRdrEnv -> [(RdrName, SrcSpan)]
    local_possibilities env
      | tried_is_qual = []
      | not local_ok  = []
      | otherwise     = [ (mkRdrUnqual occ, nameSrcSpan name)
                        | name <- localRdrEnvElts env
                        , let occ = nameOccName name
                        , correct_name_space occ]

    global_possibilities :: GlobalRdrEnv -> [(RdrName, SimilarName)]
    global_possibilities global_env
      | tried_is_qual = [ (rdr_qual, SimilarRdrName rdr_qual (Just how))
                        | gre <- globalRdrEnvElts global_env
                        , isGreOk looking_for gre
                        , let occ = greOccName gre
                        , correct_name_space occ
                        , (mod, how) <- qualsInScope gre
                        , let rdr_qual = mkRdrQual mod occ ]

      | otherwise = [ (rdr_unqual, sim)
                    | gre <- globalRdrEnvElts global_env
                    , isGreOk looking_for gre
                    , let occ = greOccName gre
                          rdr_unqual = mkRdrUnqual occ
                    , correct_name_space occ
                    , sim <- case (unquals_in_scope gre, quals_only gre) of
                                (how:_, _)    -> [ SimilarRdrName rdr_unqual (Just how) ]
                                ([],    pr:_) -> [ pr ]  -- See Note [Only-quals]
                                ([],    [])   -> [] ]

              -- Note [Only-quals]
              -- ~~~~~~~~~~~~~~~~~
              -- The second alternative returns those names with the same
              -- OccName as the one we tried, but live in *qualified* imports
              -- e.g. if you have:
              --
              -- > import qualified Data.Map as Map
              -- > foo :: Map
              --
              -- then we suggest @Map.Map@.

    --------------------
    unquals_in_scope :: GlobalRdrElt -> [HowInScope]
    unquals_in_scope (gre@GRE { gre_lcl = lcl, gre_imp = is })
      | lcl       = [ LocallyBoundAt (greDefinitionSrcSpan gre) ]
      | otherwise = [ ImportedBy ispec
                    | i <- bagToList is, let ispec = is_decl i
                    , not (is_qual ispec) ]


    --------------------
    quals_only :: GlobalRdrElt -> [SimilarName]
    -- Ones for which *only* the qualified version is in scope
    quals_only (gre@GRE { gre_imp = is })
      = [ (SimilarRdrName (mkRdrQual (is_as ispec) (greOccName gre)) (Just $ ImportedBy ispec))
        | i <- bagToList is, let ispec = is_decl i, is_qual ispec ]


-- | Generate errors and helpful suggestions if a qualified name Mod.foo is not in scope.
importSuggestions :: LookingFor
                  -> InteractiveContext -> Module
                  -> ImportAvails -> RdrName -> ([ImportError], [ImportSuggestion])
importSuggestions looking_for ic currMod imports rdr_name
  | WL_LocalOnly <- lf_where looking_for       = ([], [])
  | WL_LocalTop  <- lf_where looking_for       = ([], [])
  | not (isQual rdr_name || isUnqual rdr_name) = ([], [])
  | Just name <- mod_name
  , show_not_imported_line name
  = ([MissingModule name], [])
  | is_qualified
  , null helpful_imports
  , (mod : mods) <- map fst interesting_imports
  = ([ModulesDoNotExport (mod :| mods) (lf_which looking_for) occ_name], [])
  | mod : mods <- helpful_imports_non_hiding
  = ([], [CouldImportFrom (mod :| mods)])
  | mod : mods <- helpful_imports_hiding
  = ([], [CouldUnhideFrom (mod :| mods)])
  | otherwise
  = ([], [])
 where
  is_qualified = isQual rdr_name
  (mod_name, occ_name) = case rdr_name of
    Unqual occ_name        -> (Nothing, occ_name)
    Qual mod_name occ_name -> (Just mod_name, occ_name)
    _                      -> panic "importSuggestions: dead code"


  -- What import statements provide "Mod" at all
  -- or, if this is an unqualified name, are not qualified imports
  interesting_imports = [ (mod, imp)
    | (mod, mod_imports) <- M.toList (imp_mods imports)
    , Just imp <- return $ pick (importedByUser mod_imports)
    ]

  -- Choose the imports from the interactive context which might have provided
  -- a module.
  interactive_imports =
    filter pick_interactive (ic_imports ic)

  pick_interactive :: InteractiveImport -> Bool
  pick_interactive (IIDecl d)   | mod_name == Just (unLoc (ideclName d)) = True
                                | mod_name == fmap unLoc (ideclAs d) = True
  pick_interactive (IIModule m) | mod_name == Just m = True
  pick_interactive _ = False

  -- We want to keep only one for each original module; preferably one with an
  -- explicit import list (for no particularly good reason)
  pick :: [ImportedModsVal] -> Maybe ImportedModsVal
  pick = listToMaybe . sortBy cmp . filter select
    where select imv = case mod_name of Just name -> imv_name imv == name
                                        Nothing   -> not (imv_qualified imv)
          cmp = on compare imv_is_hiding S.<> on SrcLoc.leftmost_smallest imv_span

  -- Which of these would export a 'foo'
  -- (all of these are restricted imports, because if they were not, we
  -- wouldn't have an out-of-scope error in the first place)
  helpful_imports = filter helpful interesting_imports
    where helpful (_,imv)
            = any (isGreOk looking_for) $
              lookupGRE (imv_all_exports imv)
                (LookupOccName occ_name $ RelevantGREsFOS WantNormal)

  -- Which of these do that because of an explicit hiding list resp. an
  -- explicit import list
  (helpful_imports_hiding, helpful_imports_non_hiding)
    = partition (imv_is_hiding . snd) helpful_imports

  -- See Note [When to show/hide the module-not-imported line]
  show_not_imported_line :: ModuleName -> Bool                    -- #15611
  show_not_imported_line modnam
      | not (null interactive_imports)        = False -- 1 (interactive context)
      | not (null interesting_imports)        = False -- 1 (normal module import)
      | moduleName currMod == modnam          = False -- 2
      | otherwise                             = True

extensionSuggestions :: RdrName -> [GhcHint]
extensionSuggestions rdrName
  | rdrName == mkUnqual varName (fsLit "mdo") ||
    rdrName == mkUnqual varName (fsLit "rec")
  = [SuggestExtension $ SuggestSingleExtension empty LangExt.RecursiveDo]
  | otherwise
  = []

qualsInScope :: GlobalRdrElt -> [(ModuleName, HowInScope)]
-- Ones for which the qualified version is in scope
qualsInScope gre@GRE { gre_lcl = lcl, gre_imp = is }
      | lcl = case greDefinitionModule gre of
                Nothing -> []
                Just m  -> [(moduleName m, LocallyBoundAt (greDefinitionSrcSpan gre))]
      | otherwise = [ (is_as ispec, ImportedBy ispec)
                    | i <- bagToList is, let ispec = is_decl i ]

isGreOk :: LookingFor -> GlobalRdrElt -> Bool
isGreOk (LF what_look where_look) gre = what_ok && where_ok
  where
    -- when looking for record fields, what_ok checks whether the GRE is a
    -- record field. Otherwise, it checks whether the GRE is a record field
    -- defined in a module with -XNoFieldSelectors - it wouldn't be a useful
    -- suggestion in that case.
    what_ok  = case what_look of
                 WL_RecField -> isRecFldGRE gre
                 _           -> not (isNoFieldSelectorGRE gre)

    where_ok = case where_look of
                 WL_LocalTop  -> isLocalGRE gre
                 WL_LocalOnly -> False
                 _            -> True

-- see Note [Related name spaces]
nameSpacesRelated :: DynFlags    -- ^ to find out whether -XDataKinds is enabled
                  -> WhatLooking -- ^ What kind of name are we looking for
                  -> NameSpace   -- ^ Name space of the original name
                  -> NameSpace   -- ^ Name space of a name that might have been meant
                  -> Bool
nameSpacesRelated dflags what_looking ns ns'
  | ns == ns'
  = True
  | otherwise
  = or [ other_ns ns'
       | (orig_ns, others) <- other_namespaces
       , orig_ns ns
       , (other_ns, wls) <- others
       , what_looking `elem` WL_Anything : wls
       ]
  where
    -- explanation:
    -- [(orig_ns, [(other_ns, what_looking_possibilities)])]
    -- A particular other_ns is related if the original namespace is orig_ns
    -- and what_looking is either WL_Anything or is one of
    -- what_looking_possibilities
    other_namespaces =
      [ (isVarNameSpace     , [(isFieldNameSpace  , [WL_Term, WL_RecField, WL_NotConLike])
                              ,(isDataConNameSpace, [WL_Term, WL_ConLike, WL_Constructor])])
      , (isDataConNameSpace , [(isVarNameSpace    , [WL_Term, WL_RecField, WL_NotConLike])])
      , (isTvNameSpace      , [(isTcClsNameSpace  , [WL_Constructor, WL_NotConLike])
                              ,(isVarNameSpace    , [WL_Term, WL_NotConLike])
                              ,(isFieldNameSpace  , [WL_Term, WL_NotConLike])
                              ] ++ promoted_datacons)
      , (isTcClsNameSpace   , [(isTvNameSpace     , [WL_NotConLike])
                              ,(isDataConNameSpace, [WL_Term, WL_ConLike])
                              ,(isVarNameSpace    , [WL_Term, WL_NotConLike]) -- for symbolic identifiers
                              ,(isFieldNameSpace  , [WL_Term, WL_NotConLike]) -- such as + or :*
                              ]
                              ++ promoted_datacons)
      ]
    -- If -XDataKinds is enabled, the data constructor name space is also
    -- related to the type-level name spaces
    data_kinds = xopt LangExt.DataKinds dflags
    promoted_datacons = [(isDataConNameSpace, [WL_Constructor]) | data_kinds]

{- Note [Related name spaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Name spaces are related if there is a chance to mean the one when one writes
the other, i.e. variables <-> data constructors and type variables <-> type
constructors.

In most contexts, this mistake can happen in both directions. Not so in
patterns:

When a user writes
        foo (just a) = ...
It is possible that they meant to use `Just` instead. However, when they write
        foo (Map a) = ...
It is unlikely that they mean to use `map`, since variables cannot be used here.

Similarly, when we look for record fields, data constructors are not in a
related namespace.

Furthermore, with -XDataKinds, the data constructor name space is related to
the type variable and type constructor name spaces.

Note [When to show/hide the module-not-imported line]           -- #15611
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the error message:
    Not in scope X.Y
    Module X does not export Y
    No module named ‘X’ is imported:
there are 2 cases, where we hide the last "no module is imported" line:
1. If the module X has been imported (normally or via interactive context).
2. It is the current module we are trying to compile
   then we can use the getModule function to get the current module name.
   (See test T15611a)
-}
