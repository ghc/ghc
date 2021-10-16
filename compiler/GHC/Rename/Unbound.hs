{-

This module contains helper functions for reporting and creating
unbound variables.

-}
module GHC.Rename.Unbound
   ( mkUnboundName
   , mkUnboundNameRdr
   , isUnboundName
   , reportUnboundName
   , reportUnboundName'
   , unknownNameSuggestions
   , WhatLooking(..)
   , WhereLooking(..)
   , LookingFor(..)
   , unboundName
   , unboundNameX
   , notInScopeErr
   , nameSpacesRelated
   , exactNameErr
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Ppr

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Builtin.Names ( mkUnboundName, isUnboundName, getUnique)
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Misc

import GHC.Data.Maybe
import GHC.Data.FastString

import qualified GHC.LanguageExtensions as LangExt

import GHC.Types.Error
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Unique.DFM (udfmToList)

import GHC.Unit.Module
import GHC.Unit.Module.Imported
import GHC.Unit.Home.ModInfo

import Data.List (sortBy, partition, nub)
import Data.Function ( on )

{-
************************************************************************
*                                                                      *
               What to do when a lookup fails
*                                                                      *
************************************************************************
-}

-- What kind of suggestion are we looking for? #19843
data WhatLooking = WL_Anything    -- Any binding
                 | WL_Constructor -- Constructors and pattern synonyms
                        -- E.g. in K { f1 = True }, if K is not in scope,
                        -- suggest only constructors
                 | WL_RecField    -- Record fields
                        -- E.g. in K { f1 = True, f2 = False }, if f2 is not in
                        -- scope, suggest only constructor fields
                 | WL_None        -- No suggestions
                        -- WS_None is used for rebindable syntax, where there
                        -- is no point in suggesting alternative spellings
                 deriving Eq

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

mkUnboundNameRdr :: RdrName -> Name
mkUnboundNameRdr rdr = mkUnboundName (rdrNameOcc rdr)

reportUnboundName' :: WhatLooking -> RdrName -> RnM Name
reportUnboundName' what_look rdr = unboundName (LF what_look WL_Anywhere) rdr

reportUnboundName :: RdrName -> RnM Name
reportUnboundName = reportUnboundName' WL_Anything

unboundName :: LookingFor -> RdrName -> RnM Name
unboundName lf rdr = unboundNameX lf rdr Outputable.empty

unboundNameX :: LookingFor -> RdrName -> SDoc -> RnM Name
unboundNameX looking_for rdr_name extra
  = do  { dflags <- getDynFlags
        ; let show_helpful_errors = gopt Opt_HelpfulErrors dflags
              err = notInScopeErr (lf_where looking_for) rdr_name $$ extra
        ; if not show_helpful_errors
          then addErr (TcRnUnknownMessage $ mkPlainError noHints err)
          else do { local_env  <- getLocalRdrEnv
                  ; global_env <- getGlobalRdrEnv
                  ; impInfo <- getImports
                  ; currmod <- getModule
                  ; hpt <- getHpt
                  ; let suggestions = unknownNameSuggestions_ looking_for
                          dflags hpt currmod global_env local_env impInfo
                          rdr_name
                  ; addErr (TcRnUnknownMessage $ mkPlainError noHints (err $$ suggestions)) }
        ; return (mkUnboundNameRdr rdr_name) }

notInScopeErr :: WhereLooking -> RdrName -> SDoc
notInScopeErr where_look rdr_name
  | Just name <- isExact_maybe rdr_name = exactNameErr name
  | WL_LocalTop <- where_look = hang (text "No top-level binding for")
      2 (what <+> quotes (ppr rdr_name) <+> text "in this module")
  | otherwise = hang (text "Not in scope:")
                 2 (what <+> quotes (ppr rdr_name))
  where
    what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))

type HowInScope = Either SrcSpan ImpDeclSpec
     -- Left loc    =>  locally bound at loc
     -- Right ispec =>  imported as specified by ispec


-- | Called from the typechecker ("GHC.Tc.Errors") when we find an unbound variable
unknownNameSuggestions :: WhatLooking -> DynFlags
                       -> HomePackageTable -> Module
                       -> GlobalRdrEnv -> LocalRdrEnv -> ImportAvails
                       -> RdrName -> SDoc
unknownNameSuggestions what_look = unknownNameSuggestions_ (LF what_look WL_Anywhere)

unknownNameSuggestions_ :: LookingFor -> DynFlags
                       -> HomePackageTable -> Module
                       -> GlobalRdrEnv -> LocalRdrEnv -> ImportAvails
                       -> RdrName -> SDoc
unknownNameSuggestions_ looking_for dflags hpt curr_mod global_env local_env
                          imports tried_rdr_name =
    similarNameSuggestions looking_for dflags global_env local_env tried_rdr_name $$
    importSuggestions looking_for global_env hpt
                      curr_mod imports tried_rdr_name $$
    extensionSuggestions tried_rdr_name $$
    fieldSelectorSuggestions global_env tried_rdr_name

-- | When the name is in scope as field whose selector has been suppressed by
-- NoFieldSelectors, display a helpful message explaining this.
fieldSelectorSuggestions :: GlobalRdrEnv -> RdrName -> SDoc
fieldSelectorSuggestions global_env tried_rdr_name
  | null gres = Outputable.empty
  | otherwise = text "NB:"
      <+> quotes (ppr tried_rdr_name)
      <+> text "is a field selector" <+> whose
      $$ text "that has been suppressed by NoFieldSelectors"
  where
    gres = filter isNoFieldSelectorGRE $
               lookupGRE_RdrName' tried_rdr_name global_env
    parents = [ parent | ParentIs parent <- map gre_par gres ]

    -- parents may be empty if this is a pattern synonym field without a selector
    whose | null parents = empty
          | otherwise    = text "belonging to the type" <> plural parents
                             <+> pprQuotedList parents

similarNameSuggestions :: LookingFor -> DynFlags
                       -> GlobalRdrEnv -> LocalRdrEnv
                       -> RdrName -> SDoc
similarNameSuggestions looking_for@(LF what_look where_look) dflags global_env
                       local_env tried_rdr_name
  = case suggest of
      []  -> Outputable.empty
      [p] -> perhaps <+> pp_item p
      ps  -> sep [ perhaps <+> text "one of these:"
                 , nest 2 (pprWithCommas pp_item ps) ]
  where
    all_possibilities :: [(String, (RdrName, HowInScope))]
    all_possibilities = case what_look of
      WL_None -> []
      _ -> [ (showPpr dflags r, (r, Left loc))
           | (r,loc) <- local_possibilities local_env ]
        ++ [ (showPpr dflags r, rp) | (r, rp) <- global_possibilities global_env ]

    suggest = fuzzyLookup (showPpr dflags tried_rdr_name) all_possibilities
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
              | otherwise      = Outputable.empty
      where ns = rdrNameSpace rdr

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

    global_possibilities :: GlobalRdrEnv -> [(RdrName, (RdrName, HowInScope))]
    global_possibilities global_env
      | tried_is_qual = [ (rdr_qual, (rdr_qual, how))
                        | gre <- globalRdrEnvElts global_env
                        , isGreOk looking_for gre
                        , let occ = greOccName gre
                        , correct_name_space occ
                        , (mod, how) <- qualsInScope gre
                        , let rdr_qual = mkRdrQual mod occ ]

      | otherwise = [ (rdr_unqual, pair)
                    | gre <- globalRdrEnvElts global_env
                    , isGreOk looking_for gre
                    , let occ = greOccName gre
                          rdr_unqual = mkRdrUnqual occ
                    , correct_name_space occ
                    , pair <- case (unquals_in_scope gre, quals_only gre) of
                                (how:_, _)    -> [ (rdr_unqual, how) ]
                                ([],    pr:_) -> [ pr ]  -- See Note [Only-quals]
                                ([],    [])   -> [] ]

              -- Note [Only-quals]
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
      | lcl       = [ Left (greDefinitionSrcSpan gre) ]
      | otherwise = [ Right ispec
                    | i <- is, let ispec = is_decl i
                    , not (is_qual ispec) ]


    --------------------
    quals_only :: GlobalRdrElt -> [(RdrName, HowInScope)]
    -- Ones for which *only* the qualified version is in scope
    quals_only (gre@GRE { gre_imp = is })
      = [ (mkRdrQual (is_as ispec) (greOccName gre), Right ispec)
        | i <- is, let ispec = is_decl i, is_qual ispec ]

-- | Generate helpful suggestions if a qualified name Mod.foo is not in scope.
importSuggestions :: LookingFor
                  -> GlobalRdrEnv
                  -> HomePackageTable -> Module
                  -> ImportAvails -> RdrName -> SDoc
importSuggestions looking_for global_env hpt currMod imports rdr_name
  | WL_LocalOnly <- lf_where looking_for       = Outputable.empty
  | WL_LocalTop  <- lf_where looking_for       = Outputable.empty
  | not (isQual rdr_name || isUnqual rdr_name) = Outputable.empty
  | null interesting_imports
  , Just name <- mod_name
  , show_not_imported_line name
  = hsep
      [ text "No module named"
      , quotes (ppr name)
      , text "is imported."
      ]
  | is_qualified
  , null helpful_imports
  , [(mod,_)] <- interesting_imports
  = hsep
      [ text "Module"
      , quotes (ppr mod)
      , text "does not export"
      , quotes (ppr occ_name) <> dot
      ]
  | is_qualified
  , null helpful_imports
  , not (null interesting_imports)
  , mods <- map fst interesting_imports
  = hsep
      [ text "Neither"
      , quotedListWithNor (map ppr mods)
      , text "exports"
      , quotes (ppr occ_name) <> dot
      ]
  | [(mod,imv)] <- helpful_imports_non_hiding
  = fsep
      [ text "Perhaps you want to add"
      , quotes (ppr occ_name)
      , text "to the import list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (ppr (imv_span imv)) <> dot
      ]
  | not (null helpful_imports_non_hiding)
  = fsep
      [ text "Perhaps you want to add"
      , quotes (ppr occ_name)
      , text "to one of these import lists:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (ppr (imv_span imv))
        | (mod,imv) <- helpful_imports_non_hiding
        ])
  | [(mod,imv)] <- helpful_imports_hiding
  = fsep
      [ text "Perhaps you want to remove"
      , quotes (ppr occ_name)
      , text "from the explicit hiding list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (ppr (imv_span imv)) <> dot
      ]
  | not (null helpful_imports_hiding)
  = fsep
      [ text "Perhaps you want to remove"
      , quotes (ppr occ_name)
      , text "from the hiding clauses"
      , text "in one of these imports:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (ppr (imv_span imv))
        | (mod,imv) <- helpful_imports_hiding
        ])
  | otherwise
  = Outputable.empty
 where
  is_qualified = isQual rdr_name
  (mod_name, occ_name) = case rdr_name of
    Unqual occ_name        -> (Nothing, occ_name)
    Qual mod_name occ_name -> (Just mod_name, occ_name)
    _                      -> error "importSuggestions: dead code"


  -- What import statements provide "Mod" at all
  -- or, if this is an unqualified name, are not qualified imports
  interesting_imports = [ (mod, imp)
    | (mod, mod_imports) <- moduleEnvToList (imp_mods imports)
    , Just imp <- return $ pick (importedByUser mod_imports)
    ]

  -- We want to keep only one for each original module; preferably one with an
  -- explicit import list (for no particularly good reason)
  pick :: [ImportedModsVal] -> Maybe ImportedModsVal
  pick = listToMaybe . sortBy cmp . filter select
    where select imv = case mod_name of Just name -> imv_name imv == name
                                        Nothing   -> not (imv_qualified imv)
          cmp a b =
            (compare `on` imv_is_hiding) a b
              `thenCmp`
            (SrcLoc.leftmost_smallest `on` imv_span) a b

  -- Which of these would export a 'foo'
  -- (all of these are restricted imports, because if they were not, we
  -- wouldn't have an out-of-scope error in the first place)
  helpful_imports = filter helpful interesting_imports
    where helpful (_,imv)
            = any (isGreOk looking_for) $ greEntryToList $
              lookupGlobalRdrEnv (imv_all_exports imv) occ_name

  -- Which of these do that because of an explicit hiding list resp. an
  -- explicit import list
  (helpful_imports_hiding, helpful_imports_non_hiding)
    = partition (imv_is_hiding . snd) helpful_imports

  -- See note [When to show/hide the module-not-imported line]
  show_not_imported_line :: ModuleName -> Bool                    -- #15611
  show_not_imported_line modnam
      | modnam `elem` glob_mods               = False    -- #14225     -- 1
      | moduleName currMod == modnam          = False                  -- 2.1
      | is_last_loaded_mod modnam hpt_uniques = False                  -- 2.2
      | otherwise                             = True
    where
      hpt_uniques = map fst (udfmToList hpt)
      is_last_loaded_mod _ []         = False
      is_last_loaded_mod modnam uniqs = last uniqs == getUnique modnam
      glob_mods = nub [ mod
                     | gre <- globalRdrEnvElts global_env
                     , (mod, _) <- qualsInScope gre
                     ]

extensionSuggestions :: RdrName -> SDoc
extensionSuggestions rdrName
  | rdrName == mkUnqual varName (fsLit "mdo") ||
    rdrName == mkUnqual varName (fsLit "rec")
      = text "Perhaps you meant to use RecursiveDo"
  | otherwise = Outputable.empty

qualsInScope :: GlobalRdrElt -> [(ModuleName, HowInScope)]
-- Ones for which the qualified version is in scope
qualsInScope gre@GRE { gre_lcl = lcl, gre_imp = is }
      | lcl = case greDefinitionModule gre of
                Nothing -> []
                Just m  -> [(moduleName m, Left (greDefinitionSrcSpan gre))]
      | otherwise = [ (is_as ispec, Right ispec)
                    | i <- is, let ispec = is_decl i ]

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
  = ns' `elem` ns : [ other_ns
                    | (orig_ns, others) <- other_namespaces
                    , ns == orig_ns
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
      [ (varName  , [(dataName, [WL_Constructor])])
      , (dataName , [(varName , [WL_RecField])])
      , (tvName   , (tcClsName, [WL_Constructor]) : promoted_datacons)
      , (tcClsName, (tvName   , []) : promoted_datacons)
      ]
    -- If -XDataKinds is enabled, the data constructor name space is also
    -- related to the type-level name spaces
    data_kinds = xopt LangExt.DataKinds dflags
    promoted_datacons = [(dataName, [WL_Constructor]) | data_kinds]

{-
Note [Related name space]
~~~~~~~~~~~~~~~~~~~~~~~~~
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
  hang (text "The exact Name" <+> quotes (ppr name) <+> text "is not in scope")
    2 (vcat [ text "Probable cause: you used a unique Template Haskell name (NameU), "
            , text "perhaps via newName, but did not bind it"
            , text "If that's it, then -ddump-splices might be useful" ])
