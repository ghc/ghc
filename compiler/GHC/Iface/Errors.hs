
{-# LANGUAGE FlexibleContexts #-}

module GHC.Iface.Errors
  ( badIfaceFile
  , hiModuleNameMismatchWarn
  , homeModError
  , cannotFindInterface
  , cantFindInstalledErr
  , cannotFindModule
  , cantFindErr
  -- * Utility functions
  , mayShowLocations
  ) where

import GHC.Platform.Profile
import GHC.Platform.Ways
import GHC.Utils.Panic.Plain
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Data.Maybe
import GHC.Prelude
import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder.Types
import GHC.Utils.Outputable as Outputable


badIfaceFile :: String -> SDoc -> SDoc
badIfaceFile file err
  = vcat [text "Bad interface file:" <+> text file,
          nest 4 err]

hiModuleNameMismatchWarn :: Module -> Module -> SDoc
hiModuleNameMismatchWarn requested_mod read_mod
 | moduleUnit requested_mod == moduleUnit read_mod =
    sep [text "Interface file contains module" <+> quotes (ppr read_mod) <> comma,
         text "but we were expecting module" <+> quotes (ppr requested_mod),
         sep [text "Probable cause: the source code which generated interface file",
             text "has an incompatible module name"
            ]
        ]
 | otherwise =
  -- ToDo: This will fail to have enough qualification when the package IDs
  -- are the same
  withPprStyle (mkUserStyle alwaysQualify AllTheWay) $
    -- we want the Modules below to be qualified with package names,
    -- so reset the PrintUnqualified setting.
    hsep [ text "Something is amiss; requested module "
         , ppr requested_mod
         , text "differs from name found in the interface file"
         , ppr read_mod
         , parens (text "if these names look the same, try again with -dppr-debug")
         ]

homeModError :: InstalledModule -> ModLocation -> SDoc
-- See Note [Home module load error]
homeModError mod location
  = text "attempting to use module " <> quotes (ppr mod)
    <> (case ml_hs_file location of
           Just file -> space <> parens (text file)
           Nothing   -> Outputable.empty)
    <+> text "which is not loaded"


-- -----------------------------------------------------------------------------
-- Error messages

cannotFindInterface :: UnitState -> Maybe HomeUnit -> Profile -> ([FilePath] -> SDoc) -> ModuleName -> InstalledFindResult -> SDoc
cannotFindInterface = cantFindInstalledErr (text "Failed to load interface for")
                                           (text "Ambiguous interface for")

cantFindInstalledErr
    :: SDoc
    -> SDoc
    -> UnitState
    -> Maybe HomeUnit
    -> Profile
    -> ([FilePath] -> SDoc)
    -> ModuleName
    -> InstalledFindResult
    -> SDoc
cantFindInstalledErr cannot_find _ unit_state mhome_unit profile tried_these mod_name find_result
  = cannot_find <+> quotes (ppr mod_name)
    $$ more_info
  where
    build_tag  = waysBuildTag (profileWays profile)

    more_info
      = case find_result of
            InstalledNoPackage pkg
                -> text "no unit id matching" <+> quotes (ppr pkg) <+>
                   text "was found" $$ looks_like_srcpkgid pkg

            InstalledNotFound files mb_pkg
                | Just pkg <- mb_pkg
                , notHomeUnitId mhome_unit pkg
                -> not_found_in_package pkg files

                | null files
                -> text "It is not a module in the current program, or in any known package."

                | otherwise
                -> tried_these files

            _ -> panic "cantFindInstalledErr"

    looks_like_srcpkgid :: UnitId -> SDoc
    looks_like_srcpkgid pk
     -- Unsafely coerce a unit id (i.e. an installed package component
     -- identifier) into a PackageId and see if it means anything.
     | (pkg:pkgs) <- searchPackageId unit_state (PackageId (unitIdFS pk))
     = parens (text "This unit ID looks like the source package ID;" $$
       text "the real unit ID is" <+> quotes (ftext (unitIdFS (unitId pkg))) $$
       (if null pkgs then Outputable.empty
        else text "and" <+> int (length pkgs) <+> text "other candidates"))
     -- Todo: also check if it looks like a package name!
     | otherwise = Outputable.empty

    not_found_in_package pkg files
       | build_tag /= ""
       = let
            build = if build_tag == "p" then "profiling"
                                        else "\"" ++ build_tag ++ "\""
         in
         text "Perhaps you haven't installed the " <> text build <>
         text " libraries for package " <> quotes (ppr pkg) <> char '?' $$
         tried_these files

       | otherwise
       = text "There are files missing in the " <> quotes (ppr pkg) <>
         text " package," $$
         text "try running 'ghc-pkg check'." $$
         tried_these files

mayShowLocations :: DynFlags -> [FilePath] -> SDoc
mayShowLocations dflags files
    | null files = Outputable.empty
    | verbosity dflags < 3 =
          text "Use -v (or `:set -v` in ghci) " <>
              text "to see a list of the files searched for."
    | otherwise =
          hang (text "Locations searched:") 2 $ vcat (map text files)

cannotFindModule :: HscEnv -> ModuleName -> FindResult -> SDoc
cannotFindModule hsc_env = cannotFindModule'
    (hsc_dflags   hsc_env)
    (hsc_unit_env hsc_env)
    (targetProfile (hsc_dflags hsc_env))


cannotFindModule' :: DynFlags -> UnitEnv -> Profile -> ModuleName -> FindResult -> SDoc
cannotFindModule' dflags unit_env profile mod res = pprWithUnitState (ue_units unit_env) $
  cantFindErr (checkBuildingCabalPackage dflags)
              cannotFindMsg
              (text "Ambiguous module name")
              unit_env
              profile
              (mayShowLocations dflags)
              mod
              res
  where
    cannotFindMsg =
      case res of
        NotFound { fr_mods_hidden = hidden_mods
                 , fr_pkgs_hidden = hidden_pkgs
                 , fr_unusables = unusables }
          | not (null hidden_mods && null hidden_pkgs && null unusables)
          -> text "Could not load module"
        _ -> text "Could not find module"

cantFindErr
    :: BuildingCabalPackage -- ^ Using Cabal?
    -> SDoc
    -> SDoc
    -> UnitEnv
    -> Profile
    -> ([FilePath] -> SDoc)
    -> ModuleName
    -> FindResult
    -> SDoc
cantFindErr _ _ multiple_found _ _ _ mod_name (FoundMultiple mods)
  | Just pkgs <- unambiguousPackages
  = hang (multiple_found <+> quotes (ppr mod_name) <> colon) 2 (
       sep [text "it was found in multiple packages:",
                hsep (map ppr pkgs) ]
    )
  | otherwise
  = hang (multiple_found <+> quotes (ppr mod_name) <> colon) 2 (
       vcat (map pprMod mods)
    )
  where
    unambiguousPackages = foldl' unambiguousPackage (Just []) mods
    unambiguousPackage (Just xs) (m, ModOrigin (Just _) _ _ _)
        = Just (moduleUnit m : xs)
    unambiguousPackage _ _ = Nothing

    pprMod (m, o) = text "it is bound as" <+> ppr m <+>
                                text "by" <+> pprOrigin m o
    pprOrigin _ ModHidden = panic "cantFindErr: bound by mod hidden"
    pprOrigin _ (ModUnusable _) = panic "cantFindErr: bound by mod unusable"
    pprOrigin m (ModOrigin e res _ f) = sep $ punctuate comma (
      if e == Just True
          then [text "package" <+> ppr (moduleUnit m)]
          else [] ++
      map ((text "a reexport in package" <+>)
                .ppr.mkUnit) res ++
      if f then [text "a package flag"] else []
      )

cantFindErr using_cabal cannot_find _ unit_env profile tried_these mod_name find_result
  = cannot_find <+> quotes (ppr mod_name)
    $$ more_info
  where
    mhome_unit = ue_homeUnit unit_env
    more_info
      = case find_result of
            NoPackage pkg
                -> text "no unit id matching" <+> quotes (ppr pkg) <+>
                   text "was found"

            NotFound { fr_paths = files, fr_pkg = mb_pkg
                     , fr_mods_hidden = mod_hiddens, fr_pkgs_hidden = pkg_hiddens
                     , fr_unusables = unusables, fr_suggestions = suggest }
                | Just pkg <- mb_pkg
                , Nothing <- mhome_unit           -- no home-unit
                -> not_found_in_package pkg files

                | Just pkg <- mb_pkg
                , Just home_unit <- mhome_unit    -- there is a home-unit but the
                , not (isHomeUnit home_unit pkg)  -- module isn't from it
                -> not_found_in_package pkg files

                | not (null suggest)
                -> pp_suggestions suggest $$ tried_these files

                | null files && null mod_hiddens &&
                  null pkg_hiddens && null unusables
                -> text "It is not a module in the current program, or in any known package."

                | otherwise
                -> vcat (map pkg_hidden pkg_hiddens) $$
                   vcat (map mod_hidden mod_hiddens) $$
                   vcat (map unusable unusables) $$
                   tried_these files

            _ -> panic "cantFindErr"

    build_tag = waysBuildTag (profileWays profile)

    not_found_in_package pkg files
       | build_tag /= ""
       = let
            build = if build_tag == "p" then "profiling"
                                        else "\"" ++ build_tag ++ "\""
         in
         text "Perhaps you haven't installed the " <> text build <>
         text " libraries for package " <> quotes (ppr pkg) <> char '?' $$
         tried_these files

       | otherwise
       = text "There are files missing in the " <> quotes (ppr pkg) <>
         text " package," $$
         text "try running 'ghc-pkg check'." $$
         tried_these files

    pkg_hidden :: Unit -> SDoc
    pkg_hidden uid =
        text "It is a member of the hidden package"
        <+> quotes (ppr uid)
        --FIXME: we don't really want to show the unit id here we should
        -- show the source package id or installed package id if it's ambiguous
        <> dot $$ pkg_hidden_hint uid

    pkg_hidden_hint uid
     | using_cabal == YesBuildingCabalPackage
        = let pkg = expectJust "pkg_hidden" (lookupUnit (ue_units unit_env) uid)
           in text "Perhaps you need to add" <+>
              quotes (ppr (unitPackageName pkg)) <+>
              text "to the build-depends in your .cabal file."
     | Just pkg <- lookupUnit (ue_units unit_env) uid
         = text "You can run" <+>
           quotes (text ":set -package " <> ppr (unitPackageName pkg)) <+>
           text "to expose it." $$
           text "(Note: this unloads all the modules in the current scope.)"
     | otherwise = Outputable.empty

    mod_hidden pkg =
        text "it is a hidden module in the package" <+> quotes (ppr pkg)

    unusable (pkg, reason)
      = text "It is a member of the package"
      <+> quotes (ppr pkg)
      $$ pprReason (text "which is") reason

    pp_suggestions :: [ModuleSuggestion] -> SDoc
    pp_suggestions sugs
      | null sugs = Outputable.empty
      | otherwise = hang (text "Perhaps you meant")
                       2 (vcat (map pp_sugg sugs))

    -- NB: Prefer the *original* location, and then reexports, and then
    -- package flags when making suggestions.  ToDo: if the original package
    -- also has a reexport, prefer that one
    pp_sugg (SuggestVisible m mod o) = ppr m <+> provenance o
      where provenance ModHidden = Outputable.empty
            provenance (ModUnusable _) = Outputable.empty
            provenance (ModOrigin{ fromOrigUnit = e,
                                   fromExposedReexport = res,
                                   fromPackageFlag = f })
              | Just True <- e
                 = parens (text "from" <+> ppr (moduleUnit mod))
              | f && moduleName mod == m
                 = parens (text "from" <+> ppr (moduleUnit mod))
              | (pkg:_) <- res
                 = parens (text "from" <+> ppr (mkUnit pkg)
                    <> comma <+> text "reexporting" <+> ppr mod)
              | f
                 = parens (text "defined via package flags to be"
                    <+> ppr mod)
              | otherwise = Outputable.empty
    pp_sugg (SuggestHidden m mod o) = ppr m <+> provenance o
      where provenance ModHidden =  Outputable.empty
            provenance (ModUnusable _) = Outputable.empty
            provenance (ModOrigin{ fromOrigUnit = e,
                                   fromHiddenReexport = rhs })
              | Just False <- e
                 = parens (text "needs flag -package-id"
                    <+> ppr (moduleUnit mod))
              | (pkg:_) <- rhs
                 = parens (text "needs flag -package-id"
                    <+> ppr (mkUnit pkg))
              | otherwise = Outputable.empty

