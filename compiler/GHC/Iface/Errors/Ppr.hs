{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic TcRnMessage
{-# LANGUAGE InstanceSigs #-}

module GHC.Iface.Errors.Ppr
  ( IfaceMessageOpts(..)
  , interfaceErrorHints
  , interfaceErrorReason
  , interfaceErrorDiagnostic
  , missingInterfaceErrorHints
  , missingInterfaceErrorReason
  , missingInterfaceErrorDiagnostic
  , readInterfaceErrorDiagnostic
  )
  where

import GHC.Prelude

import GHC.Types.Error
import GHC.Types.Hint.Ppr () -- Outputable GhcHint
import GHC.Types.Error.Codes ( constructorCode )
import GHC.Types.Name
import GHC.Types.TyThing

import GHC.Unit.State
import GHC.Unit.Module

import GHC.Utils.Outputable
import GHC.Utils.Panic



import GHC.Iface.Errors.Types

data IfaceMessageOpts = IfaceMessageOpts { ifaceShowTriedFiles :: !Bool -- ^ Whether to show files we tried to look for or not when printing loader errors
                                         }

defaultIfaceMessageOpts :: IfaceMessageOpts
defaultIfaceMessageOpts = IfaceMessageOpts { ifaceShowTriedFiles = False }


instance Diagnostic IfaceMessage where
  type DiagnosticOpts IfaceMessage = IfaceMessageOpts
  defaultDiagnosticOpts = defaultIfaceMessageOpts
  diagnosticMessage opts reason = mkSimpleDecorated $
         interfaceErrorDiagnostic opts reason

  diagnosticReason = interfaceErrorReason

  diagnosticHints = interfaceErrorHints

  diagnosticCode = constructorCode

interfaceErrorHints :: IfaceMessage -> [GhcHint]
interfaceErrorHints = \ case
  Can'tFindInterface err _looking_for ->
    missingInterfaceErrorHints err
  Can'tFindNameInInterface {} ->
    noHints

missingInterfaceErrorHints :: MissingInterfaceError -> [GhcHint]
missingInterfaceErrorHints = \case
  BadSourceImport {} ->
    noHints
  HomeModError {} ->
    noHints
  DynamicHashMismatchError {} ->
    noHints
  CantFindErr {} ->
    noHints
  BadIfaceFile {} ->
    noHints
  FailedToLoadDynamicInterface {} ->
    noHints

interfaceErrorReason :: IfaceMessage -> DiagnosticReason
interfaceErrorReason (Can'tFindInterface err _)
  = missingInterfaceErrorReason err
interfaceErrorReason (Can'tFindNameInInterface {})
  = ErrorWithoutFlag

missingInterfaceErrorReason :: MissingInterfaceError -> DiagnosticReason
missingInterfaceErrorReason = \ case
  BadSourceImport {} ->
    ErrorWithoutFlag
  HomeModError {} ->
    ErrorWithoutFlag
  DynamicHashMismatchError {} ->
    ErrorWithoutFlag
  CantFindErr {} ->
    ErrorWithoutFlag
  BadIfaceFile {} ->
    ErrorWithoutFlag
  FailedToLoadDynamicInterface {} ->
    ErrorWithoutFlag


prettyCantFindWhat :: FindOrLoad -> FindingModuleOrInterface -> AmbiguousOrMissing -> SDoc
prettyCantFindWhat Find FindingModule AoM_Missing = text "Could not find module"
prettyCantFindWhat Load FindingModule AoM_Missing = text "Could not load module"
prettyCantFindWhat _ FindingInterface AoM_Missing = text "Failed to load interface for"
prettyCantFindWhat _ FindingModule AoM_Ambiguous = text "Ambiguous module name"
prettyCantFindWhat _ FindingInterface AoM_Ambiguous = text "Ambiguous interface for"

isAmbiguousInstalledReason :: CantFindInstalledReason -> AmbiguousOrMissing
isAmbiguousInstalledReason (MultiplePackages {}) = AoM_Ambiguous
isAmbiguousInstalledReason _ = AoM_Missing

isLoadOrFindReason :: CantFindInstalledReason -> FindOrLoad
isLoadOrFindReason NotAModule {} = Find
isLoadOrFindReason (GenericMissing _ a b c _) | null a && null b && null c = Find
isLoadOrFindReason (ModuleSuggestion {}) = Find
isLoadOrFindReason _ = Load

data FindOrLoad  = Find | Load

data AmbiguousOrMissing = AoM_Ambiguous | AoM_Missing

cantFindError :: IfaceMessageOpts -> FindingModuleOrInterface -> CantFindInstalled -> SDoc
cantFindError opts mod_or_interface (CantFindInstalled mod_name cfir) =
  let ambig = isAmbiguousInstalledReason cfir
      find_or_load = isLoadOrFindReason cfir
      ppr_what = prettyCantFindWhat find_or_load mod_or_interface ambig
  in
  (ppr_what <+> quotes (ppr mod_name) <> dot) $$
  case cfir of
    NoUnitIdMatching pkg cands ->

      let looks_like_srcpkgid :: SDoc
          looks_like_srcpkgid =
     -- Unsafely coerce a unit id (i.e. an installed package component
     -- identifier) into a PackageId and see if it means anything.
           case cands of
             (pkg:pkgs) ->
              parens (text "This unit ID looks like the source package ID;" $$
                      text "the real unit ID is" <+> quotes (ftext (unitIdFS (unitId pkg))) $$
                     (if null pkgs then empty
                                  else text "and" <+> int (length pkgs) <+> text "other candidate" <> plural pkgs))
             -- Todo: also check if it looks like a package name!
             [] -> empty

      in hsep [ text "no unit id matching" <+> quotes (ppr pkg)
              , text "was found"] $$ looks_like_srcpkgid
    MissingPackageFiles pkg files ->
      text "There are files missing in the " <> quotes (ppr pkg) <+>
      text "package," $$
      text "try running 'ghc-pkg check'." $$
      mayShowLocations verbose files
    MissingPackageWayFiles build pkg files ->
      text "Perhaps you haven't installed the " <> text build <+>
      text "libraries for package " <> quotes (ppr pkg) <> char '?' $$
      mayShowLocations verbose files
    ModuleSuggestion ms fps ->

      let pp_suggestions :: [ModuleSuggestion] -> SDoc
          pp_suggestions sugs
            | null sugs = empty
            | otherwise = hang (text "Perhaps you meant")
                             2 (vcat (map pp_sugg sugs))

          -- NB: Prefer the *original* location, and then reexports, and then
          -- package flags when making suggestions.  ToDo: if the original package
          -- also has a reexport, prefer that one
          pp_sugg (SuggestVisible m mod o) = ppr m <+> provenance o
            where provenance ModHidden = empty
                  provenance (ModUnusable _) = empty
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
                    | otherwise = empty
          pp_sugg (SuggestHidden m mod o) = ppr m <+> provenance o
            where provenance ModHidden =  empty
                  provenance (ModUnusable _) = empty
                  provenance (ModOrigin{ fromOrigUnit = e,
                                         fromHiddenReexport = rhs })
                    | Just False <- e
                       = parens (text "needs flag -package-id"
                          <+> ppr (moduleUnit mod))
                    | (pkg:_) <- rhs
                       = parens (text "needs flag -package-id"
                          <+> ppr (mkUnit pkg))
                    | otherwise = empty

        in pp_suggestions ms $$ mayShowLocations verbose fps
    NotAModule -> text "It is not a module in the current program, or in any known package."
    CouldntFindInFiles fps -> vcat (map text fps)
    MultiplePackages mods
      | Just pkgs <- unambiguousPackages
      -> sep [text "it was found in multiple packages:",
            hsep (map ppr pkgs)]
      | otherwise
      -> vcat (map pprMod mods)
      where
        unambiguousPackages = foldl' unambiguousPackage (Just []) mods
        unambiguousPackage (Just xs) (m, ModOrigin (Just _) _ _ _)
            = Just (moduleUnit m : xs)
        unambiguousPackage _ _ = Nothing
    GenericMissing using_cabal pkg_hiddens mod_hiddens unusables files ->
      vcat (map (pkg_hidden using_cabal) pkg_hiddens) $$
      vcat (map mod_hidden mod_hiddens) $$
      vcat (map unusable unusables) $$
      mayShowLocations verbose files
  where
    verbose = ifaceShowTriedFiles opts

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
    pkg_hidden :: BuildingCabalPackage -> (Unit, Maybe UnitInfo) -> SDoc
    pkg_hidden using_cabal (uid, uif) =
        text "It is a member of the hidden package"
        <+> quotes (ppr uid)
        --FIXME: we don't really want to show the unit id here we should
        -- show the source package id or installed package id if it's ambiguous
        <> dot $$ pkg_hidden_hint using_cabal uif

    pkg_hidden_hint using_cabal (Just pkg)
     | using_cabal == YesBuildingCabalPackage
        = text "Perhaps you need to add" <+>
              quotes (ppr (unitPackageName pkg)) <+>
              text "to the build-depends in your .cabal file."
    -- MP: This is ghci specific, remove
     | otherwise
         = text "You can run" <+>
           quotes (text ":set -package " <> ppr (unitPackageName pkg)) <+>
           text "to expose it." $$
           text "(Note: this unloads all the modules in the current scope.)"
    pkg_hidden_hint _ Nothing = empty

    mod_hidden pkg =
        text "it is a hidden module in the package" <+> quotes (ppr pkg)

    unusable (pkg, reason)
      = text "It is a member of the package"
      <+> quotes (ppr pkg)
      $$ pprReason (text "which is") reason

mayShowLocations :: Bool -> [FilePath] -> SDoc
mayShowLocations verbose files
    | null files = empty
    | not verbose =
          text "Use -v (or `:set -v` in ghci) " <>
              text "to see a list of the files searched for."
    | otherwise =
          hang (text "Locations searched:") 2 $ vcat (map text files)

interfaceErrorDiagnostic :: IfaceMessageOpts -> IfaceMessage -> SDoc
interfaceErrorDiagnostic opts = \ case
  Can'tFindNameInInterface name relevant_tyThings ->
    missingDeclInInterface name relevant_tyThings
  Can'tFindInterface err looking_for ->
    case looking_for of
      LookingForName {} ->
        missingInterfaceErrorDiagnostic opts err
      LookingForModule {} ->
        missingInterfaceErrorDiagnostic opts err
      LookingForHiBoot mod ->
        hang (text "Could not find hi-boot interface for" <+> quotes (ppr mod) <> colon)
          2 (missingInterfaceErrorDiagnostic opts err)
      LookingForSig sig ->
        hang (text "Could not find interface file for signature" <+> quotes (ppr sig) <> colon)
          2 (missingInterfaceErrorDiagnostic opts err)

readInterfaceErrorDiagnostic :: ReadInterfaceError -> SDoc
readInterfaceErrorDiagnostic = \ case
  ExceptionOccurred fp ex ->
    hang (text "Exception when reading interface file " <+> text fp)
      2 (text (showException ex))
  HiModuleNameMismatchWarn _ m1 m2 ->
    hiModuleNameMismatchWarn m1 m2

missingInterfaceErrorDiagnostic :: IfaceMessageOpts -> MissingInterfaceError -> SDoc
missingInterfaceErrorDiagnostic opts reason =
  case reason of
    BadSourceImport m -> badSourceImport m
    HomeModError im ml -> homeModError im ml
    DynamicHashMismatchError m ml -> dynamicHashMismatchError m ml
    CantFindErr us module_or_interface cfi -> pprWithUnitState us $ cantFindError opts module_or_interface cfi
    BadIfaceFile rie -> readInterfaceErrorDiagnostic rie
    FailedToLoadDynamicInterface wanted_mod err ->
      hang (text "Failed to load dynamic interface file for" <+> ppr wanted_mod <> colon)
        2 (readInterfaceErrorDiagnostic err)

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
    -- so reset the NamePprCtx setting.
    hsep [ text "Something is amiss; requested module "
         , ppr requested_mod
         , text "differs from name found in the interface file"
         , ppr read_mod
         , parens (text "if these names look the same, try again with -dppr-debug")
         ]

dynamicHashMismatchError :: Module -> ModLocation -> SDoc
dynamicHashMismatchError wanted_mod loc  =
  vcat [ text "Dynamic hash doesn't match for" <+> quotes (ppr wanted_mod)
       , text "Normal interface file from"  <+> text (ml_hi_file loc)
       , text "Dynamic interface file from" <+> text (ml_dyn_hi_file loc)
       , text "You probably need to recompile" <+> quotes (ppr wanted_mod) ]

homeModError :: InstalledModule -> ModLocation -> SDoc
-- See Note [Home module load error]
homeModError mod location
  = text "attempting to use module " <> quotes (ppr mod)
    <> (case ml_hs_file location of
           Just file -> space <> parens (text file)
           Nothing   -> empty)
    <+> text "which is not loaded"


missingDeclInInterface :: Name -> [TyThing] -> SDoc
missingDeclInInterface name things =
  whenPprDebug (found_things $$ empty) $$
  hang (text "Can't find interface-file declaration for" <+>
         pprNameSpace (nameNameSpace name) <+> ppr name)
    2 (vcat [text "Probable cause: bug in .hi-boot file, or inconsistent .hi file",
             text "Use -ddump-if-trace to get an idea of which file caused the error"])
  where
    found_things =
      hang (text "Found the following declarations in" <+> ppr (nameModule name) <> colon)
           2 (vcat (map ppr things))

badSourceImport :: Module -> SDoc
badSourceImport mod
  = hang (text "You cannot {-# SOURCE #-} import a module from another package")
       2 (text "but" <+> quotes (ppr mod) <+> text "is from package"
          <+> quotes (ppr (moduleUnit mod)))
