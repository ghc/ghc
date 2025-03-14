{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Extracting imported and top-level names in scope
-}

{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Rename.Names (
        rnImports, getLocalNonValBinders, newRecordFieldLabel,
        importsFromIface,
        ImportUserSpec(..),
        extendGlobalRdrEnvRn,
        gresFromAvails,
        calculateAvails,
        reportUnusedNames,
        checkConName,
        mkChildEnv,
        findChildren,
        findImportUsage,
        getMinimalImports,
        printMinimalImports,
        renamePkgQual, renameRawPkgQual,
        classifyGREs,
        ImportDeclUsage
    ) where

import GHC.Prelude hiding ( head, init, last, tail )

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr

import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils ( warnUnusedTopBinds )
import GHC.Rename.Unbound
import qualified GHC.Rename.Unbound as Unbound

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.LclEnv
import GHC.Tc.Zonk.TcType ( tcInitTidyEnv )

import GHC.Hs
import GHC.Iface.Load   ( loadSrcInterface )
import GHC.Iface.Syntax ( IfaceDefault, fromIfaceWarnings )
import GHC.Builtin.Names
import GHC.Parser.PostProcess ( setRdrNameSpace )
import GHC.Core.Type
import GHC.Core.PatSyn
import GHC.Core.TyCon ( TyCon, tyConName )
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic

import GHC.Types.Fixity.Env
import GHC.Types.SafeHaskell
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Avail
import GHC.Types.FieldLabel
import GHC.Types.Hint
import GHC.Types.SourceFile
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Basic  ( TopLevelFlag(..) )
import GHC.Types.SourceText
import GHC.Types.Id
import GHC.Types.PkgQual
import GHC.Types.GREInfo (ConInfo(..), ConFieldInfo (..), ConLikeInfo (ConIsData))

import GHC.Unit
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Imported
import GHC.Unit.Module.Deps
import GHC.Unit.Env

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.FastString.Env
import GHC.Data.Maybe
import GHC.Data.List.SetOps ( removeDups )

import Control.Monad
import Data.Foldable    ( for_ )
import Data.IntMap      ( IntMap )
import qualified Data.IntMap as IntMap
import Data.Map         ( Map )
import qualified Data.Map as Map
import Data.Ord         ( comparing )
import Data.List        ( partition, find, sortBy )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Function    ( on )
import qualified Data.Set as S
import System.FilePath  ((</>))
import System.IO

{-
************************************************************************
*                                                                      *
\subsection{rnImports}
*                                                                      *
************************************************************************

Note [Tracking Trust Transitively]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we import a package as well as checking that the direct imports are safe
according to the rules outlined in the Note [Safe Haskell Trust Check] in GHC.Driver.Main
we must also check that these rules hold transitively for all dependent modules
and packages. Doing this without caching any trust information would be very
slow as we would need to touch all packages and interface files a module depends
on. To avoid this we make use of the property that if a modules Safe Haskell
mode changes, this triggers a recompilation from that module in the dependency
graph. So we can just worry mostly about direct imports.

There is one trust property that can change for a package though without
recompilation being triggered: package trust. So we must check that all
packages a module transitively depends on to be trusted are still trusted when
we are compiling this module (as due to recompilation avoidance some modules
below may not be considered trusted any more without recompilation being
triggered).

We handle this by augmenting the existing transitive list of packages a module M
depends on with a bool for each package that says if it must be trusted when the
module M is being checked for trust. This list of trust required packages for a
single import is gathered in the rnImportDecl function and stored in an
ImportAvails data structure. The union of these trust required packages for all
imports is done by the rnImports function using the combine function which calls
the plusImportAvails function that is a union operation for the ImportAvails
type. This gives us in an ImportAvails structure all packages required to be
trusted for the module we are currently compiling. Checking that these packages
are still trusted (and that direct imports are trusted) is done in
GHC.Driver.Main.checkSafeImports.

See the note below, [Trust Own Package] for a corner case in this method and
how its handled.


Note [Trust Own Package]
~~~~~~~~~~~~~~~~~~~~~~~~
There is a corner case of package trust checking that the usual transitive check
doesn't cover. (For how the usual check operates see the Note [Tracking Trust
Transitively] below). The case is when you import a -XSafe module M and M
imports a -XTrustworthy module N. If N resides in a different package than M,
then the usual check works as M will record a package dependency on N's package
and mark it as required to be trusted. If N resides in the same package as M
though, then importing M should require its own package be trusted due to N
(since M is -XSafe so doesn't create this requirement by itself). The usual
check fails as a module doesn't record a package dependency of its own package.
So instead we now have a bool field in a modules interface file that simply
states if the module requires its own package to be trusted. This field avoids
us having to load all interface files that the module depends on to see if one
is trustworthy.


Note [Trust Transitive Property]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
So there is an interesting design question in regards to transitive trust
checking. Say I have a module B compiled with -XSafe. B is dependent on a bunch
of modules and packages, some packages it requires to be trusted as its using
-XTrustworthy modules from them. Now if I have a module A that doesn't use safe
haskell at all and simply imports B, should A inherit all the trust
requirements from B? Should A now also require that a package p is trusted since
B required it?

We currently say no but saying yes also makes sense. The difference is, if a
module M that doesn't use Safe Haskell imports a module N that does, should all
the trusted package requirements be dropped since M didn't declare that it cares
about Safe Haskell (so -XSafe is more strongly associated with the module doing
the importing) or should it be done still since the author of the module N that
uses Safe Haskell said they cared (so -XSafe is more strongly associated with
the module that was compiled that used it).

Going with yes is a simpler semantics we think and harder for the user to stuff
up but it does mean that Safe Haskell will affect users who don't care about
Safe Haskell as they might grab a package from Cabal which uses safe haskell (say
network) and that packages imports -XTrustworthy modules from another package
(say bytestring), so requires that package is trusted. The user may now get
compilation errors in code that doesn't do anything with Safe Haskell simply
because they are using the network package. They will have to call 'ghc-pkg
trust network' to get everything working. Due to this invasive nature of going
with yes we have gone with no for now.
-}

-- | Process Import Decls.  See 'rnImportDecl' for a description of what
-- the return types represent.
-- Note: Do the non SOURCE ones first, so that we get a helpful warning
-- for SOURCE ones that are unnecessary
rnImports :: [(LImportDecl GhcPs, SDoc)]
          -> RnM ([LImportDecl GhcRn], [ImportUserSpec], GlobalRdrEnv, ImportAvails, [(Module, IfaceDefault)])
rnImports imports = do
    tcg_env <- getGblEnv
    -- NB: want an identity module here, because it's OK for a signature
    -- module to import from its implementor
    let this_mod = tcg_mod tcg_env
    let (source, ordinary) = partition (is_source_import . fst) imports
        is_source_import (d::LImportDecl GhcPs) = ideclSource (unLoc d) == IsBoot
    stuff1 <- mapAndReportM (rnImportDecl this_mod) ordinary
    stuff2 <- mapAndReportM (rnImportDecl this_mod) source
    -- Safe Haskell: See Note [Tracking Trust Transitively]
    let (decls, imp_user_spec, rdr_env, imp_avails, defaults) = combine (stuff1 ++ stuff2)
    -- Update imp_boot_mods if imp_direct_mods mentions any of them
    let merged_import_avail = clobberSourceImports imp_avails
    return (decls, imp_user_spec, rdr_env, merged_import_avail, defaults)

  where
    clobberSourceImports imp_avails =
      imp_avails { imp_boot_mods = imp_boot_mods' }
      where
        imp_boot_mods' = mergeInstalledModuleEnv combJ id (const emptyInstalledModuleEnv)
                            (imp_boot_mods imp_avails)
                            (imp_direct_dep_mods imp_avails)

        combJ (GWIB _ IsBoot) x = Just x
        combJ r _               = Just r
    -- See Note [Combining ImportAvails]
    combine :: [(LImportDecl GhcRn,  ImportUserSpec, GlobalRdrEnv, ImportAvails, [(Module, IfaceDefault)])]
            -> ([LImportDecl GhcRn], [ImportUserSpec], GlobalRdrEnv, ImportAvails, [(Module, IfaceDefault)])
    combine ss =
      let (decls, imp_user_spec, rdr_env, imp_avails, defaults, finsts) = foldr
            plus
            ([], [], emptyGlobalRdrEnv, emptyImportAvails, [], emptyModuleSet)
            ss
      in (decls, imp_user_spec, rdr_env, imp_avails { imp_finsts = moduleSetElts finsts },
            defaults)

    plus (decl,  us, gbl_env1, imp_avails1, defaults1)
         (decls, uss, gbl_env2, imp_avails2, defaults2, finsts_set)
      = ( decl:decls,
          us:uss,
          gbl_env1 `plusGlobalRdrEnv` gbl_env2,
          imp_avails1' `plusImportAvails` imp_avails2,
          defaults1 ++ defaults2,
          extendModuleSetList finsts_set new_finsts )
      where
      imp_avails1' = imp_avails1 { imp_finsts = [] }
      new_finsts = imp_finsts imp_avails1

{-
Note [Combining ImportAvails]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
imp_finsts in ImportAvails is a list of family instance modules
transitively depended on by an import. imp_finsts for a currently
compiled module is a union of all the imp_finsts of imports.
Computing the union of two lists of size N is O(N^2) and if we
do it to M imports we end up with O(M*N^2). That can get very
expensive for bigger module hierarchies.

Union can be optimized to O(N log N) if we use a Set.
imp_finsts is converted back and forth between dep_finsts, so
changing a type of imp_finsts means either paying for the conversions
or changing the type of dep_finsts as well.

I've measured that the conversions would cost 20% of allocations on my
test case, so that can be ruled out.

Changing the type of dep_finsts forces checkFamInsts to
get the module lists in non-deterministic order. If we wanted to restore
the deterministic order, we'd have to sort there, which is an additional
cost. As far as I can tell, using a non-deterministic order is fine there,
but that's a brittle nonlocal property which I'd like to avoid.

Additionally, dep_finsts is read from an interface file, so its "natural"
type is a list. Which makes it a natural type for imp_finsts.

Since rnImports.combine is really the only place that would benefit from
it being a Set, it makes sense to optimize the hot loop in rnImports.combine
without changing the representation.

So here's what we do: instead of naively merging ImportAvails with
plusImportAvails in a loop, we make plusImportAvails merge empty imp_finsts
and compute the union on the side using Sets. When we're done, we can
convert it back to a list. One nice side effect of this approach is that
if there's a lot of overlap in the imp_finsts of imports, the
Set doesn't really need to grow and we don't need to allocate.

Running generateModules from #14693 with DEPTH=16, WIDTH=30 finishes in
23s before, and 11s after.
-}

-- | Given a located import declaration @decl@ from @this_mod@,
-- calculate the following pieces of information:
--
--  1. An updated 'LImportDecl', where all unresolved 'RdrName' in
--     the entity lists have been resolved into 'Name's,
--
--  2. A 'GlobalRdrEnv' representing the new identifiers that were
--     brought into scope (taking into account module qualification
--     and hiding),
--
--  3. 'ImportAvails' summarizing the identifiers that were imported
--     by this declaration, and
--
--  4. A boolean 'AnyHpcUsage' which is true if the imported module
--     used HPC.
rnImportDecl :: Module -> (LImportDecl GhcPs, SDoc)
             -> RnM (LImportDecl GhcRn, ImportUserSpec , GlobalRdrEnv, ImportAvails, [(Module, IfaceDefault)])
rnImportDecl this_mod
             (L loc decl@(ImportDecl { ideclName = loc_imp_mod_name
                                     , ideclPkgQual = raw_pkg_qual
                                     , ideclSource = want_boot, ideclSafe = mod_safe
                                     , ideclQualified = qual_style
                                     , ideclExt = XImportDeclPass { ideclImplicit = implicit }
                                     , ideclAs = as_mod, ideclImportList = imp_details }), import_reason)
  = setSrcSpanA loc $ do

    case raw_pkg_qual of
      NoRawPkgQual -> pure ()
      RawPkgQual _ -> do
        pkg_imports <- xoptM LangExt.PackageImports
        when (not pkg_imports) $ addErr TcRnPackageImportsDisabled

    let qual_only = isImportDeclQualified qual_style

    -- If there's an error in loadInterface, (e.g. interface
    -- file not found) we get lots of spurious errors from 'filterImports'
    let imp_mod_name = unLoc loc_imp_mod_name
        doc = ppr imp_mod_name <+> import_reason

    hsc_env <- getTopEnv
    unit_env <- hsc_unit_env <$> getTopEnv
    let pkg_qual = renameRawPkgQual unit_env imp_mod_name raw_pkg_qual

    -- Check for self-import, which confuses the typechecker (#9032)
    -- ghc --make rejects self-import cycles already, but batch-mode may not
    -- at least not until GHC.IfaceToCore.tcHiBootIface, which is too late to avoid
    -- typechecker crashes.  (Indirect self imports are not caught until
    -- GHC.IfaceToCore, see #10337 tracking how to make this error better.)
    --
    -- Originally, we also allowed 'import {-# SOURCE #-} M', but this
    -- caused bug #10182: in one-shot mode, we should never load an hs-boot
    -- file for the module we are compiling into the EPS.  In principle,
    -- it should be possible to support this mode of use, but we would have to
    -- extend Provenance to support a local definition in a qualified location.
    -- For now, we don't support it, but see #10336
    when (imp_mod_name == moduleName this_mod &&
          (case pkg_qual of -- If we have import "<pkg>" M, then we should
                            -- check that "<pkg>" is "this" (which is magic)
                            -- or the name of this_mod's package.  Yurgh!
                            -- c.f. GHC.findModule, and #9997
             NoPkgQual         -> True
             ThisPkg uid       -> uid == homeUnitId_ (hsc_dflags hsc_env)
             OtherPkg _        -> False))
         (addErr (TcRnSelfImport imp_mod_name))

    -- Check for a missing import list (Opt_WarnMissingImportList also
    -- checks for T(..) items but that is done in checkDodgyImport below)
    case imp_details of
        Just (Exactly, _) -> return () -- Explicit import list
        _  | implicit   -> return () -- Do not bleat for implicit imports
           | qual_only  -> return ()
           | otherwise  -> addDiagnostic (TcRnNoExplicitImportList imp_mod_name)


    iface <- loadSrcInterface doc imp_mod_name want_boot pkg_qual

    -- Compiler sanity check: if the import didn't say
    -- {-# SOURCE #-} we should not get a hi-boot file
    warnPprTrace ((want_boot == NotBoot) && (mi_boot iface == IsBoot)) "rnImportDecl" (ppr imp_mod_name) $ do

    -- Issue a user warning for a redundant {- SOURCE -} import
    -- NB that we arrange to read all the ordinary imports before
    -- any of the {- SOURCE -} imports.
    --
    -- in --make and GHCi, the compilation manager checks for this,
    -- and indeed we shouldn't do it here because the existence of
    -- the non-boot module depends on the compilation order, which
    -- is not deterministic.  The hs-boot test can show this up.
    dflags <- getDynFlags
    warnIf ((want_boot == IsBoot) && (mi_boot iface == NotBoot) && isOneShot (ghcMode dflags))
           (TcRnRedundantSourceImport imp_mod_name)
    when (mod_safe && not (safeImportsOn dflags)) $
        addErr (TcRnSafeImportsDisabled imp_mod_name)

    let imp_mod = mi_module iface
        qual_mod_name = fmap unLoc as_mod `orElse` imp_mod_name
        imp_spec  = ImpDeclSpec { is_mod = imp_mod, is_qual = qual_only,
                                  is_dloc = locA loc, is_as = qual_mod_name,
                                  is_pkg_qual = pkg_qual, is_isboot = want_boot }

    -- filter the imports according to the import declaration
    (new_imp_details, imp_user_list, gbl_env) <- filterImports hsc_env iface imp_spec imp_details

    -- for certain error messages, we’d like to know what could be imported
    -- here, if everything were imported
    potential_gres <- (\(_,_,x) -> x) <$> filterImports hsc_env iface imp_spec Nothing

    let is_hiding | Just (EverythingBut,_) <- imp_details = True
                  | otherwise                             = False

        -- should the import be safe?
        mod_safe' = mod_safe
                    || (not implicit && safeDirectImpsReq dflags)
                    || (implicit && safeImplicitImpsReq dflags)

    hsc_env <- getTopEnv
    let home_unit = hsc_home_unit hsc_env
        other_home_units = hsc_all_home_unit_ids hsc_env
        imv = ImportedModsVal
            { imv_name        = is_as imp_spec
            , imv_span        = locA loc
            , imv_is_safe     = mod_safe'
            , imv_is_hiding   = is_hiding
            , imv_all_exports = potential_gres
            , imv_qualified   = qual_only
            }
        imports = calculateAvails home_unit other_home_units iface mod_safe' want_boot (ImportedByUser imv)

    -- Complain if we import a deprecated module
    case fromIfaceWarnings (mi_warns iface) of
       WarnAll txt -> addDiagnostic (TcRnDeprecatedModule imp_mod_name txt)
       _           -> return ()

    let new_imp_decl = ImportDecl
          { ideclExt       = ideclExt decl
          , ideclName      = ideclName decl
          , ideclPkgQual   = pkg_qual
          , ideclSource    = ideclSource decl
          , ideclSafe      = mod_safe'
          , ideclQualified = ideclQualified decl
          , ideclAs        = ideclAs decl
          , ideclImportList = new_imp_details
          }

    return (L loc new_imp_decl, ImpUserSpec imp_spec imp_user_list, gbl_env,
            imports, (,) (mi_module iface) <$> mi_defaults iface)


-- | Rename raw package imports
renameRawPkgQual :: UnitEnv -> ModuleName -> RawPkgQual -> PkgQual
renameRawPkgQual unit_env mn = \case
  NoRawPkgQual -> NoPkgQual
  RawPkgQual p -> renamePkgQual unit_env mn (Just (sl_fs p))

-- | Rename raw package imports
renamePkgQual :: UnitEnv -> ModuleName -> Maybe FastString -> PkgQual
renamePkgQual unit_env mn mb_pkg = case mb_pkg of
  Nothing -> NoPkgQual
  Just pkg_fs
    | Just uid <- homeUnitId <$> ue_homeUnit unit_env
    , pkg_fs == fsLit "this"
    -> ThisPkg uid

    | Just (uid, _) <- find (fromMaybe False . fmap (== pkg_fs) . snd) home_names
    -> ThisPkg uid

    | Just uid <- resolvePackageImport unit_state mn (PackageName pkg_fs)
    -> OtherPkg uid

    | otherwise
    -> OtherPkg (UnitId pkg_fs)
       -- not really correct as pkg_fs is unlikely to be a valid unit-id but
       -- we will report the failure later...
  where
    home_names  = map (\uid -> (uid, mkFastString <$> thisPackageName (homeUnitEnv_dflags (ue_findHomeUnitEnv uid unit_env)))) hpt_deps

    unit_state = ue_homeUnitState unit_env

    hpt_deps :: [UnitId]
    hpt_deps  = homeUnitDepends unit_state


-- | Calculate the 'ImportAvails' induced by an import of a particular
-- interface, but without 'imp_mods'.
calculateAvails :: HomeUnit
                -> S.Set UnitId
                -> ModIface
                -> IsSafeImport
                -> IsBootInterface
                -> ImportedBy
                -> ImportAvails
calculateAvails home_unit other_home_units iface mod_safe' want_boot imported_by =
  let imp_mod    = mi_module iface
      imp_sem_mod= mi_semantic_module iface
      orph_iface = mi_orphan (mi_final_exts iface)
      has_finsts = mi_finsts (mi_final_exts iface)
      deps       = mi_deps iface
      trust      = getSafeMode $ mi_trust iface
      trust_pkg  = mi_trust_pkg iface
      is_sig     = mi_hsc_src iface == HsigFile

      -- If the module exports anything defined in this module, just
      -- ignore it.  Reason: otherwise it looks as if there are two
      -- local definition sites for the thing, and an error gets
      -- reported.  Easiest thing is just to filter them out up
      -- front. This situation only arises if a module imports
      -- itself, or another module that imported it.  (Necessarily,
      -- this involves a loop.)
      --
      -- We do this *after* filterImports, so that if you say
      --      module A where
      --         import B( AType )
      --         type AType = ...
      --
      --      module B( AType ) where
      --         import {-# SOURCE #-} A( AType )
      --
      -- then you won't get a 'B does not export AType' message.


      -- Compute new transitive dependencies
      --
      -- 'dep_orphs' and 'dep_finsts' do NOT include the imported module
      -- itself, but we DO need to include this module in 'imp_orphs' and
      -- 'imp_finsts' if it defines an orphan or instance family; thus the
      -- orph_iface/has_iface tests.

      deporphs  = dep_orphs deps
      depfinsts = dep_finsts deps

      orphans | orph_iface = assertPpr (not (imp_sem_mod `elem` deporphs)) (ppr imp_sem_mod <+> ppr deporphs) $
                             imp_sem_mod : deporphs
              | otherwise  = deporphs

      finsts | has_finsts = assertPpr (not (imp_sem_mod `elem` depfinsts)) (ppr imp_sem_mod <+> ppr depfinsts) $
                            imp_sem_mod : depfinsts
             | otherwise  = depfinsts

      -- Trusted packages are a lot like orphans.
      trusted_pkgs | mod_safe' = dep_trusted_pkgs deps
                   | otherwise = S.empty


      pkg = moduleUnit (mi_module iface)
      ipkg = toUnitId pkg

      -- Does this import mean we now require our own pkg
      -- to be trusted? See Note [Trust Own Package]
      ptrust = trust == Sf_Trustworthy || trust_pkg
      pkg_trust_req
        | isHomeUnit home_unit pkg = ptrust
        | otherwise = False

      dependent_pkgs = if toUnitId pkg `S.member` other_home_units
                        then S.empty
                        else S.singleton ipkg

      direct_mods = mkModDeps $ if toUnitId pkg `S.member` other_home_units
                      then S.singleton (moduleUnitId imp_mod, (GWIB (moduleName imp_mod) want_boot))
                      else S.empty

      dep_boot_mods_map = mkModDeps (dep_boot_mods deps)

      boot_mods
        -- If we are looking for a boot module, it must be HPT
        | IsBoot <- want_boot = extendInstalledModuleEnv dep_boot_mods_map (toUnitId <$> imp_mod) (GWIB (moduleName imp_mod) IsBoot)
        -- Now we are importing A properly, so don't go looking for
        -- A.hs-boot
        | isHomeUnit home_unit pkg = dep_boot_mods_map
        -- There's no boot files to find in external imports
        | otherwise = emptyInstalledModuleEnv

      sig_mods =
        if is_sig
          then moduleName imp_mod : dep_sig_mods deps
          else dep_sig_mods deps


  in ImportAvails {
          imp_mods       = Map.singleton (mi_module iface) [imported_by],
          imp_orphs      = orphans,
          imp_finsts     = finsts,
          imp_sig_mods   = sig_mods,
          imp_direct_dep_mods = direct_mods,
          imp_dep_direct_pkgs = dependent_pkgs,
          imp_boot_mods = boot_mods,

          -- Add in the imported modules trusted package
          -- requirements. ONLY do this though if we import the
          -- module as a safe import.
          -- See Note [Tracking Trust Transitively]
          -- and Note [Trust Transitive Property]
          imp_trust_pkgs = trusted_pkgs,
          -- Do we require our own pkg to be trusted?
          -- See Note [Trust Own Package]
          imp_trust_own_pkg = pkg_trust_req
     }


{-
************************************************************************
*                                                                      *
\subsection{importsFromLocalDecls}
*                                                                      *
************************************************************************

From the top-level declarations of this module produce
        * the lexical environment
        * the ImportAvails
created by its bindings.

Note [Top-level Names in Template Haskell decl quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also: Note [Interactively-bound Ids in GHCi] in GHC.Driver.Env
          Note [Looking up Exact RdrNames] in GHC.Rename.Env

Consider a Template Haskell declaration quotation like this:
      module M where
        f x = h [d| f = 3 |]
When renaming the declarations inside [d| ...|], we treat the
top level binders specially in two ways

1.  We give them an Internal Name, not (as usual) an External one.
    This is done by GHC.Rename.Env.newTopSrcBinder.

2.  We make them *shadow* the outer bindings.
    See Note [GlobalRdrEnv shadowing]

3. We find out whether we are inside a [d| ... |] by testing the TH
   stage. This is a slight hack, because the stage field was really
   meant for the type checker, and here we are not interested in the
   fields of Brack, hence the error thunks in thRnBrack.
-}

extendGlobalRdrEnvRn :: [GlobalRdrElt]
                     -> MiniFixityEnv
                     -> RnM (TcGblEnv, TcLclEnv)
-- Updates both the GlobalRdrEnv and the FixityEnv
-- We return a new TcLclEnv only because we might have to
-- delete some bindings from it;
-- see Note [Top-level Names in Template Haskell decl quotes]

extendGlobalRdrEnvRn new_gres new_fixities
  = checkNoErrs $  -- See Note [Fail fast on duplicate definitions]
    do  { (gbl_env, lcl_env) <- getEnvs
        ; stage <- getStage
        ; isGHCi <- getIsGHCi
        ; let rdr_env  = tcg_rdr_env gbl_env
              fix_env  = tcg_fix_env gbl_env
              th_bndrs = getLclEnvThBndrs lcl_env
              th_lvl   = thLevel stage

              -- Delete new_occs from global and local envs
              -- If we are in a TemplateHaskell decl bracket,
              --    we are going to shadow them
              -- See Note [GlobalRdrEnv shadowing]
              inBracket = isBrackStage stage

              lcl_env_TH = modifyLclCtxt (\lcl_env -> lcl_env { tcl_rdr = minusLocalRdrEnv (tcl_rdr lcl_env) new_gres_env }) lcl_env
                           -- See Note [GlobalRdrEnv shadowing]

              lcl_env2 | inBracket = lcl_env_TH
                       | otherwise = lcl_env

              -- Deal with shadowing: see Note [GlobalRdrEnv shadowing]
              want_shadowing = isGHCi || inBracket
              rdr_env1 | want_shadowing = shadowNames False rdr_env new_gres_env
                       | otherwise      = rdr_env

              lcl_env3 = modifyLclCtxt (\lcl_env -> lcl_env { tcl_th_bndrs = extendNameEnvList th_bndrs
                                                       [ ( n, (TopLevel, th_lvl) )
                                                       | n <- new_names ] }) lcl_env2

        ; rdr_env2 <- foldlM add_gre rdr_env1 new_gres

        ; let fix_env' = foldl' extend_fix_env fix_env new_gres
              gbl_env' = gbl_env { tcg_rdr_env = rdr_env2, tcg_fix_env = fix_env' }

        ; traceRn "extendGlobalRdrEnvRn 2" (pprGlobalRdrEnv True rdr_env2)
        ; return (gbl_env', lcl_env3) }
  where
    new_names    = map greName new_gres
    new_gres_env = mkGlobalRdrEnv new_gres

    -- If there is a fixity decl for the gre, add it to the fixity env
    extend_fix_env fix_env gre
      | Just (L _ fi) <- lookupMiniFixityEnv new_fixities name
      = extendNameEnv fix_env name (FixItem occ fi)
      | otherwise
      = fix_env
      where
        name = greName gre
        occ  = greOccName gre

    add_gre :: GlobalRdrEnv -> GlobalRdrElt -> RnM GlobalRdrEnv
    -- Extend the GlobalRdrEnv with a LocalDef GRE
    -- If there is already a LocalDef GRE with the same OccName,
    --    report an error and discard the new GRE
    -- This establishes INVARIANT 1 of GlobalRdrEnvs
    add_gre env gre
      | not (null dups)    -- Same OccName defined twice
      = do { addDupDeclErr (gre :| dups); return env }

      | otherwise
      = return (extendGlobalRdrEnv env gre)
      where
        -- See Note [Reporting duplicate local declarations]
        dups = filter isBadDupGRE
             $ lookupGRE env (LookupOccName (greOccName gre) (RelevantGREsFOS WantBoth))
        isBadDupGRE old_gre = isLocalGRE old_gre && greClashesWith gre old_gre

{- Note [Fail fast on duplicate definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If there are duplicate bindings for the same thing, we want to fail
fast. Having two bindings for the same thing can cause follow-on errors.
Example (test T9975a):
   data Test = Test { x :: Int }
   pattern Test wat = Test { x = wat }
This defines 'Test' twice.  The second defn has no field-names; and then
we get an error from Test { x=wat }, saying "Test has no field 'x'".

Easiest thing is to bale out fast on duplicate definitions, which
we do via `checkNoErrs` on `extendGlobalRdrEnvRn`.

Note [Reporting duplicate local declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a single module may not define the same OccName multiple times. This
is checked in extendGlobalRdrEnvRn: when adding a new locally-defined GRE to the
GlobalRdrEnv we report an error if there are already duplicates in the
environment.  This establishes INVARIANT 1 (see comments on GlobalRdrEnv in
GHC.Types.Name.Reader), which says that for a given OccName, all the
GlobalRdrElts to which it maps must have distinct 'greName's.

For example, the following will be rejected:

  f x = x
  g x = x
  f x = x  -- Duplicate!

Users are allowed to introduce new GREs with the same OccName as an imported GRE,
as disambiguation is possible through the module system, e.g.:

  module M where
    import N (f)
    f x = x
    g x = M.f x + N.f x

If both GREs are local, the general rule is that two GREs clash if they have
the same OccName, i.e. they share a textual name and live in the same namespace.
However, there are additional clashes due to record fields:

  - a new variable clashes with previously defined record fields
    which define field selectors,

  - a new record field shadows:

    - previously defined variables, if it defines a field selector,
    - previously defined record fields, unless it is a duplicate record field.

This logic is implemented in the function 'GHC.Types.Name.Reader.greClashesWith'.

See also Note [Skipping ambiguity errors at use sites of local declarations] in
GHC.Rename.Utils.
-}


{- *********************************************************************
*                                                                      *
    getLocalDeclBindersd@ returns the names for an HsDecl
             It's used for source code.

        *** See Note [The Naming story] in GHC.Hs.Decls ****
*                                                                      *
********************************************************************* -}

getLocalNonValBinders :: MiniFixityEnv -> HsGroup GhcPs
    -> RnM ((TcGblEnv, TcLclEnv), NameSet)
-- Get all the top-level binders bound the group *except*
-- for value bindings, which are treated separately
-- Specifically we return AvailInfo for
--      * type decls (incl constructors and record selectors)
--      * class decls (including class ops)
--      * associated types
--      * foreign imports
--      * value signatures (in hs-boot files only)

getLocalNonValBinders fixity_env
     (HsGroup { hs_valds  = binds,
                hs_tyclds = tycl_decls,
                hs_fords  = foreign_decls })
  = do  { -- Process all type/class decls *except* family instances
        ; let inst_decls = tycl_decls >>= group_instds
        ; dup_fields_ok <- xopt_DuplicateRecordFields <$> getDynFlags
        ; has_sel <- xopt_FieldSelectors <$> getDynFlags
        ; tc_gres
            <- concatMapM
                 (new_tc dup_fields_ok has_sel)
                 (tyClGroupTyClDecls tycl_decls)
        ; traceRn "getLocalNonValBinders 1" (ppr tc_gres)
        ; envs <- extendGlobalRdrEnvRn tc_gres fixity_env
        ; restoreEnvs envs $ do {
            -- Bring these things into scope first
            -- See Note [Looking up family names in family instances]

          -- Process all family instances
          -- to bring new data constructors into scope
        ; nti_gress <- mapM (new_assoc dup_fields_ok has_sel) inst_decls

          -- Finish off with value binders:
          --    foreign decls and pattern synonyms for an ordinary module
          --    type sigs in case of a hs-boot file only
        ; is_boot <- tcIsHsBootOrSig
        ; let val_bndrs
                | is_boot = case binds of
                      ValBinds _ _val_binds val_sigs ->
                          -- In a hs-boot file, the value binders come from the
                          --  *signatures*, and there should be no foreign binders
                          [ L (l2l decl_loc) (unLoc n)
                          | L decl_loc (TypeSig _ ns _) <- val_sigs, n <- ns]
                      _ -> panic "Non-ValBinds in hs-boot group"
                | otherwise = for_hs_bndrs
        ; val_gres <- mapM new_simple val_bndrs

        ; let avails    = concat nti_gress ++ val_gres
              new_bndrs = gresToNameSet avails `unionNameSet`
                          gresToNameSet tc_gres
        ; traceRn "getLocalNonValBinders 2" (ppr avails)
        ; envs <- extendGlobalRdrEnvRn avails fixity_env
        ; return (envs, new_bndrs) } }
  where
    for_hs_bndrs :: [LocatedN RdrName]
    for_hs_bndrs = hsForeignDeclsBinders foreign_decls

      -- the SrcSpan attached to the input should be the span of the
      -- declaration, not just the name
    new_simple :: LocatedN RdrName -> RnM GlobalRdrElt
    new_simple rdr_name = do { nm <- newTopSrcBinder rdr_name
                             ; return (mkLocalVanillaGRE NoParent nm) }

    new_tc :: DuplicateRecordFields -> FieldSelectors -> LTyClDecl GhcPs
           -> RnM [GlobalRdrElt]
    new_tc dup_fields_ok has_sel tc_decl -- NOT for type/data instances
        = do { let TyDeclBinders (main_bndr, tc_flav) at_bndrs sig_bndrs
                     (LConsWithFields cons_with_flds flds) = hsLTyClDeclBinders tc_decl
             ; tycon_name          <- newTopSrcBinder $ la2la main_bndr
             ; at_names            <- mapM (newTopSrcBinder . la2la . fst) at_bndrs
             ; sig_names           <- mapM (newTopSrcBinder . la2la) sig_bndrs
             ; con_names_with_flds <- mapM (\(con,flds) -> (,flds) <$> newTopSrcBinder (la2la con)) cons_with_flds
             ; flds' <- mapM (newRecordFieldLabel dup_fields_ok has_sel $ map fst con_names_with_flds) flds
             ; mapM_ (add_dup_fld_errs flds') con_names_with_flds
             ; let tc_gre = mkLocalTyConGRE (fmap (const tycon_name) tc_flav) tycon_name
                   fld_env = mk_fld_env con_names_with_flds flds'
                   at_gres = zipWith (\ (_, at_flav) at_nm -> mkLocalTyConGRE (fmap (const tycon_name) at_flav) at_nm)
                               at_bndrs at_names
                   sig_gres = map (mkLocalVanillaGRE (ParentIs tycon_name)) sig_names
                   con_gres = map (mkLocalConLikeGRE (ParentIs tycon_name)) fld_env
                   fld_gres = mkLocalFieldGREs (ParentIs tycon_name) fld_env
                   sub_gres = at_gres ++ sig_gres ++ con_gres ++ fld_gres
             ; traceRn "getLocalNonValBinders new_tc" $
                 vcat [ text "tycon:" <+> ppr tycon_name
                      , text "tc_gre:" <+> ppr tc_gre
                      , text "sub_gres:" <+> ppr sub_gres ]
             ; return $ tc_gre : sub_gres }

    -- Calculate the record field information, which feeds into the GlobalRdrElts
    -- for DataCons and their fields. It's convenient to do this here where
    -- we are working with a single datatype definition.
    --
    -- The information we needed was all set up for us:
    -- see Note [Collecting record fields in data declarations] in GHC.Hs.Utils.
    mk_fld_env :: [(Name, Maybe [Located Int])] -> IntMap FieldLabel
               -> [(ConLikeName, ConInfo)]
    mk_fld_env names flds =
      [ (DataConName con, ConInfo (ConIsData (map fst names)) fld_info)
      | (con, mb_fl_indxs) <- names
      , let fld_info = case fmap (map ((flds IntMap.!) . unLoc)) mb_fl_indxs of
              Nothing         -> ConHasPositionalArgs
              Just []         -> ConIsNullary
              Just (fld:flds) -> ConHasRecordFields $ fld NE.:| flds ]

    new_assoc :: DuplicateRecordFields -> FieldSelectors -> LInstDecl GhcPs
              -> RnM [GlobalRdrElt]
    new_assoc _ _ (L _ (TyFamInstD {})) = return []
      -- type instances don't bind new names

    new_assoc dup_fields_ok has_sel (L _ (DataFamInstD _ d))
      = new_di dup_fields_ok has_sel Nothing d
    new_assoc dup_fields_ok has_sel
      (L _ (ClsInstD _ (ClsInstDecl { cid_poly_ty = inst_ty
                                    , cid_datafam_insts = adts })))
      = do -- First, attempt to grab the name of the class from the instance.
           -- This step could fail if the instance is not headed by a class,
           -- such as in the following examples:
           --
           -- (1) The class is headed by a bang pattern, such as in
           --     `instance !Show Int` (#3811c)
           -- (2) The class is headed by a type variable, such as in
           --     `instance c` (#16385)
           --
           -- If looking up the class name fails, then mb_cls_gre will
           -- be Nothing.
           mb_cls_gre <- runMaybeT $ do
             -- See (1) above
             L loc cls_rdr <- MaybeT $ pure $ getLHsInstDeclClass_maybe inst_ty
             -- See (2) above
             MaybeT $ setSrcSpan (locA loc) $ lookupGlobalOccRn_maybe SameNameSpace cls_rdr
           -- Assuming the previous step succeeded, process any associated data
           -- family instances. If the previous step failed, bail out.
           case mb_cls_gre of
             Nothing
               -> pure []
             Just cls_gre
               -> let cls_nm = greName cls_gre
                  in concatMapM (new_di dup_fields_ok has_sel (Just cls_nm) . unLoc) adts

    new_di :: DuplicateRecordFields -> FieldSelectors
           -> Maybe Name -- class name
           -> DataFamInstDecl GhcPs
           -> RnM [GlobalRdrElt]
    new_di dup_fields_ok has_sel mb_cls dfid@(DataFamInstDecl { dfid_eqn = ti_decl })
        = do { main_name <- unLoc <$> lookupFamInstName mb_cls (feqn_tycon ti_decl)
             ; let LConsWithFields cons_with_flds flds = hsDataFamInstBinders dfid
             ; sub_names <- mapM (\(con,flds) -> (,flds) <$> newTopSrcBinder (la2la con)) cons_with_flds
             ; flds' <- mapM (newRecordFieldLabel dup_fields_ok has_sel $ map fst sub_names) flds
             ; mapM_ (add_dup_fld_errs flds') sub_names
             ; let fld_env  = mk_fld_env sub_names flds'
                   con_gres = map (mkLocalConLikeGRE (ParentIs main_name)) fld_env
                   field_gres = mkLocalFieldGREs (ParentIs main_name) fld_env
               -- NB: the data family name is not bound here,
               -- so we don't return a GlobalRdrElt for it here!
             ; return $ con_gres ++ field_gres }

    -- Add errors if a constructor has a duplicate record field.
    add_dup_fld_errs :: IntMap FieldLabel
                     -> (Name, Maybe [Located Int])
                     -> IOEnv (Env TcGblEnv TcLclEnv) ()
    add_dup_fld_errs all_flds (con, mb_con_flds)
      | Just con_flds <- mb_con_flds
      , let (_, dups) = removeDups (comparing unLoc) con_flds
      = for_ dups $ \ dup_flds ->
          -- Report the error at the location of the second occurrence
          -- of the duplicate field.
          let loc =
                case dup_flds of
                  _ :| ( L loc _ : _) -> loc
                  L loc _ :| _ -> loc
              dup_rdrs = fmap (nameRdrName . flSelector . (all_flds IntMap.!) . unLoc) dup_flds
          in addErrAt loc $ TcRnDuplicateFieldName (RecordFieldDecl con) dup_rdrs
      | otherwise
      = return ()

newRecordFieldLabel :: DuplicateRecordFields -> FieldSelectors -> [Name] -> LFieldOcc GhcPs -> RnM FieldLabel
newRecordFieldLabel _ _ [] _ = error "newRecordFieldLabel: datatype has no constructors!"
newRecordFieldLabel dup_fields_ok has_sel (dc:_) (L loc (FieldOcc _ (L _ fld)))
  = do { selName <- newTopSrcBinder $ L (l2l loc) $ field
       ; return $ FieldLabel { flHasDuplicateRecordFields = dup_fields_ok
                             , flHasFieldSelector = has_sel
                             , flSelector = selName } }
  where
    fld_occ = rdrNameOcc fld
    dc_fs = occNameFS $ nameOccName dc
    field
      -- Use an Exact RdrName as-is, to preserve the bindings
      -- of an already renamer-resolved field and its use
      -- sites. This is needed to correctly support record
      -- selectors in Template Haskell. See Note [Binders in
      -- Template Haskell] in "GHC.ThToHs" and Note [Looking up
      -- Exact RdrNames] in "GHC.Rename.Env".
      | isExact fld
      = assertPpr (fieldOcc_maybe fld_occ == Just dc_fs)
          (vcat [ text "newRecordFieldLabel: incorrect namespace for exact Name" <+> quotes (ppr fld)
                , text "expected namespace:" <+> pprNameSpace (fieldName dc_fs)
                , text "  actual namespace:" <+> pprNameSpace (occNameSpace fld_occ) ])
        fld

      -- Field names produced by the parser are namespaced with VarName.
      -- Here we namespace them according to the first constructor.
      -- See Note [Record field namespacing] in GHC.Types.Name.Occurrence.
      | otherwise
      = mkRdrUnqual $ varToRecFieldOcc dc_fs fld_occ

{-
Note [Looking up family names in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  module M where
    type family T a :: *
    type instance M.T Int = Bool

We might think that we can simply use 'lookupOccRn' when processing the type
instance to look up 'M.T'.  Alas, we can't!  The type family declaration is in
the *same* HsGroup as the type instance declaration.  Hence, as we are
currently collecting the binders declared in that HsGroup, these binders will
not have been added to the global environment yet.

Solution is simple: process the type family declarations first, extend
the environment, and then process the type instances.


************************************************************************
*                                                                      *
\subsection{Filtering imports}
*                                                                      *
************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

Note [Dealing with imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For import M( ies ), we take each AvailInfo from the mi_exports of M, and make

  imp_occ_env :: OccEnv (NameEnv ImpOccItem)

This map contains one entry for each OccName that M exports, mapping each OccName
to the following information:

  1. the GlobalRdrElt corresponding to the OccName,
  2. whether this GlobalRdrElt was the parent in the AvailInfo we found
     the OccName in.
  3. the GlobalRdrElts that were bundled together in the AvailInfo we found
    this OccName in (not including the parent),

We need (2) and (3) during the construction of the OccEnv because of associated
types and bundled pattern synonyms, respectively.
(3) is explained in Note [Importing PatternSynonyms].

To explain (2), consider for example:

  module M where
    class    C a    where { data T a }
    instance C Int  where { data T Int = T1 | T2 }
    instance C Bool where { data T Int = T3 }

Here, M's exports avails are (recalling the AvailTC invariant from GHC.Types.Avail)

  C(C,T), T(T,T1,T2,T3)

Notice that T appears *twice*, once as a child and once as a parent. From
these two exports, respectively, during construction of the imp_occ_env, we begin
by associating the following two elements with the key T:

  T -> ImpOccItem { imp_item = gre1, imp_bundled = [C,T]     , imp_is_parent = False }
  T -> ImpOccItem { imp_item = gre2, imp_bundled = [T1,T2,T3], imp_is_parent = True  }

where `gre1`, `gre2` are two GlobalRdrElts with greName T.
We combine these (in function 'combine' in 'mkImportOccEnv') by discarding the
non-parent item, thusly:

  T -> IE_ITem { imp_item = gre1 `plusGRE` gre2, imp_bundled = [T1,T2,T3], imp_is_parent = True }

Note the `plusGRE`: this ensures we don't drop parent information;
see Note [Preserve parent information when combining import OccEnvs].

So the overall imp_occ_env is:

  C  -> ImpOccItem { imp_item = C,  imp_bundled = [T       ], imp_is_parent = True  }
  T  -> ImpOccItem { imp_item = T , imp_bundled = [T1,T2,T3], imp_is_parent = True  }
  T1 -> ImpOccItem { imp_item = T1, imp_bundled = [T1,T2,T3], imp_is_parent = False }
    -- similarly for T2, T3

Note that the imp_occ_env will have entries for data constructors too,
although we never look up data constructors.

Note [Importing PatternSynonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Dealing with imports], associated types can lead to the
same Name appearing twice, both as a child and once as a parent, when
constructing the imp_occ_env.  The same thing can happen with pattern synonyms
if they are exported bundled with a type.

A simplified example, based on #11959:

  {-# LANGUAGE PatternSynonyms #-}
  module M (T(P), pattern P) where  -- Duplicate export warning, but allowed
    data T = MkT
    pattern P = MkT

Here we have T(P) and P in export_avails, and respectively construct both

  P -> ImpOccItem { imp_item = P, imp_bundled = [P], imp_is_parent = False }
  P -> ImpOccItem { imp_item = P, imp_bundled = [] , imp_is_parent = False }

We combine these by dropping the one with no siblings, leaving us with:

  P -> ImpOccItem { imp_item = P, imp_bundled = [P], imp_is_parent = False }

That is, we simply discard the non-bundled Avail.

Note [Importing DuplicateRecordFields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In filterImports, another complicating factor is DuplicateRecordFields.
Suppose we have:

  {-# LANGUAGE DuplicateRecordFields #-}
  module M (S(foo), T(foo)) where
    data S = MkS { foo :: Int }
    data T = MkT { foo :: Int }

  module N where
    import M (foo)    -- this is allowed (A)
    import M (S(foo)) -- this is allowed (B)

Here M exports 'foo' at two different OccNames, with different namespaces for
the two construtors MkS and MkT. Then, when we look up 'foo' in lookup_names
for case (A), we have a variable foo but must look in all the record field
namespaces to find the two fields (and hence two different Avails).
Whereas in case (B) we reach the lookup_ie case for IEThingWith,
which looks up 'S' and then finds the unique 'foo' amongst its children.

See T16745 for a test of this.

Note [Preserve parent information when combining import OccEnvs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When discarding one ImpOccItem in favour of another, as described in
Note [Dealing with imports], we must make sure to combine the GREs so that
we don't lose information.

Consider for example #24084:

  module M1 where { class C a where { type T a } }
  module M2 ( module M1 ) where { import M1 }
  module M3 where { import M2 ( C, T ); instance C () where T () = () }

When processing the import list of `M3`, we will have two `Avail`s attached
to `T`, namely `C(C, T)` and `T(T)`. We combine them in the `combine` function
of `mkImportOccEnv`; as described in Note [Dealing with imports] we discard
`C(C, T)` in favour of `T(T)`. However, in doing so, we **must not**
discard the information want that `C` is the parent of `T`. Indeed,
losing track of this information can cause errors when importing,
as we could get an error of the form

  ‘T’ is not a (visible) associated type of class ‘C’

This explains why we use `plusGRE` when combining the two ImpOccItems, even
though we are discarding one in favour of the other.
-}

-- | All the 'GlobalRdrElt's associated with an 'AvailInfo'.
gresFromAvail :: HasDebugCallStack
              => HscEnv -> Maybe ImportSpec -> AvailInfo -> [GlobalRdrElt]
gresFromAvail hsc_env prov avail =
  [ mk_gre nm info
  | nm <- availNames avail
  , let info = lookupGREInfo hsc_env nm ]
  where

    mk_gre n info
      = case prov of
            -- Nothing => bound locally
            -- Just is => imported from 'is'
          Nothing -> GRE { gre_name = n, gre_par = mkParent n avail
                         , gre_lcl = True, gre_imp = emptyBag
                         , gre_info = info }
          Just is -> GRE { gre_name = n, gre_par = mkParent n avail
                         , gre_lcl = False, gre_imp = unitBag is
                         , gre_info = info }

-- | All the 'GlobalRdrElt's associated with a collection of 'AvailInfo's.
gresFromAvails :: HscEnv -> Maybe ImportSpec -> [AvailInfo] -> [GlobalRdrElt]
gresFromAvails hsc_env prov = concatMap (gresFromAvail hsc_env prov)

importsFromIface :: HscEnv -> ModIface -> ImpDeclSpec -> Maybe NameSet -> GlobalRdrEnv
importsFromIface hsc_env iface decl_spec hidden = mkGlobalRdrEnv $ case hidden of
    Nothing -> all_gres
    Just hidden_names -> filter (not . (`elemNameSet` hidden_names) . greName) all_gres
  where
    all_gres = gresFromAvails hsc_env (Just imp_spec) (mi_exports iface)
    imp_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }

filterImports
    :: HasDebugCallStack
    => HscEnv
    -> ModIface
    -> ImpDeclSpec
         -- ^ Import spec
    -> Maybe (ImportListInterpretation, LocatedLI [LIE GhcPs])
         -- ^ Whether this is a "hiding" import list
    -> RnM (Maybe (ImportListInterpretation, LocatedLI [LIE GhcRn]), -- Import spec w/ Names
            ImpUserList,                      -- same, but designed for storage in interfaces
            GlobalRdrEnv)                   -- Same again, but in GRE form
filterImports hsc_env iface decl_spec Nothing
  = return (Nothing, ImpUserAll, importsFromIface hsc_env iface decl_spec Nothing)
filterImports hsc_env iface decl_spec (Just (want_hiding, L l import_items))
  = do  -- check for errors, convert RdrNames to Names
        items1 <- mapM lookup_lie import_items

        let items2 :: [(LIE GhcRn, [GlobalRdrElt])]
            items2 = concat items1
                -- NB we may have duplicates, and several items
                --    for the same parent; e.g N(x) and N(y)

            (gres, imp_user_list) = case want_hiding of
              Exactly ->
                let gre_env = mkGlobalRdrEnv $ concatMap (gresFromIE decl_spec) items2
                in (gre_env, ImpUserExplicit (gresToAvailInfo $ globalRdrEnvElts $ gre_env))
              EverythingBut ->
                let hidden_names = mkNameSet $ concatMap (map greName . snd) items2
                in (importsFromIface hsc_env iface decl_spec (Just hidden_names), ImpUserEverythingBut hidden_names)

        return (Just (want_hiding, L l (map fst items2)), imp_user_list, gres)
  where
    import_mod = mi_module iface
    all_avails = mi_exports iface
    imp_occ_env = mkImportOccEnv hsc_env decl_spec all_avails

    -- Look up a parent (type constructor, class or data constructor)
    -- in an import.
    lookup_parent :: IE GhcPs -> RdrName -> IELookupM ImpOccItem
    lookup_parent ie rdr =
      assertPpr (not $ isVarNameSpace ns)
        (vcat [ text "filterImports lookup_parent: unexpected variable"
              , text "rdr:" <+> ppr rdr
              , text "namespace:" <+> pprNameSpace ns ]) $
      do { xs <- lookup_names ie rdr
         ; case xs of
            cax :| [] -> return cax
            _         -> pprPanic "filter_imports lookup_parent ambiguous" $
                           vcat [ text "rdr:" <+> ppr rdr
                                , text "lookups:" <+> ppr (fmap imp_item xs) ] }
              -- Looking up non-variables is always unambiguous,
              -- as there can be at most one corresponding item
              -- in the imp_occ_env.
              -- See item (1) of Note [Exporting duplicate declarations]
              -- in GHC.Tc.Gen.Export.
      where
        occ = rdrNameOcc rdr
        ns  = occNameSpace occ

    -- Look up a RdrName used in an import, returning multiple values if there
    -- are several fields with the same name exposed by the module
    lookup_names :: IE GhcPs -> RdrName -> IELookupM (NonEmpty ImpOccItem)
    lookup_names ie rdr
       | isQual rdr
       = failLookupWith (QualImportError rdr)
       | otherwise
       = case lookups of
           []         -> failLookupWith (BadImport ie IsNotSubordinate)
           item:items -> return $ item :| items
      where
        lookups = concatMap nonDetNameEnvElts
                $ lookupImpOccEnv (RelevantGREsFOS WantNormal) imp_occ_env (rdrNameOcc rdr)

    lookup_lie :: LIE GhcPs -> TcRn [(LIE GhcRn, [GlobalRdrElt])]
    lookup_lie (L loc ieRdr)
        = setSrcSpanA loc $
          do (stuff, warns) <- liftM (fromMaybe ([],[])) $
                               run_lookup (lookup_ie ieRdr)
             mapM_ (addTcRnDiagnostic <=< warning_msg) warns
             return [ (L loc ie, gres) | (ie,gres) <- stuff ]
        where

            -- Warn when importing T(..) and no children are brought in scope
            warning_msg (DodgyImport n) =
              pure (TcRnDodgyImports (DodgyImportsEmptyParent n))
            warning_msg MissingImportList =
              pure (TcRnMissingImportList ieRdr)
            warning_msg (BadImportW ie) = do
              -- 'BadImportW' is only constructed below in 'handle_bad_import', in
              -- the 'EverythingBut' case, so that's what we pass to
              -- 'badImportItemErr'.
              reason <- badImportItemErr iface decl_spec ie IsNotSubordinate all_avails
              pure (TcRnDodgyImports (DodgyImportsHiding reason))
            warning_msg (DeprecatedExport n w) =
              pure $ TcRnPragmaWarning
                         PragmaWarningExport
                           { pwarn_occname = occName n
                           , pwarn_impmod  = moduleName import_mod }
                         w

            run_lookup :: IELookupM a -> TcRn (Maybe a)
            run_lookup m = case m of
              Failed err -> do
                msg <- lookup_err_msg err
                addErr (TcRnImportLookup msg)
                return Nothing
              Succeeded a -> return (Just a)

            lookup_err_msg err = case err of
              BadImport ie sub    -> badImportItemErr iface decl_spec ie sub all_avails
              IllegalImport       -> pure ImportLookupIllegal
              QualImportError rdr -> pure (ImportLookupQualified rdr)

        -- For each import item, we convert its RdrNames to Names,
        -- and at the same time compute all the GlobalRdrElt corresponding
        -- to what is actually imported by this item.
        -- Returns Nothing on error.
        --
        -- Returns a list because, with DuplicateRecordFields, a naked
        -- import/export of a record field can correspond to multiple
        -- different GlobalRdrElts. See Note [Importing DuplicateRecordFields].
    lookup_ie :: IE GhcPs
              -> IELookupM ([(IE GhcRn, [GlobalRdrElt])], [IELookupWarning])
    lookup_ie ie = handle_bad_import $
      case ie of
        IEVar _ (L l n) _ -> do
            -- See Note [Importing DuplicateRecordFields]
            xs <- lookup_names ie (ieWrappedName n)
            let gres = map imp_item $ NE.toList xs
                export_depr_warns
                  | want_hiding == Exactly
                      = mapMaybe mk_depr_export_warning gres
                  | otherwise = []
            return ( [ (IEVar Nothing (L l (replaceWrappedName n name)) noDocstring, [gre])
                     | gre <- gres
                     , let name = greName gre ]
                   , export_depr_warns )

        IEThingAll _ (L l tc) _ -> do
            ImpOccItem { imp_item      = gre
                       , imp_bundled   = bundled_gres
                       , imp_is_parent = is_par
                       }
              <- lookup_parent ie $ ieWrappedName tc
            let name = greName gre
                child_gres = if is_par then bundled_gres else []
                imp_list_warn

                  | null child_gres
                  -- e.g. f(..) or T(..) where T is a type synonym
                  = [DodgyImport gre]

                  -- e.g. import M( T(..) )
                  | not (is_qual decl_spec)
                  = [MissingImportList]

                  | otherwise
                  = []

                renamed_ie = IEThingAll (Nothing, noAnn) (L l (replaceWrappedName tc name)) noDocstring
                export_depr_warn
                  | want_hiding == Exactly
                      = maybeToList $ mk_depr_export_warning gre
                        -- We don't want to warn about the children as they
                        -- are not explicitly mentioned; the warning will
                        -- be emitted later on if they are used
                  | otherwise = []

            return ( [(renamed_ie, gre:child_gres)]
                   , imp_list_warn ++ export_depr_warn)


        IEThingAbs _ (L l tc') _
            | want_hiding == EverythingBut   -- hiding ( C )
                       -- Here the 'C' can be a data constructor
                       --  *or* a type/class, or even both
            -> let tc = ieWrappedName tc'
                   tc_name = lookup_parent ie tc
                   dc_name = lookup_parent ie (setRdrNameSpace tc srcDataName)
               in
               case catIELookupM [ tc_name, dc_name ] of
                 []    -> failLookupWith (BadImport ie IsNotSubordinate)
                 names -> return ( [mkIEThingAbs tc' l (imp_item name) | name <- names], [])
            | otherwise
            -> do ImpOccItem { imp_item = gre } <- lookup_parent ie (ieWrappedName tc')
                  return ( [mkIEThingAbs tc' l gre]
                         , maybeToList $ mk_depr_export_warning gre)

        IEThingWith (deprecation, ann) ltc@(L l rdr_tc) wc rdr_ns _ -> do
           ImpOccItem { imp_item = gre, imp_bundled = subnames }
               <- lookup_parent (IEThingAbs Nothing ltc noDocstring) (ieWrappedName rdr_tc)
           let name = greName gre

           -- Look up the children in the sub-names of the parent
           -- See Note [Importing DuplicateRecordFields]
           case lookupChildren subnames rdr_ns of

             Failed rdrs -> failLookupWith $
                            BadImport (IEThingWith (deprecation, ann) ltc wc rdrs noDocstring) IsSubordinate
                                -- We are trying to import T( a,b,c,d ), and failed
                                -- to find 'b' and 'd'.  So we make up an import item
                                -- to report as failing, namely T( b, d ).
                                -- c.f. #15412

             Succeeded childnames ->
                return ([ (IEThingWith (Nothing, ann) (L l name') wc childnames' noDocstring
                          ,gres)]
                       , export_depr_warns)

              where name' = replaceWrappedName rdr_tc name
                    childnames' = map (to_ie_post_rn . fmap greName) childnames
                    gres = gre : map unLoc childnames
                    export_depr_warns
                      | want_hiding == Exactly = mapMaybe mk_depr_export_warning gres
                      | otherwise              = []

        _other -> failLookupWith IllegalImport
        -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed...
        -- all of those constitute errors.

      where
        mkIEThingAbs tc l gre
          = (IEThingAbs Nothing (L l (replaceWrappedName tc n)) noDocstring, [gre])
          where n = greName gre

        -- N.B. imports never have docstrings
        noDocstring = Nothing

        handle_bad_import m = catchIELookup m $ \err -> case err of
          BadImport ie _
            | want_hiding == EverythingBut
            -> return ([], [BadImportW ie])
          _ -> failLookupWith err

        mk_depr_export_warning gre
          = DeprecatedExport name <$> mi_export_warn_fn (mi_final_exts iface) name
          where
            name = greName gre

type IELookupM = MaybeErr IELookupError

data IELookupWarning
  = BadImportW (IE GhcPs)
  | MissingImportList
  | DodgyImport GlobalRdrElt
  | DeprecatedExport Name (WarningTxt GhcRn)

-- | Is this import/export item a subordinate or not?
data IsSubordinate
  = IsSubordinate | IsNotSubordinate

data IELookupError
  = QualImportError RdrName
  | BadImport (IE GhcPs) IsSubordinate
  | IllegalImport

failLookupWith :: IELookupError -> IELookupM a
failLookupWith err = Failed err

catchIELookup :: IELookupM a -> (IELookupError -> IELookupM a) -> IELookupM a
catchIELookup m h = case m of
  Succeeded r -> return r
  Failed err  -> h err

catIELookupM :: [IELookupM a] -> [a]
catIELookupM ms = [ a | Succeeded a <- ms ]

-- | Information associated to an 'AvailInfo' used in constructing
-- an 'OccEnv' corresponding to imports.
--
-- See Note [Dealing with imports].
data ImpOccItem
  = ImpOccItem
      { imp_item      :: GlobalRdrElt
        -- ^ The import item
      , imp_bundled   :: [GlobalRdrElt]
        -- ^ Items bundled in the Avail this import item came from,
        -- not including the import item itself if it is a parent.
      , imp_is_parent :: Bool
        -- ^ Is the import item a parent? See Note [Dealing with imports].
      }

instance Outputable ImpOccItem where
  ppr (ImpOccItem { imp_item = item, imp_bundled = bundled, imp_is_parent = is_par })
    = braces $ hsep
       [ text "ImpOccItem"
       , if is_par then text "[is_par]" else empty
       , ppr (greName item) <+> ppr (greParent item)
       , braces $ text "bundled:" <+> ppr (map greName bundled) ]

-- | Make an 'OccEnv' of all the imports.
--
-- Complicated by the fact that associated data types and pattern synonyms
-- can appear twice. See Note [Dealing with imports].
mkImportOccEnv :: HscEnv -> ImpDeclSpec -> [IfaceExport] -> OccEnv (NameEnv ImpOccItem)
mkImportOccEnv hsc_env decl_spec all_avails =
  mkOccEnv_C (plusNameEnv_C combine)
    [ (occ, mkNameEnv [(nm, item)])
    | avail <- all_avails
    , let gres = gresFromAvail hsc_env (Just hiding_spec) avail
    , gre <- gres
    , let nm = greName gre
          occ = greOccName gre
          (is_parent, bundled) = case avail of
            AvailTC c _
              | c == nm -- (Recall the AvailTC invariant from GHC.Types.AvailInfo)
              -> ( True, drop 1 gres ) -- "drop 1": don't include the parent itself.
              | otherwise
              -> ( False, gres )
            _ -> ( False, [] )
          item = ImpOccItem
               { imp_item      = gre
               , imp_bundled   = bundled
               , imp_is_parent = is_parent }
    ]
  where

    hiding_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }

    -- See Note [Dealing with imports]
    -- 'combine' may be called for associated data types which appear
    -- twice in the all_avails. In the example, we have two Avails for T,
    -- namely T(T,T1,T2,T3) and C(C,T), and we combine them by dropping the
    -- latter, in which T is not the parent.
    combine :: ImpOccItem -> ImpOccItem -> ImpOccItem
    combine item1@(ImpOccItem { imp_item = gre1, imp_is_parent = is_parent1 })
            item2@(ImpOccItem { imp_item = gre2, imp_is_parent = is_parent2 })
      | is_parent1 || is_parent2
      , not (isRecFldGRE gre1 || isRecFldGRE gre2) -- NB: does not force GREInfo.
      , let name1 = greName gre1
            name2 = greName gre2
            gre = gre1 `plusGRE` gre2
              -- See Note [Preserve parent information when combining import OccEnvs]
      = assertPpr (name1 == name2)
                  (ppr name1 <+> ppr name2) $
        if is_parent1
        then item1 { imp_item = gre }
        else item2 { imp_item = gre }
      -- Discard C(C,T) in favour of T(T, T1, T2, T3).

    -- 'combine' may also be called for pattern synonyms which appear both
    -- unassociated and associated (see Note [Importing PatternSynonyms]).
    combine item1@(ImpOccItem { imp_item = c1, imp_bundled = kids1 })
            item2@(ImpOccItem { imp_item = c2, imp_bundled = kids2 })
      = assertPpr (greName c1 == greName c2
                   && (not (null kids1 && null kids2)))
                  (ppr c1 <+> ppr c2 <+> ppr kids1 <+> ppr kids2) $
        if null kids1
        then item2
        else item1
      -- Discard standalone pattern P in favour of T(P).

-- | Essentially like @lookupGRE env (LookupOccName occ which_gres)@,
-- but working with 'ImpOccItem's instead of 'GlobalRdrElt's.
lookupImpOccEnv :: WhichGREs GREInfo
                -> OccEnv (NameEnv ImpOccItem) -> OccName -> [NameEnv ImpOccItem]
lookupImpOccEnv which_gres env occ =
  mapMaybe relevant_items $ lookupOccEnv_AllNameSpaces env occ
  where
    is_relevant :: ImpOccItem -> Bool
    is_relevant (ImpOccItem { imp_item = gre }) =
      greIsRelevant which_gres (occNameSpace occ) gre
    relevant_items :: NameEnv ImpOccItem -> Maybe (NameEnv ImpOccItem)
    relevant_items nms
      | let nms' = filterNameEnv is_relevant nms
      = if isEmptyNameEnv nms'
        then Nothing
        else Just nms'

{-
************************************************************************
*                                                                      *
\subsection{Import/Export Utils}
*                                                                      *
************************************************************************
-}

-- | Given an import\/export spec, appropriately set the @gre_imp@ field
-- for the 'GlobalRdrElt's.
gresFromIE :: ImpDeclSpec -> (LIE GhcRn, [GlobalRdrElt]) -> [GlobalRdrElt]
gresFromIE decl_spec (L loc ie, gres)
  = map set_gre_imp gres
  where
    is_explicit = case ie of
                    IEThingAll _ name _ -> \n -> n == lieWrappedName name
                    _                   -> \_ -> True
    prov_fn name
      = ImpSpec { is_decl = decl_spec, is_item = item_spec }
      where
        item_spec = ImpSome { is_explicit = is_explicit name
                            , is_iloc = locA loc }
    set_gre_imp gre@( GRE { gre_name = nm } )
      = gre { gre_imp = unitBag $ prov_fn nm }

{-
Note [Children for duplicate record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the module

    {-# LANGUAGE DuplicateRecordFields #-}
    module M (F(foo, MkFInt, MkFBool)) where
      data family F a
      data instance F Int = MkFInt { foo :: Int }
      data instance F Bool = MkFBool { foo :: Bool }

The `foo` in the export list refers to *both* selectors! For this
reason, lookupChildren builds an environment that maps the FastString
to a list of items, rather than a single item.
-}

mkChildEnv :: [GlobalRdrElt] -> NameEnv [GlobalRdrElt]
mkChildEnv gres = foldr add emptyNameEnv gres
  where
    add gre env = case greParent gre of
        ParentIs  p -> extendNameEnv_Acc (:) Utils.singleton env p gre
        NoParent    -> env

findChildren :: NameEnv [a] -> Name -> [a]
findChildren env n = lookupNameEnv env n `orElse` []

lookupChildren :: [GlobalRdrElt]
               -> [LIEWrappedName GhcPs]
               -> MaybeErr [LIEWrappedName GhcPs]   -- The ones for which the lookup failed
                           [LocatedA GlobalRdrElt]
-- (lookupChildren all_kids rdr_items) maps each rdr_item to its
-- corresponding Name all_kids, if the former exists
-- The matching is done by FastString, not OccName, so that
--    Cls( meth, AssocTy )
-- will correctly find AssocTy among the all_kids of Cls, even though
-- the RdrName for AssocTy may have a (bogus) DataName namespace
-- (Really the rdr_items should be FastStrings in the first place.)
lookupChildren all_kids rdr_items
  | null fails
  = Succeeded (concat oks)
       -- This 'fmap concat' trickily applies concat to the /second/ component
       -- of the pair, whose type is ([LocatedA Name], [[Located FieldLabel]])
  | otherwise
  = Failed fails
  where
    mb_xs = map doOne rdr_items
    fails = [ bad_rdr | Failed bad_rdr <- mb_xs ]
    oks   = [ ok      | Succeeded ok   <- mb_xs ]
    oks :: [[LocatedA GlobalRdrElt]]

    doOne item@(L l r)
       = case (lookupFsEnv kid_env . occNameFS . rdrNameOcc . ieWrappedName) r of
           Just [g]
             | not $ isRecFldGRE g
             -> Succeeded [L l g]
           Just gs
             | all isRecFldGRE gs
             -> Succeeded $ map (L l) gs
           _ -> Failed    item

    -- See Note [Children for duplicate record fields]
    kid_env = extendFsEnvList_C (++) emptyFsEnv
              [(occNameFS (occName x), [x]) | x <- all_kids]


-------------------------------

{-
*********************************************************
*                                                       *
\subsection{Unused names}
*                                                       *
*********************************************************
-}

reportUnusedNames :: TcGblEnv -> HscSource -> RnM ()
reportUnusedNames gbl_env hsc_src
  = do  { keep <- readTcRef (tcg_keep gbl_env)
        ; traceRn "RUN" (ppr (tcg_dus gbl_env))
        ; warnUnusedImportDecls gbl_env hsc_src
        ; warnUnusedTopBinds $ unused_locals keep
        ; warnMissingSignatures gbl_env
        ; warnMissingKindSignatures gbl_env }
  where
    used_names :: NameSet -> NameSet
    used_names keep = findUses (tcg_dus gbl_env) emptyNameSet `unionNameSet` keep
    -- NB: currently, if f x = g, we only treat 'g' as used if 'f' is used
    -- Hence findUses

    -- Collect the defined names from the in-scope environment
    defined_names :: [GlobalRdrElt]
    defined_names = globalRdrEnvElts (tcg_rdr_env gbl_env)

    kids_env = mkChildEnv defined_names
    -- This is done in mkExports too; duplicated work

    gre_is_used :: NameSet -> GlobalRdrElt -> Bool
    gre_is_used used_names gre0
        = name `elemNameSet` used_names
          || any (\ gre -> greName gre `elemNameSet` used_names) (findChildren kids_env name)
                -- A use of C implies a use of T,
                -- if C was brought into scope by T(..) or T(C)
      where
        name = greName gre0

    -- Filter out the ones that are
    --  (a) defined in this module, and
    --  (b) not defined by a 'deriving' clause
    -- The latter have an Internal Name, so we can filter them out easily
    unused_locals :: NameSet -> [GlobalRdrElt]
    unused_locals keep =
      let -- Note that defined_and_used, defined_but_not_used
          -- are both [GRE]; that's why we need defined_and_used
          -- rather than just used_names
          _defined_and_used, defined_but_not_used :: [GlobalRdrElt]
          (_defined_and_used, defined_but_not_used)
              = partition (gre_is_used (used_names keep)) defined_names

      in filter is_unused_local defined_but_not_used
    is_unused_local :: GlobalRdrElt -> Bool
    is_unused_local gre = isLocalGRE gre
                       && isExternalName (greName gre)

{- *********************************************************************
*                                                                      *
              Missing signatures
*                                                                      *
********************************************************************* -}

{-
Note [Missing signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~
There are four warning flags in play:

  * -Wmissing-exported-signatures
    Warn about any exported top-level function/value without a type signature.
    Does not include pattern synonyms.

  * -Wmissing-signatures
    Warn about any top-level function/value without a type signature. Does not
    include pattern synonyms. Takes priority over -Wmissing-exported-signatures.

  * -Wmissing-exported-pattern-synonym-signatures
    Warn about any exported pattern synonym without a type signature.

  * -Wmissing-pattern-synonym-signatures
    Warn about any pattern synonym without a type signature. Takes priority over
    -Wmissing-exported-pattern-synonym-signatures.

-}

-- | Warn the user about top level binders that lack type signatures.
-- Called /after/ type inference, so that we can report the
-- inferred type of the function
warnMissingSignatures :: TcGblEnv -> RnM ()
warnMissingSignatures gbl_env
  = do { let exports = availsToNameSet (tcg_exports gbl_env)
             sig_ns  = tcg_sigs gbl_env
               -- We use sig_ns to exclude top-level bindings that are generated by GHC
             binds    = collectHsBindsBinders CollNoDictBinders $ tcg_binds gbl_env
             pat_syns = tcg_patsyns gbl_env

             not_ghc_generated :: Name -> Bool
             not_ghc_generated name = name `elemNameSet` sig_ns

             add_binding_warn :: Id -> RnM ()
             add_binding_warn id =
               when (not_ghc_generated name) $
               do { env <- liftZonkM $ tcInitTidyEnv -- Why not use emptyTidyEnv?
                  ; let ty = tidyOpenType env (idType id)
                        missing = MissingTopLevelBindingSig name ty
                        diag = TcRnMissingSignature missing exported
                  ; addDiagnosticAt (getSrcSpan name) diag }
               where
                 name = idName id
                 exported = if name `elemNameSet` exports
                            then IsExported
                            else IsNotExported

             add_patsyn_warn :: PatSyn -> RnM ()
             add_patsyn_warn ps =
               when (not_ghc_generated name) $
                 addDiagnosticAt (getSrcSpan name)
                  (TcRnMissingSignature missing exported)
               where
                 name = patSynName ps
                 missing = MissingPatSynSig ps
                 exported = if name `elemNameSet` exports
                            then IsExported
                            else IsNotExported

         -- Warn about missing signatures
         -- Do this only when we have a type to offer
         -- See Note [Missing signatures]
       ; mapM_ add_binding_warn binds
       ; mapM_ add_patsyn_warn  pat_syns
       }

-- | Warn the user about tycons that lack kind signatures.
-- Called /after/ type (and kind) inference, so that we can report the
-- inferred kinds.
warnMissingKindSignatures :: TcGblEnv -> RnM ()
warnMissingKindSignatures gbl_env
  = do { cusks_enabled <- xoptM LangExt.CUSKs
       ; mapM_ (add_ty_warn cusks_enabled) tcs
       }
  where
    tcs = tcg_tcs gbl_env
    ksig_ns = tcg_ksigs gbl_env
    exports = availsToNameSet (tcg_exports gbl_env)

    has_kind_signature :: Name -> Bool
    has_kind_signature name = name `elemNameSet` ksig_ns

    add_ty_warn :: Bool -> TyCon -> RnM ()
    add_ty_warn cusks_enabled tyCon =
      when (has_kind_signature name) $
        addDiagnosticAt (getSrcSpan name) diag
      where
        name = tyConName tyCon
        diag = TcRnMissingSignature missing exported
        missing = MissingTyConKindSig tyCon cusks_enabled
        exported = if name `elemNameSet` exports
                   then IsExported
                   else IsNotExported

{-
*********************************************************
*                                                       *
\subsection{Unused imports}
*                                                       *
*********************************************************

This code finds which import declarations are unused.  The
specification and implementation notes are here:
  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/unused-imports

See also Note [Choosing the best import declaration] in GHC.Types.Name.Reader
-}

type ImportDeclUsage
   = ( LImportDecl GhcRn   -- The import declaration
     , [GlobalRdrElt]      -- What *is* used (normalised)
     , [Name] )            -- What is imported but *not* used

warnUnusedImportDecls :: TcGblEnv -> HscSource -> RnM ()
warnUnusedImportDecls gbl_env hsc_src
  = do { uses <- readMutVar (tcg_used_gres gbl_env)
       ; let user_imports = filterOut
                              (ideclImplicit . ideclExt . unLoc)
                              (tcg_rn_imports gbl_env)
                -- This whole function deals only with *user* imports
                -- both for warning about unnecessary ones, and for
                -- deciding the minimal ones
             rdr_env = tcg_rdr_env gbl_env

       ; let usage :: [ImportDeclUsage]
             usage = findImportUsage user_imports uses

       ; traceRn "warnUnusedImportDecls" $
                       (vcat [ text "Uses:" <+> ppr uses
                             , text "Import usage" <+> ppr usage])

       ; mapM_ (warnUnusedImport rdr_env) usage

       ; whenGOptM Opt_D_dump_minimal_imports $
         printMinimalImports hsc_src usage }

findImportUsage :: [LImportDecl GhcRn]
                -> [GlobalRdrElt]
                -> [ImportDeclUsage]

findImportUsage imports used_gres
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage = mkImportMap used_gres

    unused_decl :: LImportDecl GhcRn -> (LImportDecl GhcRn, [GlobalRdrElt], [Name])
    unused_decl decl@(L loc (ImportDecl { ideclImportList = imps }))
      = (decl, used_gres, nameSetElemsStable unused_imps)
      where
        used_gres = lookupSrcLoc (srcSpanEnd $ locA loc) import_usage
                               -- srcSpanEnd: see Note [The ImportMap]
                    `orElse` []

        used_names   = mkNameSet (map      greName        used_gres)
        used_parents = mkNameSet (mapMaybe greParent_maybe used_gres)

        unused_imps   -- Not trivial; see eg #7454
          = case imps of
              Just (Exactly, L _ imp_ies) ->
                                 foldr (add_unused . unLoc) emptyNameSet imp_ies
              _other -> emptyNameSet -- No explicit import list => no unused-name list

        add_unused :: IE GhcRn -> NameSet -> NameSet
        add_unused (IEVar _ n _)    acc   = add_unused_name (lieWrappedName n) acc
        add_unused (IEThingAbs _ n _) acc = add_unused_name (lieWrappedName n) acc
        add_unused (IEThingAll _ n _) acc = add_unused_all  (lieWrappedName n) acc
        add_unused (IEThingWith _ p wc ns _) acc =
          add_wc_all (add_unused_with pn xs acc)
          where pn = lieWrappedName p
                xs = map lieWrappedName ns
                add_wc_all = case wc of
                            NoIEWildcard -> id
                            IEWildcard _ -> add_unused_all pn
        add_unused _ acc = acc

        add_unused_name n acc
          | n `elemNameSet` used_names = acc
          | otherwise                  = acc `extendNameSet` n
        add_unused_all n acc
          | n `elemNameSet` used_names   = acc
          | n `elemNameSet` used_parents = acc
          | otherwise                    = acc `extendNameSet` n
        add_unused_with p ns acc
          | all (`elemNameSet` acc1) ns = add_unused_name p acc1
          | otherwise = acc1
          where
            acc1 = foldr add_unused_name acc ns
       -- If you use 'signum' from Num, then the user may well have
       -- imported Num(signum).  We don't want to complain that
       -- Num is not itself mentioned.  Hence the two cases in add_unused_with.


{- Note [The ImportMap]
~~~~~~~~~~~~~~~~~~~~~~~
The ImportMap is a short-lived intermediate data structure records, for
each import declaration, what stuff brought into scope by that
declaration is actually used in the module.

The SrcLoc is the location of the END of a particular 'import'
declaration.  Why *END*?  Because we don't want to get confused
by the implicit Prelude import. Consider (#7476) the module
    import Foo( foo )
    main = print foo
There is an implicit 'import Prelude(print)', and it gets a SrcSpan
of line 1:1 (just the point, not a span). If we use the *START* of
the SrcSpan to identify the import decl, we'll confuse the implicit
import Prelude with the explicit 'import Foo'.  So we use the END.
It's just a cheap hack; we could equally well use the Span too.

The [GlobalRdrElt] are the things imported from that decl.
-}

type ImportMap = Map RealSrcLoc [GlobalRdrElt]  -- See [The ImportMap]
     -- If loc :-> gres, then
     --   'loc' = the end loc of the bestImport of each GRE in 'gres'

mkImportMap :: [GlobalRdrElt] -> ImportMap
-- For each of a list of used GREs, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
mkImportMap gres
  = foldr add_one Map.empty gres
  where
    add_one gre@(GRE { gre_imp = imp_specs }) imp_map =
      case srcSpanEnd (is_dloc (is_decl best_imp_spec)) of
                              -- For srcSpanEnd see Note [The ImportMap]
       RealSrcLoc decl_loc _ -> Map.insertWith add decl_loc [gre] imp_map
       UnhelpfulLoc _ -> imp_map
       where
          best_imp_spec =
            case bagToList imp_specs of
              []     -> pprPanic "mkImportMap: GRE with no ImportSpecs" (ppr gre)
              is:iss -> bestImport (is NE.:| iss)
          add _ gres = gre : gres

warnUnusedImport :: GlobalRdrEnv -> ImportDeclUsage -> RnM ()
warnUnusedImport rdr_env (L loc decl, used, unused)

  -- Do not warn for 'import M()'
  | Just (Exactly, L _ []) <- ideclImportList decl
  = return ()

  -- Note [Do not warn about Prelude hiding]
  | Just (EverythingBut, L _ hides) <- ideclImportList decl
  , not (null hides)
  , pRELUDE_NAME == unLoc (ideclName decl)
  = return ()

  -- Nothing used; drop entire declaration
  | null used
  = addDiagnosticAt (locA loc) (TcRnUnusedImport decl UnusedImportNone)

  -- Everything imported is used; nop
  | null unused
  = return ()

  -- Only one import is unused, with `SrcSpan` covering only the unused item instead of
  -- the whole import statement
  | Just (_, L _ imports) <- ideclImportList decl
  , length unused == 1
  , Just (L loc _) <- find (\(L _ ie) -> ((ieName ie) :: Name) `elem` unused) imports
  = addDiagnosticAt (locA loc) (TcRnUnusedImport decl (UnusedImportSome sort_unused))

  -- Some imports are unused
  | otherwise
  = addDiagnosticAt (locA loc) (TcRnUnusedImport decl (UnusedImportSome sort_unused))

  where
    -- In warning message, pretty-print identifiers unqualified unconditionally
    -- to improve the consistent for ambiguous/unambiguous identifiers.
    -- See #14881.
    possible_field n =
      case lookupGRE_Name rdr_env n of
        Just (GRE { gre_par = par, gre_info = IAmRecField info }) ->
          let fld_occ :: OccName
              fld_occ = nameOccName $ flSelector $ recFieldLabel info
          in UnusedImportNameRecField par fld_occ
        _  -> UnusedImportNameRegular n

    -- Print unused names in a deterministic (lexicographic) order
    sort_unused :: [UnusedImportName]
    sort_unused = fmap possible_field $
                  sortBy (comparing nameOccName) unused

{-
Note [Do not warn about Prelude hiding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not warn about
   import Prelude hiding( x, y )
because even if nothing else from Prelude is used, it may be essential to hide
x,y to avoid name-shadowing warnings.  Example (#9061)
   import Prelude hiding( log )
   f x = log where log = ()


Note [Printing minimal imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To print the minimal imports we walk over the user-supplied import
decls, and simply trim their import lists.  NB that

  * We do *not* change the 'qualified' or 'as' parts!

  * We do not discard a decl altogether; we might need instances
    from it.  Instead we just trim to an empty import list
-}

getMinimalImports :: [ImportDeclUsage] -> RnM [LImportDecl GhcRn]
getMinimalImports ie_decls
  = do { rdr_env <- getGlobalRdrEnv
       ; fmap combine $ mapM (mk_minimal rdr_env) ie_decls }
  where
    mk_minimal rdr_env (L l decl, used_gres, unused)
      | null unused
      , Just (Exactly, _) <- ideclImportList decl
      = return (L l decl)
      | otherwise
      = do { let ImportDecl { ideclName    = L _ mod_name
                            , ideclSource  = is_boot
                            , ideclPkgQual = pkg_qual } = decl
           ; iface <- loadSrcInterface doc mod_name is_boot pkg_qual
           ; let used_avails = gresToAvailInfo used_gres
           ; lies <- map (L l) <$> concatMapM (to_ie rdr_env iface) used_avails
           ; return (L l (decl { ideclImportList = Just (Exactly, L (l2l l) lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: GlobalRdrEnv -> ModIface -> AvailInfo -> RnM [IE GhcRn]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie rdr_env _ (Avail c)  -- Note [Overloaded field import]
      = do { let
               gre = expectJust $ lookupGRE_Name rdr_env c
           ; return $ [IEVar Nothing (to_ie_post_rn $ noLocA $ greName gre) Nothing] }
    to_ie _ _ avail@(AvailTC n [_])  -- Exporting the main decl and nothing else
      | availExportsDecl avail
      = return [IEThingAbs Nothing (to_ie_post_rn $ noLocA n) Nothing]
    to_ie rdr_env iface (AvailTC n cs) =
      case [ xs | avail@(AvailTC x xs) <- mi_exports iface
           , x == n
           , availExportsDecl avail  -- Note [Partial export]
           ] of
        [xs]
          | all_used xs
          -> return [IEThingAll (Nothing, noAnn) (to_ie_post_rn $ noLocA n) Nothing]
          | otherwise
          -> do { let ns_gres = map (expectJust . lookupGRE_Name rdr_env) cs
                      ns = map greName ns_gres
                ; return [IEThingWith (Nothing, noAnn) (to_ie_post_rn $ noLocA n) NoIEWildcard
                                 (map (to_ie_post_rn . noLocA) (filter (/= n) ns)) Nothing] }
                                       -- Note [Overloaded field import]
        _other
          -> do { let infos = map (expectJust . lookupGRE_Name rdr_env) cs
                      (ns_gres,fs_gres) = classifyGREs infos
                      ns = map greName (ns_gres ++ fs_gres)
                      fs = map fieldGREInfo fs_gres
                ; return $
                  if all_non_overloaded fs
                  then map (\nm -> IEVar Nothing (to_ie_post_rn_var $ noLocA nm) Nothing) ns
                  else [IEThingWith (Nothing, noAnn) (to_ie_post_rn $ noLocA n) NoIEWildcard
                         (map (to_ie_post_rn . noLocA) (filter (/= n) ns)) Nothing] }
        where

          all_used avail_cs = all (`elem` cs) avail_cs

          all_non_overloaded = all (not . flIsOverloaded . recFieldLabel)

    combine :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
    combine = map merge . NE.groupAllWith getKey

    getKey :: LImportDecl GhcRn -> (Bool, Maybe ModuleName, ModuleName)
    getKey decl =
      ( isImportDeclQualified . ideclQualified $ idecl -- is this qualified? (important that this be first)
      , unLoc <$> ideclAs idecl -- what is the qualifier (inside Maybe monad)
      , unLoc . ideclName $ idecl -- Module Name
      )
      where
        idecl :: ImportDecl GhcRn
        idecl = unLoc decl

    merge :: NonEmpty (LImportDecl GhcRn) -> LImportDecl GhcRn
    merge decls@((L l decl) :| _) = L l (decl { ideclImportList = Just (Exactly, L (noAnnSrcSpan (locA l)) lies) })
      where lies = concatMap (unLoc . snd) $ mapMaybe (ideclImportList . unLoc) $ NE.toList decls

classifyGREs :: [GlobalRdrElt] -> ([GlobalRdrElt], [FieldGlobalRdrElt])
classifyGREs = partition (not . isRecFldGRE)

printMinimalImports :: HscSource -> [ImportDeclUsage] -> RnM ()
-- See Note [Printing minimal imports]
printMinimalImports hsc_src imports_w_usage
  = do { imports' <- getMinimalImports imports_w_usage
       ; this_mod <- getModule
       ; dflags   <- getDynFlags
       ; liftIO $ withFile (mkFilename dflags this_mod) WriteMode $ \h ->
          printForUser dflags h neverQualify AllTheWay (vcat (map ppr imports'))
              -- The neverQualify is important.  We are printing Names
              -- but they are in the context of an 'import' decl, and
              -- we never qualify things inside there
              -- E.g.   import Blag( f, b )
              -- not    import Blag( Blag.f, Blag.g )!
       }
  where
    mkFilename dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise                = basefn
      where
        suffix = case hsc_src of
                     HsBootFile -> ".imports-boot"
                     HsSrcFile  -> ".imports"
                     HsigFile   -> ".imports"
        basefn = moduleNameString (moduleName this_mod) ++ suffix


to_ie_post_rn_var :: LocatedA (IdP GhcRn) -> LIEWrappedName GhcRn
to_ie_post_rn_var (L l n)
  | isDataOcc $ occName n = L l (IEPattern noAnn      (L (l2l l) n))
  | otherwise             = L l (IEName    noExtField (L (l2l l) n))


to_ie_post_rn :: LocatedA (IdP GhcRn) -> LIEWrappedName GhcRn
to_ie_post_rn (L l n)
  | isTcOcc occ && isSymOcc occ = L l (IEType noAnn      (L (l2l l) n))
  | otherwise                   = L l (IEName noExtField (L (l2l l) n))
  where occ = occName n

{-
Note [Partial export]
~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   module A( op ) where
     class C a where
       op :: a -> a

   module B where
   import A
   f = ..op...

Then the minimal import for module B is
   import A( op )
not
   import A( C( op ) )
which we would usually generate if C was exported from B.  Hence
the availExportsDecl test when deciding what to generate.


Note [Overloaded field import]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the other hand, if we have

    {-# LANGUAGE DuplicateRecordFields #-}
    module A where
      data T = MkT { foo :: Int }

    module B where
      import A
      f = ...foo...

then the minimal import for module B must be
    import A ( T(foo) )
because when DuplicateRecordFields is enabled, field selectors are
not in scope without their enclosing datatype.

On the third hand, if we have

    {-# LANGUAGE DuplicateRecordFields #-}
    module A where
      pattern MkT { foo } = Just foo

    module B where
      import A
      f = ...foo...

then the minimal import for module B must be
    import A ( foo )
because foo doesn't have a parent.  This might actually be ambiguous if A
exports another field called foo, but there is no good answer to return and this
is a very obscure corner, so it seems to be the best we can do.  See
DRFPatSynExport for a test of this.


************************************************************************
*                                                                      *
\subsection{Errors}
*                                                                      *
************************************************************************
-}

badImportItemErr
  :: ModIface -> ImpDeclSpec -> IE GhcPs -> IsSubordinate
  -> [AvailInfo]
  -> TcRn ImportLookupReason
badImportItemErr iface decl_spec ie sub avails = do
  patsyns_enabled <- xoptM LangExt.PatternSynonyms
  expl_ns_enabled <- xoptM LangExt.ExplicitNamespaces
  dflags <- getDynFlags
  hsc_env <- getTopEnv
  let rdr_env = mkGlobalRdrEnv
              $ gresFromAvails hsc_env (Just imp_spec) all_avails
  pure (ImportLookupBad (importErrorKind dflags rdr_env expl_ns_enabled) iface decl_spec ie patsyns_enabled)
  where
    importErrorKind dflags rdr_env expl_ns_enabled
      | any checkIfTyCon avails = case sub of
          IsNotSubordinate -> BadImportAvailTyCon expl_ns_enabled
          IsSubordinate -> BadImportNotExportedSubordinates unavailableChildren
      | any checkIfVarName avails = BadImportAvailVar
      | Just con <- find checkIfDataCon avails = BadImportAvailDataCon (availOccName con)
      | otherwise = BadImportNotExported suggs
        where
          suggs = similar_suggs ++ fieldSelectorSuggestions rdr_env rdr
          similar_names =
            similarNameSuggestions (Unbound.LF WL_Anything WL_Global)
              dflags rdr_env emptyLocalRdrEnv rdr
          similar_suggs =
            case NE.nonEmpty $ mapMaybe imported_item $ similar_names of
              Just similar -> [ SuggestSimilarNames rdr similar ]
              Nothing      -> [ ]

          -- Only keep imported items, and set the "HowInScope" to
          -- "Nothing" to avoid printing "imported from..." in the suggestion
          -- error message.
          imported_item (SimilarRdrName rdr_name (Just (ImportedBy {})))
            = Just (SimilarRdrName rdr_name Nothing)
          imported_item _ = Nothing

    checkIfDataCon = checkIfAvailMatches isDataConName
    checkIfTyCon = checkIfAvailMatches isTyConName
    checkIfVarName =
      \case
        AvailTC{} -> False
        Avail n -> importedFS == occNameFS (occName n)
                && (isVarOcc <||> isFieldOcc) (occName n)
    checkIfAvailMatches namePred =
      \case
        AvailTC _ ns ->
          case find (\n -> importedFS == occNameFS (occName n)) ns of
            Just n  -> namePred n
            Nothing -> False
        Avail{} -> False
    availOccName = occName . availName
    rdr = ieName ie
    importedFS = occNameFS $ rdrNameOcc rdr
    imp_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }
    all_avails = mi_exports iface
    unavailableChildren = case ie of
      IEThingWith _ _ _ ns _ -> map (rdrNameOcc . ieWrappedName  . unLoc) ns
      _ -> panic "importedChildren failed pattern match: no children"

addDupDeclErr :: NonEmpty GlobalRdrElt -> TcRn ()
addDupDeclErr gres@(gre :| _)
  -- Report the error at the later location
  = addErrAt (getSrcSpan (NE.last sorted_names)) $ (TcRnDuplicateDecls (greOccName gre) sorted_names)
  where
    sorted_names =
      NE.sortBy (SrcLoc.leftmost_smallest `on` nameSrcSpan)
        (fmap greName gres)

-- This data decl will parse OK
--      data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--      data T = :% Int Int
-- from interface files, which always print in prefix form
--
-- We also allow type constructor names, which are defined by "type data"
-- declarations.  See Note [Type data declarations] in GHC.Rename.Module.

checkConName :: RdrName -> TcRn ()
checkConName name
  = checkErr (isRdrDataCon name || isRdrTc name) (TcRnIllegalDataCon name)
