{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnNames]{Extracting imported and top-level names in scope}
-}

{-# LANGUAGE CPP, NondecreasingIndentation, MultiWayIf, NamedFieldPuns #-}

module RnNames (
        rnImports, getLocalNonValBinders, newRecordSelector,
        extendGlobalRdrEnvRn,
        gresFromAvails,
        calculateAvails,
        reportUnusedNames,
        checkConName,
        mkChildEnv,
        findChildren,
        dodgyMsg
    ) where

#include "HsVersions.h"

import DynFlags
import HsSyn
import TcEnv
import RnEnv
import LoadIface        ( loadSrcInterface )
import TcRnMonad
import PrelNames
import Module
import Name
import NameEnv
import NameSet
import Avail
import FieldLabel
import HscTypes
import RdrName
import RdrHsSyn        ( setRdrNameSpace )
import Outputable
import Maybes
import SrcLoc
import BasicTypes      ( TopLevelFlag(..), StringLiteral(..) )
import Util
import FastString
import FastStringEnv
import Id
import Type
import PatSyn
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Either      ( partitionEithers, isRight, rights )
-- import qualified Data.Foldable as Foldable
import Data.Map         ( Map )
import qualified Data.Map as Map
import Data.Ord         ( comparing )
import Data.List        ( partition, (\\), find, sortBy )
-- import qualified Data.Set as Set
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
according to the rules outlined in the Note [HscMain . Safe Haskell Trust Check]
we must also check that these rules hold transitively for all dependent modules
and packages. Doing this without caching any trust information would be very
slow as we would need to touch all packages and interface files a module depends
on. To avoid this we make use of the property that if a modules Safe Haskell
mode changes, this triggers a recompilation from that module in the dependcy
graph. So we can just worry mostly about direct imports.

There is one trust property that can change for a package though without
recompliation being triggered: package trust. So we must check that all
packages a module tranitively depends on to be trusted are still trusted when
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
HscMain.checkSafeImports.

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
haskell at all and simply imports B, should A inherit all the the trust
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
rnImports :: [LImportDecl RdrName]
          -> RnM ([LImportDecl Name], GlobalRdrEnv, ImportAvails, AnyHpcUsage)
rnImports imports = do
    tcg_env <- getGblEnv
    -- NB: want an identity module here, because it's OK for a signature
    -- module to import from its implementor
    let this_mod = tcg_mod tcg_env
    let (source, ordinary) = partition is_source_import imports
        is_source_import d = ideclSource (unLoc d)
    stuff1 <- mapAndReportM (rnImportDecl this_mod) ordinary
    stuff2 <- mapAndReportM (rnImportDecl this_mod) source
    -- Safe Haskell: See Note [Tracking Trust Transitively]
    let (decls, rdr_env, imp_avails, hpc_usage) = combine (stuff1 ++ stuff2)
    return (decls, rdr_env, imp_avails, hpc_usage)

  where
    combine :: [(LImportDecl Name,  GlobalRdrEnv, ImportAvails, AnyHpcUsage)]
            -> ([LImportDecl Name], GlobalRdrEnv, ImportAvails, AnyHpcUsage)
    combine = foldr plus ([], emptyGlobalRdrEnv, emptyImportAvails, False)

    plus (decl,  gbl_env1, imp_avails1,hpc_usage1)
         (decls, gbl_env2, imp_avails2,hpc_usage2)
      = ( decl:decls,
          gbl_env1 `plusGlobalRdrEnv` gbl_env2,
          imp_avails1 `plusImportAvails` imp_avails2,
          hpc_usage1 || hpc_usage2 )

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
rnImportDecl  :: Module -> LImportDecl RdrName
              -> RnM (LImportDecl Name, GlobalRdrEnv, ImportAvails, AnyHpcUsage)
rnImportDecl this_mod
             (L loc decl@(ImportDecl { ideclName = loc_imp_mod_name, ideclPkgQual = mb_pkg
                                     , ideclSource = want_boot, ideclSafe = mod_safe
                                     , ideclQualified = qual_only, ideclImplicit = implicit
                                     , ideclAs = as_mod, ideclHiding = imp_details }))
  = setSrcSpan loc $ do

    when (isJust mb_pkg) $ do
        pkg_imports <- xoptM LangExt.PackageImports
        when (not pkg_imports) $ addErr packageImportErr

    -- If there's an error in loadInterface, (e.g. interface
    -- file not found) we get lots of spurious errors from 'filterImports'
    let imp_mod_name = unLoc loc_imp_mod_name
        doc = ppr imp_mod_name <+> text "is directly imported"

    -- Check for self-import, which confuses the typechecker (Trac #9032)
    -- ghc --make rejects self-import cycles already, but batch-mode may not
    -- at least not until TcIface.tcHiBootIface, which is too late to avoid
    -- typechecker crashes.  (Indirect self imports are not caught until
    -- TcIface, see #10337 tracking how to make this error better.)
    --
    -- Originally, we also allowed 'import {-# SOURCE #-} M', but this
    -- caused bug #10182: in one-shot mode, we should never load an hs-boot
    -- file for the module we are compiling into the EPS.  In principle,
    -- it should be possible to support this mode of use, but we would have to
    -- extend Provenance to support a local definition in a qualified location.
    -- For now, we don't support it, but see #10336
    when (imp_mod_name == moduleName this_mod &&
          (case mb_pkg of  -- If we have import "<pkg>" M, then we should
                           -- check that "<pkg>" is "this" (which is magic)
                           -- or the name of this_mod's package.  Yurgh!
                           -- c.f. GHC.findModule, and Trac #9997
             Nothing         -> True
             Just (StringLiteral _ pkg_fs) -> pkg_fs == fsLit "this" ||
                            fsToUnitId pkg_fs == moduleUnitId this_mod))
         (addErr (text "A module cannot import itself:" <+> ppr imp_mod_name))

    -- Check for a missing import list (Opt_WarnMissingImportList also
    -- checks for T(..) items but that is done in checkDodgyImport below)
    case imp_details of
        Just (False, _) -> return () -- Explicit import list
        _  | implicit   -> return () -- Do not bleat for implicit imports
           | qual_only  -> return ()
           | otherwise  -> whenWOptM Opt_WarnMissingImportList $
                           addWarn (Reason Opt_WarnMissingImportList)
                                   (missingImportListWarn imp_mod_name)

    iface <- loadSrcInterface doc imp_mod_name want_boot (fmap sl_fs mb_pkg)

    -- Compiler sanity check: if the import didn't say
    -- {-# SOURCE #-} we should not get a hi-boot file
    WARN( not want_boot && mi_boot iface, ppr imp_mod_name ) do

    -- Issue a user warning for a redundant {- SOURCE -} import
    -- NB that we arrange to read all the ordinary imports before
    -- any of the {- SOURCE -} imports.
    --
    -- in --make and GHCi, the compilation manager checks for this,
    -- and indeed we shouldn't do it here because the existence of
    -- the non-boot module depends on the compilation order, which
    -- is not deterministic.  The hs-boot test can show this up.
    dflags <- getDynFlags
    warnIf NoReason
           (want_boot && not (mi_boot iface) && isOneShot (ghcMode dflags))
           (warnRedundantSourceImport imp_mod_name)
    when (mod_safe && not (safeImportsOn dflags)) $
        addErr (text "safe import can't be used as Safe Haskell isn't on!"
                $+$ ptext (sLit $ "please enable Safe Haskell through either "
                                   ++ "Safe, Trustworthy or Unsafe"))

    let
        qual_mod_name = fmap unLoc as_mod `orElse` imp_mod_name
        imp_spec  = ImpDeclSpec { is_mod = imp_mod_name, is_qual = qual_only,
                                  is_dloc = loc, is_as = qual_mod_name }

    -- filter the imports according to the import declaration
    (new_imp_details, gres) <- filterImports iface imp_spec imp_details

    -- for certain error messages, we’d like to know what could be imported
    -- here, if everything were imported
    potential_gres <- mkGlobalRdrEnv . snd <$> filterImports iface imp_spec Nothing

    let gbl_env = mkGlobalRdrEnv gres

        is_hiding | Just (True,_) <- imp_details = True
                  | otherwise                    = False

        -- should the import be safe?
        mod_safe' = mod_safe
                    || (not implicit && safeDirectImpsReq dflags)
                    || (implicit && safeImplicitImpsReq dflags)

    let imv = ImportedModsVal
            { imv_name        = qual_mod_name
            , imv_span        = loc
            , imv_is_safe     = mod_safe'
            , imv_is_hiding   = is_hiding
            , imv_all_exports = potential_gres
            , imv_qualified   = qual_only
            }
    let imports
          = (calculateAvails dflags iface mod_safe' want_boot)
                { imp_mods = unitModuleEnv (mi_module iface) [imv] }

    -- Complain if we import a deprecated module
    whenWOptM Opt_WarnWarningsDeprecations (
       case (mi_warns iface) of
          WarnAll txt -> addWarn (Reason Opt_WarnWarningsDeprecations)
                                (moduleWarn imp_mod_name txt)
          _           -> return ()
     )

    let new_imp_decl = L loc (decl { ideclSafe = mod_safe'
                                   , ideclHiding = new_imp_details })

    return (new_imp_decl, gbl_env, imports, mi_hpc iface)

-- | Calculate the 'ImportAvails' induced by an import of a particular
-- interface, but without 'imp_mods'.
calculateAvails :: DynFlags
                -> ModIface
                -> IsSafeImport
                -> IsBootInterface
                -> ImportAvails
calculateAvails dflags iface mod_safe' want_boot =
  let imp_mod    = mi_module iface
      imp_sem_mod= mi_semantic_module iface
      orph_iface = mi_orphan iface
      has_finsts = mi_finsts iface
      deps       = mi_deps iface
      trust      = getSafeMode $ mi_trust iface
      trust_pkg  = mi_trust_pkg iface

      -- If the module exports anything defined in this module, just
      -- ignore it.  Reason: otherwise it looks as if there are two
      -- local definition sites for the thing, and an error gets
      -- reported.  Easiest thing is just to filter them out up
      -- front. This situation only arises if a module imports
      -- itself, or another module that imported it.  (Necessarily,
      -- this invoves a loop.)
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

      orphans | orph_iface = ASSERT2( not (imp_sem_mod `elem` dep_orphs deps), ppr imp_sem_mod <+> ppr (dep_orphs deps) )
                             imp_sem_mod : dep_orphs deps
              | otherwise  = dep_orphs deps

      finsts | has_finsts = ASSERT2( not (imp_sem_mod `elem` dep_finsts deps), ppr imp_sem_mod <+> ppr (dep_orphs deps) )
                            imp_sem_mod : dep_finsts deps
             | otherwise  = dep_finsts deps

      pkg = moduleUnitId (mi_module iface)
      ipkg = toInstalledUnitId pkg

      -- Does this import mean we now require our own pkg
      -- to be trusted? See Note [Trust Own Package]
      ptrust = trust == Sf_Trustworthy || trust_pkg

      (dependent_mods, dependent_pkgs, pkg_trust_req)
         | pkg == thisPackage dflags =
            -- Imported module is from the home package
            -- Take its dependent modules and add imp_mod itself
            -- Take its dependent packages unchanged
            --
            -- NB: (dep_mods deps) might include a hi-boot file
            -- for the module being compiled, CM. Do *not* filter
            -- this out (as we used to), because when we've
            -- finished dealing with the direct imports we want to
            -- know if any of them depended on CM.hi-boot, in
            -- which case we should do the hi-boot consistency
            -- check.  See LoadIface.loadHiBootInterface
            ((moduleName imp_mod,want_boot):dep_mods deps,dep_pkgs deps,ptrust)

         | otherwise =
            -- Imported module is from another package
            -- Dump the dependent modules
            -- Add the package imp_mod comes from to the dependent packages
            ASSERT2( not (ipkg `elem` (map fst $ dep_pkgs deps))
                   , ppr ipkg <+> ppr (dep_pkgs deps) )
            ([], (ipkg, False) : dep_pkgs deps, False)

  in ImportAvails {
          imp_mods       = emptyModuleEnv, -- this gets filled in later
          imp_orphs      = orphans,
          imp_finsts     = finsts,
          imp_dep_mods   = mkModDeps dependent_mods,
          imp_dep_pkgs   = map fst $ dependent_pkgs,
          -- Add in the imported modules trusted package
          -- requirements. ONLY do this though if we import the
          -- module as a safe import.
          -- See Note [Tracking Trust Transitively]
          -- and Note [Trust Transitive Property]
          imp_trust_pkgs = if mod_safe'
                               then map fst $ filter snd dependent_pkgs
                               else [],
          -- Do we require our own pkg to be trusted?
          -- See Note [Trust Own Package]
          imp_trust_own_pkg = pkg_trust_req
     }


warnRedundantSourceImport :: ModuleName -> SDoc
warnRedundantSourceImport mod_name
  = text "Unnecessary {-# SOURCE #-} in the import of module"
          <+> quotes (ppr mod_name)

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
See also: Note [Interactively-bound Ids in GHCi] in HscTypes
          Note [Looking up Exact RdrNames] in RnEnv

Consider a Template Haskell declaration quotation like this:
      module M where
        f x = h [d| f = 3 |]
When renaming the declarations inside [d| ...|], we treat the
top level binders specially in two ways

1.  We give them an Internal Name, not (as usual) an External one.
    This is done by RnEnv.newTopSrcBinder.

2.  We make them *shadow* the outer bindings.
    See Note [GlobalRdrEnv shadowing]

3. We find out whether we are inside a [d| ... |] by testing the TH
   stage. This is a slight hack, because the stage field was really
   meant for the type checker, and here we are not interested in the
   fields of Brack, hence the error thunks in thRnBrack.
-}

extendGlobalRdrEnvRn :: [AvailInfo]
                     -> MiniFixityEnv
                     -> RnM (TcGblEnv, TcLclEnv)
-- Updates both the GlobalRdrEnv and the FixityEnv
-- We return a new TcLclEnv only because we might have to
-- delete some bindings from it;
-- see Note [Top-level Names in Template Haskell decl quotes]

extendGlobalRdrEnvRn avails new_fixities
  = do  { (gbl_env, lcl_env) <- getEnvs
        ; stage <- getStage
        ; isGHCi <- getIsGHCi
        ; let rdr_env  = tcg_rdr_env gbl_env
              fix_env  = tcg_fix_env gbl_env
              th_bndrs = tcl_th_bndrs lcl_env
              th_lvl   = thLevel stage

              -- Delete new_occs from global and local envs
              -- If we are in a TemplateHaskell decl bracket,
              --    we are going to shadow them
              -- See Note [GlobalRdrEnv shadowing]
              inBracket = isBrackStage stage

              lcl_env_TH = lcl_env { tcl_rdr = delLocalRdrEnvList (tcl_rdr lcl_env) new_occs }
                           -- See Note [GlobalRdrEnv shadowing]

              lcl_env2 | inBracket = lcl_env_TH
                       | otherwise = lcl_env

              -- Deal with shadowing: see Note [GlobalRdrEnv shadowing]
              want_shadowing = isGHCi || inBracket
              rdr_env1 | want_shadowing = shadowNames rdr_env new_names
                       | otherwise      = rdr_env

              lcl_env3 = lcl_env2 { tcl_th_bndrs = extendNameEnvList th_bndrs
                                                       [ (n, (TopLevel, th_lvl))
                                                       | n <- new_names ] }

        ; rdr_env2 <- foldlM add_gre rdr_env1 new_gres

        ; let fix_env' = foldl extend_fix_env fix_env new_gres
              gbl_env' = gbl_env { tcg_rdr_env = rdr_env2, tcg_fix_env = fix_env' }

        ; traceRn "extendGlobalRdrEnvRn 2" (pprGlobalRdrEnv True rdr_env2)
        ; return (gbl_env', lcl_env3) }
  where
    new_names = concatMap availNames avails
    new_occs  = map nameOccName new_names

    -- If there is a fixity decl for the gre, add it to the fixity env
    extend_fix_env fix_env gre
      | Just (L _ fi) <- lookupFsEnv new_fixities (occNameFS occ)
      = extendNameEnv fix_env name (FixItem occ fi)
      | otherwise
      = fix_env
      where
        name = gre_name gre
        occ  = greOccName gre

    new_gres :: [GlobalRdrElt]  -- New LocalDef GREs, derived from avails
    new_gres = concatMap localGREsFromAvail avails

    add_gre :: GlobalRdrEnv -> GlobalRdrElt -> RnM GlobalRdrEnv
    -- Extend the GlobalRdrEnv with a LocalDef GRE
    -- If there is already a LocalDef GRE with the same OccName,
    --    report an error and discard the new GRE
    -- This establishes INVARIANT 1 of GlobalRdrEnvs
    add_gre env gre
      | not (null dups)    -- Same OccName defined twice
      = do { addDupDeclErr (gre : dups); return env }

      | otherwise
      = return (extendGlobalRdrEnv env gre)
      where
        name = gre_name gre
        occ  = nameOccName name
        dups = filter isLocalGRE (lookupGlobalRdrEnv env occ)


{- *********************************************************************
*                                                                      *
    getLocalDeclBindersd@ returns the names for an HsDecl
             It's used for source code.

        *** See Note [The Naming story] in HsDecls ****
*                                                                      *
********************************************************************* -}

getLocalNonValBinders :: MiniFixityEnv -> HsGroup RdrName
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
        ; overload_ok <- xoptM LangExt.DuplicateRecordFields
        ; (tc_avails, tc_fldss)
            <- fmap unzip $ mapM (new_tc overload_ok)
                                 (tyClGroupTyClDecls tycl_decls)
        ; traceRn "getLocalNonValBinders 1" (ppr tc_avails)
        ; envs <- extendGlobalRdrEnvRn tc_avails fixity_env
        ; setEnvs envs $ do {
            -- Bring these things into scope first
            -- See Note [Looking up family names in family instances]

          -- Process all family instances
          -- to bring new data constructors into scope
        ; (nti_availss, nti_fldss) <- mapAndUnzipM (new_assoc overload_ok)
                                                   inst_decls

          -- Finish off with value binders:
          --    foreign decls and pattern synonyms for an ordinary module
          --    type sigs in case of a hs-boot file only
        ; is_boot <- tcIsHsBootOrSig
        ; let val_bndrs | is_boot   = hs_boot_sig_bndrs
                        | otherwise = map lEmb for_hs_bndrs
        ; val_avails <- mapM new_simple val_bndrs

        ; let avails    = concat nti_availss ++ val_avails
              new_bndrs = availsToNameSetWithSelectors avails `unionNameSet`
                          availsToNameSetWithSelectors tc_avails
              flds      = concat nti_fldss ++ concat tc_fldss
        ; traceRn "getLocalNonValBinders 2" (ppr avails)
        ; (tcg_env, tcl_env) <- extendGlobalRdrEnvRn avails fixity_env

        -- Extend tcg_field_env with new fields (this used to be the
        -- work of extendRecordFieldEnv)
        ; let field_env = extendNameEnvList (tcg_field_env tcg_env) flds
              envs      = (tcg_env { tcg_field_env = field_env }, tcl_env)

        ; traceRn "getLocalNonValBinders 3" (vcat [ppr flds, ppr field_env])
        ; return (envs, new_bndrs) } }
  where
    ValBindsIn _val_binds val_sigs = binds

    for_hs_bndrs :: [Located RdrName]
    for_hs_bndrs = hsForeignDeclsBinders foreign_decls

    -- In a hs-boot file, the value binders come from the
    --  *signatures*, and there should be no foreign binders
    hs_boot_sig_bndrs = [ L decl_loc (unLoc n)
                        | L decl_loc (TypeSig ns _) <- val_sigs, n <- ns]

      -- the SrcSpan attached to the input should be the span of the
      -- declaration, not just the name
    new_simple :: LEmbellished RdrName -> RnM AvailInfo
    new_simple rdr_name = do{ nm <- newTopSrcBinder $ unLEmb rdr_name
                            ; return (avail nm) }

    new_tc :: Bool -> LTyClDecl RdrName
           -> RnM (AvailInfo, [(Name, [FieldLabel])])
    new_tc overload_ok tc_decl -- NOT for type/data instances
        = do { let (bndrs, flds) = hsLTyClDeclBinders tc_decl
             ; names@(main_name : sub_names)
                                     <- mapM (newTopSrcBinder . unLEmb) bndrs
             ; flds' <- mapM (newRecordSelector overload_ok sub_names) flds
             ; let fld_env = case unLoc tc_decl of
                     DataDecl { tcdDataDefn = d } -> mk_fld_env d names flds'
                     _                            -> []
             ; return (AvailTC main_name names flds', fld_env) }


    -- Calculate the mapping from constructor names to fields, which
    -- will go in tcg_field_env. It's convenient to do this here where
    -- we are working with a single datatype definition.
    mk_fld_env :: HsDataDefn RdrName -> [Name] -> [FieldLabel] -> [(Name, [FieldLabel])]
    mk_fld_env d names flds = concatMap find_con_flds (dd_cons d)
      where
        find_con_flds (L _ (ConDeclH98 { con_name    = L _ rdr
                                       , con_details = RecCon cdflds }))
            = [( find_con_name $ unEmb rdr
               , concatMap find_con_decl_flds (unLoc cdflds) )]
        find_con_flds (L _ (ConDeclGADT
                              { con_names = rdrs
                              , con_type = (HsIB { hsib_body = res_ty})}))
            = map (\ (L _ rdr) -> ( find_con_name $ unEmb rdr
                                  , concatMap find_con_decl_flds cdflds))
                  rdrs
            where
              (_tvs, _cxt, tau) = splitLHsSigmaTy res_ty
              cdflds = case tau of
                 L _ (HsFunTy
                      (L _ (HsAppsTy
                        [L _ (HsAppPrefix (L _ (HsRecTy flds)))])) _) -> flds
                 L _ (HsFunTy (L _ (HsRecTy flds)) _) -> flds
                 _                                    -> []
        find_con_flds _ = []

        find_con_name rdr
          = expectJust "getLocalNonValBinders/find_con_name" $
              find (\ n -> nameOccName n == rdrNameOcc rdr) names
        find_con_decl_flds (L _ x)
          = map find_con_decl_fld (cd_fld_names x)
        find_con_decl_fld  (L _ (FieldOcc (L _ rdr) _))
          = expectJust "getLocalNonValBinders/find_con_decl_fld" $
              find (\ fl -> flLabel fl == lbl) flds
          where lbl = occNameFS (rdrNameOcc $ unEmb rdr)

    new_assoc :: Bool -> LInstDecl RdrName
              -> RnM ([AvailInfo], [(Name, [FieldLabel])])
    new_assoc _ (L _ (TyFamInstD {})) = return ([], [])
      -- type instances don't bind new names

    new_assoc overload_ok (L _ (DataFamInstD d))
      = do { (avail, flds) <- new_di overload_ok Nothing d
           ; return ([avail], flds) }
    new_assoc overload_ok (L _ (ClsInstD (ClsInstDecl { cid_poly_ty = inst_ty
                                                      , cid_datafam_insts = adts })))
      | Just (L loc cls_rdr) <- getLHsInstDeclClass_maybe inst_ty
      = do { cls_nm <- setSrcSpan loc $ lookupGlobalOccRn cls_rdr
           ; (avails, fldss)
                    <- mapAndUnzipM (new_loc_di overload_ok (Just cls_nm)) adts
           ; return (avails, concat fldss) }
      | otherwise
      = return ([], [])    -- Do not crash on ill-formed instances
                           -- Eg   instance !Show Int   Trac #3811c

    new_di :: Bool -> Maybe Name -> DataFamInstDecl RdrName
                   -> RnM (AvailInfo, [(Name, [FieldLabel])])
    new_di overload_ok mb_cls ti_decl
        = do { main_name <- lookupFamInstName mb_cls (dfid_tycon ti_decl)
             ; let (bndrs, flds) = hsDataFamInstBinders ti_decl
             ; sub_names <- mapM (newTopSrcBinder . unLEmb) bndrs
             ; flds' <- mapM (newRecordSelector overload_ok sub_names) flds
             ; let avail    = AvailTC (unLoc main_name) sub_names flds'
                                  -- main_name is not bound here!
                   fld_env  = mk_fld_env (dfid_defn ti_decl) sub_names flds'
             ; return (avail, fld_env) }

    new_loc_di :: Bool -> Maybe Name -> LDataFamInstDecl RdrName
                   -> RnM (AvailInfo, [(Name, [FieldLabel])])
    new_loc_di overload_ok mb_cls (L _ d) = new_di overload_ok mb_cls d

newRecordSelector :: Bool -> [Name] -> LFieldOcc RdrName -> RnM FieldLabel
newRecordSelector _ [] _ = error "newRecordSelector: datatype has no constructors!"
newRecordSelector overload_ok (dc:_) (L loc (FieldOcc (L _ fld) _))
  = do { selName <- newTopSrcBinder $ L loc $ unEmb field
       ; return $ qualFieldLbl { flSelector = selName } }
  where
    fieldOccName = occNameFS $ rdrNameOcc $ unEmb fld
    qualFieldLbl = mkFieldLabelOccs fieldOccName (nameOccName dc) overload_ok
    field | isExact $ unEmb fld = fld
              -- use an Exact RdrName as is to preserve the bindings
              -- of an already renamer-resolved field and its use
              -- sites. This is needed to correctly support record
              -- selectors in Template Haskell. See Note [Binders in
              -- Template Haskell] in Convert.hs and Note [Looking up
              -- Exact RdrNames] in RnEnv.hs.
          | otherwise   = EName $ mkRdrUnqual (flSelector qualFieldLbl)

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
For import M( ies ), we take the mi_exports of M, and make
   imp_occ_env :: OccEnv (Name, AvailInfo, Maybe Name)
One entry for each Name that M exports; the AvailInfo is the
AvailInfo exported from M that exports that Name.

The situation is made more complicated by associated types. E.g.
   module M where
     class    C a    where { data T a }
     instance C Int  where { data T Int = T1 | T2 }
     instance C Bool where { data T Int = T3 }
Then M's export_avails are (recall the AvailTC invariant from Avails.hs)
  C(C,T), T(T,T1,T2,T3)
Notice that T appears *twice*, once as a child and once as a parent. From
this list we construt a raw list including
   T -> (T, T( T1, T2, T3 ), Nothing)
   T -> (C, C( C, T ),       Nothing)
and we combine these (in function 'combine' in 'imp_occ_env' in
'filterImports') to get
   T  -> (T,  T(T,T1,T2,T3), Just C)

So the overall imp_occ_env is
   C  -> (C,  C(C,T),        Nothing)
   T  -> (T,  T(T,T1,T2,T3), Just C)
   T1 -> (T1, T(T,T1,T2,T3), Nothing)   -- similarly T2,T3

If we say
   import M( T(T1,T2) )
then we get *two* Avails:  C(T), T(T1,T2)

Note that the imp_occ_env will have entries for data constructors too,
although we never look up data constructors.
-}

filterImports
    :: ModIface
    -> ImpDeclSpec                     -- The span for the entire import decl
    -> Maybe (Bool, Located [LIE RdrName])    -- Import spec; True => hiding
    -> RnM (Maybe (Bool, Located [LIE Name]), -- Import spec w/ Names
            [GlobalRdrElt])                   -- Same again, but in GRE form
filterImports iface decl_spec Nothing
  = return (Nothing, gresFromAvails (Just imp_spec) (mi_exports iface))
  where
    imp_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }


filterImports iface decl_spec (Just (want_hiding, L l import_items))
  = do  -- check for errors, convert RdrNames to Names
        items1 <- mapM lookup_lie import_items

        let items2 :: [(LIE Name, AvailInfo)]
            items2 = concat items1
                -- NB the AvailInfo may have duplicates, and several items
                --    for the same parent; e.g N(x) and N(y)

            names  = availsToNameSet (map snd items2)
            keep n = not (n `elemNameSet` names)
            pruned_avails = filterAvails keep all_avails
            hiding_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }

            gres | want_hiding = gresFromAvails (Just hiding_spec) pruned_avails
                 | otherwise   = concatMap (gresFromIE decl_spec) items2

        return (Just (want_hiding, L l (map fst items2)), gres)
  where
    all_avails = mi_exports iface

        -- See Note [Dealing with imports]
    imp_occ_env :: OccEnv (Name,        -- the name
                           AvailInfo,   -- the export item providing the name
                           Maybe Name)  -- the parent of associated types
    imp_occ_env = mkOccEnv_C combine [ (nameOccName n, (n, a, Nothing))
                                     | a <- all_avails, n <- availNames a]
      where
        -- See Note [Dealing with imports]
        -- 'combine' is only called for associated data types which appear
        -- twice in the all_avails. In the example, we combine
        --    T(T,T1,T2,T3) and C(C,T)  to give   (T, T(T,T1,T2,T3), Just C)
        -- NB: the AvailTC can have fields as well as data constructors (Trac #12127)
        combine (name1, a1@(AvailTC p1 _ _), mp1)
                (name2, a2@(AvailTC p2 _ _), mp2)
          = ASSERT2( name1 == name2 && isNothing mp1 && isNothing mp2
                   , ppr name1 <+> ppr name2 <+> ppr mp1 <+> ppr mp2 )
            if p1 == name1 then (name1, a1, Just p2)
                           else (name1, a2, Just p1)
        combine x y = pprPanic "filterImports/combine" (ppr x $$ ppr y)

    lookup_name :: RdrName -> IELookupM (Name, AvailInfo, Maybe Name)
    lookup_name rdr | isQual rdr              = failLookupWith (QualImportError rdr)
                    | Just succ <- mb_success = return succ
                    | otherwise               = failLookupWith BadImport
      where
        mb_success = lookupOccEnv imp_occ_env (rdrNameOcc rdr)

    lookup_lie :: LIE RdrName -> TcRn [(LIE Name, AvailInfo)]
    lookup_lie (L loc ieRdr)
        = do (stuff, warns) <- setSrcSpan loc $
                               liftM (fromMaybe ([],[])) $
                               run_lookup (lookup_ie ieRdr)
             mapM_ emit_warning warns
             return [ (L loc ie, avail) | (ie,avail) <- stuff ]
        where
            -- Warn when importing T(..) if T was exported abstractly
            emit_warning (DodgyImport n) = whenWOptM Opt_WarnDodgyImports $
              addWarn (Reason Opt_WarnDodgyImports) (dodgyImportWarn n)
            emit_warning MissingImportList = whenWOptM Opt_WarnMissingImportList $
              addWarn (Reason Opt_WarnMissingImportList) (missingImportListItem ieRdr)
            emit_warning BadImportW = whenWOptM Opt_WarnDodgyImports $
              addWarn (Reason Opt_WarnDodgyImports) (lookup_err_msg BadImport)

            run_lookup :: IELookupM a -> TcRn (Maybe a)
            run_lookup m = case m of
              Failed err -> addErr (lookup_err_msg err) >> return Nothing
              Succeeded a -> return (Just a)

            lookup_err_msg err = case err of
              BadImport -> badImportItemErr iface decl_spec ieRdr all_avails
              IllegalImport -> illegalImportItemErr
              QualImportError rdr -> qualImportItemErr rdr

        -- For each import item, we convert its RdrNames to Names,
        -- and at the same time construct an AvailInfo corresponding
        -- to what is actually imported by this item.
        -- Returns Nothing on error.
        -- We return a list here, because in the case of an import
        -- item like C, if we are hiding, then C refers to *both* a
        -- type/class and a data constructor.  Moreover, when we import
        -- data constructors of an associated family, we need separate
        -- AvailInfos for the data constructors and the family (as they have
        -- different parents).  See Note [Dealing with imports]
    lookup_ie :: IE RdrName -> IELookupM ([(IE Name, AvailInfo)], [IELookupWarning])
    lookup_ie ie = handle_bad_import $ do
      case ie of
        IEVar (L l n) -> do
            (name, avail, _) <- lookup_name $ ieWrappedName n
            return ([(IEVar (L l (replaceWrappedName n name)),
                                                  trimAvail avail name)], [])

        IEThingAll (L l tc) -> do
            (name, avail, mb_parent) <- lookup_name $ ieWrappedName tc
            let warns = case avail of
                          Avail {}                     -- e.g. f(..)
                            -> [DodgyImport $ ieWrappedName tc]

                          AvailTC _ subs fs
                            | null (drop 1 subs) && null fs -- e.g. T(..) where T is a synonym
                            -> [DodgyImport $ ieWrappedName tc]

                            | not (is_qual decl_spec)  -- e.g. import M( T(..) )
                            -> [MissingImportList]

                            | otherwise
                            -> []

                renamed_ie = IEThingAll (L l (replaceWrappedName tc name))
                sub_avails = case avail of
                               Avail {}              -> []
                               AvailTC name2 subs fs -> [(renamed_ie, AvailTC name2 (subs \\ [name]) fs)]
            case mb_parent of
              Nothing     -> return ([(renamed_ie, avail)], warns)
                             -- non-associated ty/cls
              Just parent -> return ((renamed_ie, AvailTC parent [name] []) : sub_avails, warns)
                             -- associated type

        IEThingAbs (L l tc')
            | want_hiding   -- hiding ( C )
                       -- Here the 'C' can be a data constructor
                       --  *or* a type/class, or even both
            -> let tc = ieWrappedName tc'
                   tc_name = lookup_name tc
                   dc_name = lookup_name (setRdrNameSpace tc srcDataName)
               in
               case catIELookupM [ tc_name, dc_name ] of
                 []    -> failLookupWith BadImport
                 names -> return ([mkIEThingAbs tc' l name | name <- names], [])
            | otherwise
            -> do nameAvail <- lookup_name (ieWrappedName tc')
                  return ([mkIEThingAbs tc' l nameAvail]
                         , [])

        IEThingWith (L l rdr_tc) wc rdr_ns' rdr_fs ->
          ASSERT2(null rdr_fs, ppr rdr_fs) do
           (name, AvailTC _ ns subflds, mb_parent)
                                         <- lookup_name (ieWrappedName rdr_tc)

           -- Look up the children in the sub-names of the parent
           let subnames = case ns of   -- The tc is first in ns,
                            [] -> []   -- if it is there at all
                                       -- See the AvailTC Invariant in Avail.hs
                            (n1:ns1) | n1 == name -> ns1
                                     | otherwise  -> ns
               rdr_ns = map ieLWrappedName rdr_ns'
           case lookupChildren (map Left subnames ++ map Right subflds) rdr_ns of
             Nothing                      -> failLookupWith BadImport
             Just (childnames, childflds) ->
               case mb_parent of
                 -- non-associated ty/cls
                 Nothing
                   -> return ([(IEThingWith (L l name') wc childnames'
                                                           childflds,
                               AvailTC name (name:map unLoc childnames) (map unLoc childflds))],
                              [])
                   where name' = replaceWrappedName rdr_tc name
                         childnames' = map to_ie_post_rn childnames
                         -- childnames' = postrn_ies childnames
                 -- associated ty
                 Just parent
                   -> return ([(IEThingWith (L l name') wc childnames'
                                                           childflds,
                                AvailTC name (map unLoc childnames) (map unLoc childflds)),
                               (IEThingWith (L l name') wc childnames'
                                                           childflds,
                                AvailTC parent [name] [])],
                              [])
                   where name' = replaceWrappedName rdr_tc name
                         childnames' = map to_ie_post_rn childnames

        _other -> failLookupWith IllegalImport
        -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed
        -- all errors.

      where
        mkIEThingAbs tc l (n, av, Nothing    )
          = (IEThingAbs (L l (replaceWrappedName tc n)), trimAvail av n)
        mkIEThingAbs tc l (n, _,  Just parent)
          = (IEThingAbs (L l (replaceWrappedName tc n)), AvailTC parent [n] [])

        handle_bad_import m = catchIELookup m $ \err -> case err of
          BadImport | want_hiding -> return ([], [BadImportW])
          _                       -> failLookupWith err

type IELookupM = MaybeErr IELookupError

data IELookupWarning
  = BadImportW
  | MissingImportList
  | DodgyImport RdrName
  -- NB. use the RdrName for reporting a "dodgy" import

data IELookupError
  = QualImportError RdrName
  | BadImport
  | IllegalImport

failLookupWith :: IELookupError -> IELookupM a
failLookupWith err = Failed err

catchIELookup :: IELookupM a -> (IELookupError -> IELookupM a) -> IELookupM a
catchIELookup m h = case m of
  Succeeded r -> return r
  Failed err  -> h err

catIELookupM :: [IELookupM a] -> [a]
catIELookupM ms = [ a | Succeeded a <- ms ]

{-
************************************************************************
*                                                                      *
\subsection{Import/Export Utils}
*                                                                      *
************************************************************************
-}

-- | Given an import\/export spec, construct the appropriate 'GlobalRdrElt's.
gresFromIE :: ImpDeclSpec -> (LIE Name, AvailInfo) -> [GlobalRdrElt]
gresFromIE decl_spec (L loc ie, avail)
  = gresFromAvail prov_fn avail
  where
    is_explicit = case ie of
                    IEThingAll (L _ name) -> \n -> n == ieWrappedName name
                    _                     -> \_ -> True
    prov_fn name
      = Just (ImpSpec { is_decl = decl_spec, is_item = item_spec })
      where
        item_spec = ImpSome { is_explicit = is_explicit name, is_iloc = loc }


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
    add gre env = case gre_par gre of
        FldParent p _  -> extendNameEnv_Acc (:) singleton env p gre
        ParentIs  p    -> extendNameEnv_Acc (:) singleton env p gre
        NoParent       -> env

findChildren :: NameEnv [a] -> Name -> [a]
findChildren env n = lookupNameEnv env n `orElse` []

lookupChildren :: [Either Name FieldLabel] -> [Located RdrName]
               -> Maybe ([Located Name], [Located FieldLabel])
-- (lookupChildren all_kids rdr_items) maps each rdr_item to its
-- corresponding Name all_kids, if the former exists
-- The matching is done by FastString, not OccName, so that
--    Cls( meth, AssocTy )
-- will correctly find AssocTy among the all_kids of Cls, even though
-- the RdrName for AssocTy may have a (bogus) DataName namespace
-- (Really the rdr_items should be FastStrings in the first place.)
lookupChildren all_kids rdr_items
  = do xs <- mapM doOne rdr_items
       return (fmap concat (partitionEithers xs))
  where
    doOne (L l r) = case (lookupFsEnv kid_env . occNameFS . rdrNameOcc) r of
      Just [Left n]            -> Just (Left (L l n))
      Just rs | all isRight rs -> Just (Right (map (L l) (rights rs)))
      _                        -> Nothing

    -- See Note [Children for duplicate record fields]
    kid_env = extendFsEnvList_C (++) emptyFsEnv
                      [(either (occNameFS . nameOccName) flLabel x, [x]) | x <- all_kids]



-------------------------------

{-
*********************************************************
*                                                       *
\subsection{Unused names}
*                                                       *
*********************************************************
-}

reportUnusedNames :: Maybe (Located [LIE RdrName])  -- Export list
                  -> TcGblEnv -> RnM ()
reportUnusedNames _export_decls gbl_env
  = do  { traceRn "RUN" (ppr (tcg_dus gbl_env))
        ; warnUnusedImportDecls gbl_env
        ; warnUnusedTopBinds unused_locals
        ; warnMissingSignatures gbl_env }
  where
    used_names :: NameSet
    used_names = findUses (tcg_dus gbl_env) emptyNameSet
    -- NB: currently, if f x = g, we only treat 'g' as used if 'f' is used
    -- Hence findUses

    -- Collect the defined names from the in-scope environment
    defined_names :: [GlobalRdrElt]
    defined_names = globalRdrEnvElts (tcg_rdr_env gbl_env)

    -- Note that defined_and_used, defined_but_not_used
    -- are both [GRE]; that's why we need defined_and_used
    -- rather than just used_names
    _defined_and_used, defined_but_not_used :: [GlobalRdrElt]
    (_defined_and_used, defined_but_not_used)
        = partition (gre_is_used used_names) defined_names

    kids_env = mkChildEnv defined_names
    -- This is done in mkExports too; duplicated work

    gre_is_used :: NameSet -> GlobalRdrElt -> Bool
    gre_is_used used_names (GRE {gre_name = name})
        = name `elemNameSet` used_names
          || any (\ gre -> gre_name gre `elemNameSet` used_names) (findChildren kids_env name)
                -- A use of C implies a use of T,
                -- if C was brought into scope by T(..) or T(C)

    -- Filter out the ones that are
    --  (a) defined in this module, and
    --  (b) not defined by a 'deriving' clause
    -- The latter have an Internal Name, so we can filter them out easily
    unused_locals :: [GlobalRdrElt]
    unused_locals = filter is_unused_local defined_but_not_used
    is_unused_local :: GlobalRdrElt -> Bool
    is_unused_local gre = isLocalGRE gre && isExternalName (gre_name gre)

{-
*********************************************************
*                                                       *
\subsection{Unused imports}
*                                                       *
*********************************************************

This code finds which import declarations are unused.  The
specification and implementation notes are here:
  http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/UnusedImports
-}

type ImportDeclUsage
   = ( LImportDecl Name   -- The import declaration
     , [AvailInfo]        -- What *is* used (normalised)
     , [Name] )           -- What is imported but *not* used

warnUnusedImportDecls :: TcGblEnv -> RnM ()
warnUnusedImportDecls gbl_env
  = do { uses <- readMutVar (tcg_used_gres gbl_env)
       ; let user_imports = filterOut (ideclImplicit . unLoc) (tcg_rn_imports gbl_env)
                            -- This whole function deals only with *user* imports
                            -- both for warning about unnecessary ones, and for
                            -- deciding the minimal ones
             rdr_env = tcg_rdr_env gbl_env
             fld_env = mkFieldEnv rdr_env

       ; let usage :: [ImportDeclUsage]
             usage = findImportUsage user_imports uses

       ; traceRn "warnUnusedImportDecls" $
                       (vcat [ text "Uses:" <+> ppr uses
                       , text "Import usage" <+> ppr usage])
       ; whenWOptM Opt_WarnUnusedImports $
         mapM_ (warnUnusedImport Opt_WarnUnusedImports fld_env) usage

       ; whenGOptM Opt_D_dump_minimal_imports $
         printMinimalImports usage }

-- | Warn the user about top level binders that lack type signatures.
-- Called /after/ type inference, so that we can report the
-- inferred type of the function
warnMissingSignatures :: TcGblEnv -> RnM ()
warnMissingSignatures gbl_env
  = do { let exports = availsToNameSet (tcg_exports gbl_env)
             sig_ns  = tcg_sigs gbl_env
               -- We use sig_ns to exclude top-level bindings that are generated by GHC
             binds    = collectHsBindsBinders $ tcg_binds gbl_env
             pat_syns = tcg_patsyns gbl_env

         -- Warn about missing signatures
         -- Do this only when we we have a type to offer
       ; warn_missing_sigs  <- woptM Opt_WarnMissingSignatures
       ; warn_only_exported <- woptM Opt_WarnMissingExportedSignatures
       ; warn_pat_syns      <- woptM Opt_WarnMissingPatternSynonymSignatures

       ; let add_sig_warns
               | warn_only_exported = add_warns Opt_WarnMissingExportedSignatures
               | warn_missing_sigs  = add_warns Opt_WarnMissingSignatures
               | warn_pat_syns      = add_warns Opt_WarnMissingPatternSynonymSignatures
               | otherwise          = return ()

             add_warns flag
                = when warn_pat_syns
                       (mapM_ add_pat_syn_warn pat_syns) >>
                  when (warn_missing_sigs || warn_only_exported)
                       (mapM_ add_bind_warn binds)
                where
                  add_pat_syn_warn p
                    = add_warn name $
                      hang (text "Pattern synonym with no type signature:")
                         2 (text "pattern" <+> pprPrefixName name <+> dcolon <+> pp_ty)
                    where
                      name  = patSynName p
                      pp_ty = pprPatSynType p

                  add_bind_warn id
                    = do { env <- tcInitTidyEnv     -- Why not use emptyTidyEnv?
                         ; let name    = idName id
                               (_, ty) = tidyOpenType env (idType id)
                               ty_msg  = pprSigmaType ty
                         ; add_warn name $
                           hang (text "Top-level binding with no type signature:")
                              2 (pprPrefixName name <+> dcolon <+> ty_msg) }

                  add_warn name msg
                    = when (name `elemNameSet` sig_ns && export_check name)
                           (addWarnAt (Reason flag) (getSrcSpan name) msg)

                  export_check name
                    = not warn_only_exported || name `elemNameSet` exports

       ; add_sig_warns }

{-
Note [The ImportMap]
~~~~~~~~~~~~~~~~~~~~
The ImportMap is a short-lived intermediate data struture records, for
each import declaration, what stuff brought into scope by that
declaration is actually used in the module.

The SrcLoc is the location of the END of a particular 'import'
declaration.  Why *END*?  Because we don't want to get confused
by the implicit Prelude import. Consider (Trac #7476) the module
    import Foo( foo )
    main = print foo
There is an implicit 'import Prelude(print)', and it gets a SrcSpan
of line 1:1 (just the point, not a span). If we use the *START* of
the SrcSpan to identify the import decl, we'll confuse the implicit
import Prelude with the explicit 'import Foo'.  So we use the END.
It's just a cheap hack; we could equally well use the Span too.

The AvailInfos are the things imported from that decl (just a list,
not normalised).
-}

type ImportMap = Map SrcLoc [AvailInfo]  -- See [The ImportMap]

findImportUsage :: [LImportDecl Name]
                -> [GlobalRdrElt]
                -> [ImportDeclUsage]

findImportUsage imports used_gres
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage
      = foldr extendImportMap Map.empty used_gres

    unused_decl decl@(L loc (ImportDecl { ideclHiding = imps }))
      = (decl, nubAvails used_avails, nameSetElemsStable unused_imps)
      where
        used_avails = Map.lookup (srcSpanEnd loc) import_usage `orElse` []
                      -- srcSpanEnd: see Note [The ImportMap]
        used_names   = availsToNameSetWithSelectors used_avails
        used_parents = mkNameSet [n | AvailTC n _ _ <- used_avails]

        unused_imps   -- Not trivial; see eg Trac #7454
          = case imps of
              Just (False, L _ imp_ies) ->
                                 foldr (add_unused . unLoc) emptyNameSet imp_ies
              _other -> emptyNameSet -- No explicit import list => no unused-name list

        add_unused :: IE Name -> NameSet -> NameSet
        add_unused (IEVar (L _ n))      acc
                                       = add_unused_name (ieWrappedName n) acc
        add_unused (IEThingAbs (L _ n)) acc
                                       = add_unused_name (ieWrappedName n) acc
        add_unused (IEThingAll (L _ n)) acc
                                       = add_unused_all  (ieWrappedName n) acc
        add_unused (IEThingWith (L _ p) wc ns fs) acc =
          add_wc_all (add_unused_with (ieWrappedName p) xs acc)
          where xs = map (ieWrappedName . unLoc) ns
                          ++ map (flSelector . unLoc) fs
                add_wc_all = case wc of
                            NoIEWildcard -> id
                            IEWildcard _ -> add_unused_all (ieWrappedName p)
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

extendImportMap :: GlobalRdrElt -> ImportMap -> ImportMap
-- For each of a list of used GREs, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
extendImportMap gre imp_map
   = add_imp gre (bestImport (gre_imp gre)) imp_map
  where
    add_imp :: GlobalRdrElt -> ImportSpec -> ImportMap -> ImportMap
    add_imp gre (ImpSpec { is_decl = imp_decl_spec }) imp_map
      = Map.insertWith add decl_loc [avail] imp_map
      where
        add _ avails = avail : avails -- add is really just a specialised (++)
        decl_loc = srcSpanEnd (is_dloc imp_decl_spec)
                   -- For srcSpanEnd see Note [The ImportMap]
        avail    = availFromGRE gre

warnUnusedImport :: WarningFlag -> NameEnv (FieldLabelString, Name)
                 -> ImportDeclUsage -> RnM ()
warnUnusedImport flag fld_env (L loc decl, used, unused)
  | Just (False,L _ []) <- ideclHiding decl
                = return ()            -- Do not warn for 'import M()'

  | Just (True, L _ hides) <- ideclHiding decl
  , not (null hides)
  , pRELUDE_NAME == unLoc (ideclName decl)
                = return ()            -- Note [Do not warn about Prelude hiding]
  | null used   = addWarnAt (Reason flag) loc msg1 -- Nothing used; drop entire decl
  | null unused = return ()            -- Everything imported is used; nop
  | otherwise   = addWarnAt (Reason flag) loc msg2 -- Some imports are unused
  where
    msg1 = vcat [pp_herald <+> quotes pp_mod <+> pp_not_used,
                 nest 2 (text "except perhaps to import instances from"
                                   <+> quotes pp_mod),
                 text "To import instances alone, use:"
                                   <+> text "import" <+> pp_mod <> parens Outputable.empty ]
    msg2 = sep [pp_herald <+> quotes sort_unused,
                    text "from module" <+> quotes pp_mod <+> pp_not_used]
    pp_herald  = text "The" <+> pp_qual <+> text "import of"
    pp_qual
      | ideclQualified decl = text "qualified"
      | otherwise           = Outputable.empty
    pp_mod      = ppr (unLoc (ideclName decl))
    pp_not_used = text "is redundant"

    ppr_possible_field n = case lookupNameEnv fld_env n of
                               Just (fld, p) -> ppr p <> parens (ppr fld)
                               Nothing  -> ppr n

    -- Print unused names in a deterministic (lexicographic) order
    sort_unused = pprWithCommas ppr_possible_field $
                    sortBy (comparing nameOccName) unused

{-
Note [Do not warn about Prelude hiding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not warn about
   import Prelude hiding( x, y )
because even if nothing else from Prelude is used, it may be essential to hide
x,y to avoid name-shadowing warnings.  Example (Trac #9061)
   import Prelude hiding( log )
   f x = log where log = ()



Note [Printing minimal imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To print the minimal imports we walk over the user-supplied import
decls, and simply trim their import lists.  NB that

  * We do *not* change the 'qualified' or 'as' parts!

  * We do not disard a decl altogether; we might need instances
    from it.  Instead we just trim to an empty import list
-}

printMinimalImports :: [ImportDeclUsage] -> RnM ()
-- See Note [Printing minimal imports]
printMinimalImports imports_w_usage
  = do { imports' <- mapM mk_minimal imports_w_usage
       ; this_mod <- getModule
       ; dflags   <- getDynFlags
       ; liftIO $
         do { h <- openFile (mkFilename dflags this_mod) WriteMode
            ; printForUser dflags h neverQualify (vcat (map ppr imports')) }
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
        basefn = moduleNameString (moduleName this_mod) ++ ".imports"

    mk_minimal (L l decl, used, unused)
      | null unused
      , Just (False, _) <- ideclHiding decl
      = return (L l decl)
      | otherwise
      = do { let ImportDecl { ideclName    = L _ mod_name
                            , ideclSource  = is_boot
                            , ideclPkgQual = mb_pkg } = decl
           ; iface <- loadSrcInterface doc mod_name is_boot (fmap sl_fs mb_pkg)
           ; let lies = map (L l) (concatMap (to_ie iface) used)
           ; return (L l (decl { ideclHiding = Just (False, L l lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: ModIface -> AvailInfo -> [IE Name]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie _ (Avail n)
       = [IEVar (to_ie_post_rn $ noLoc n)]
    to_ie _ (AvailTC n [m] [])
       | n==m = [IEThingAbs (to_ie_post_rn $ noLoc n)]
    to_ie iface (AvailTC n ns fs)
      = case [(xs,gs) |  AvailTC x xs gs <- mi_exports iface
                 , x == n
                 , x `elem` xs    -- Note [Partial export]
                 ] of
           [xs] | all_used xs -> [IEThingAll (to_ie_post_rn $ noLoc n)]
                | otherwise   ->
                   [IEThingWith (to_ie_post_rn $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]
                                          -- Note [Overloaded field import]
           _other | all_non_overloaded fs
                              -> map (IEVar . to_ie_post_rn_var . noLoc) $ ns
                                 ++ map flSelector fs
                  | otherwise ->
                      [IEThingWith (to_ie_post_rn $ noLoc n) NoIEWildcard
                                (map (to_ie_post_rn . noLoc) (filter (/= n) ns))
                                (map noLoc fs)]
        where

          fld_lbls = map flLabel fs

          all_used (avail_occs, avail_flds)
              = all (`elem` ns) avail_occs
                    && all (`elem` fld_lbls) (map flLabel avail_flds)

          all_non_overloaded = all (not . flIsOverloaded)

to_ie_post_rn_var :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn_var (L l n)
  | isDataOcc $ occName n = L l (IEPattern (L l n))
  | otherwise             = L l (IEName    (L l n))


to_ie_post_rn :: (HasOccName name) => Located name -> LIEWrappedName name
to_ie_post_rn (L l n)
  | isTcOcc occ && isSymOcc occ = L l (IEType (L l n))
  | otherwise                   = L l (IEName (L l n))
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
the (x `elem` xs) test when deciding what to generate.


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


************************************************************************
*                                                                      *
\subsection{Errors}
*                                                                      *
************************************************************************
-}

qualImportItemErr :: RdrName -> SDoc
qualImportItemErr rdr
  = hang (text "Illegal qualified name in import item:")
       2 (ppr rdr)

badImportItemErrStd :: ModIface -> ImpDeclSpec -> IE RdrName -> SDoc
badImportItemErrStd iface decl_spec ie
  = sep [text "Module", quotes (ppr (is_mod decl_spec)), source_import,
         text "does not export", quotes (ppr ie)]
  where
    source_import | mi_boot iface = text "(hi-boot interface)"
                  | otherwise     = Outputable.empty

badImportItemErrDataCon :: OccName -> ModIface -> ImpDeclSpec -> IE RdrName -> SDoc
badImportItemErrDataCon dataType_occ iface decl_spec ie
  = vcat [ text "In module"
             <+> quotes (ppr (is_mod decl_spec))
             <+> source_import <> colon
         , nest 2 $ quotes datacon
             <+> text "is a data constructor of"
             <+> quotes dataType
         , text "To import it use"
         , nest 2 $ text "import"
             <+> ppr (is_mod decl_spec)
             <> parens_sp (dataType <> parens_sp datacon)
         , text "or"
         , nest 2 $ text "import"
             <+> ppr (is_mod decl_spec)
             <> parens_sp (dataType <> text "(..)")
         ]
  where
    datacon_occ = rdrNameOcc $ ieName ie
    datacon = parenSymOcc datacon_occ (ppr datacon_occ)
    dataType = parenSymOcc dataType_occ (ppr dataType_occ)
    source_import | mi_boot iface = text "(hi-boot interface)"
                  | otherwise     = Outputable.empty
    parens_sp d = parens (space <> d <> space)  -- T( f,g )

badImportItemErr :: ModIface -> ImpDeclSpec -> IE RdrName -> [AvailInfo] -> SDoc
badImportItemErr iface decl_spec ie avails
  = case find checkIfDataCon avails of
      Just con -> badImportItemErrDataCon (availOccName con) iface decl_spec ie
      Nothing  -> badImportItemErrStd iface decl_spec ie
  where
    checkIfDataCon (AvailTC _ ns _) =
      case find (\n -> importedFS == nameOccNameFS n) ns of
        Just n  -> isDataConName n
        Nothing -> False
    checkIfDataCon _ = False
    availOccName = nameOccName . availName
    nameOccNameFS = occNameFS . nameOccName
    importedFS = occNameFS . rdrNameOcc $ ieName ie

illegalImportItemErr :: SDoc
illegalImportItemErr = text "Illegal import item"

dodgyImportWarn :: RdrName -> SDoc
dodgyImportWarn item = dodgyMsg (text "import") item

dodgyMsg :: (OutputableBndr n, HasOccName n) => SDoc -> n -> SDoc
dodgyMsg kind tc
  = sep [ text "The" <+> kind <+> ptext (sLit "item")
                     <+> quotes (ppr (IEThingAll (noLoc (IEName $ noLoc tc))))
                <+> text "suggests that",
          quotes (ppr tc) <+> text "has (in-scope) constructors or class methods,",
          text "but it has none" ]


addDupDeclErr :: [GlobalRdrElt] -> TcRn ()
addDupDeclErr [] = panic "addDupDeclErr: empty list"
addDupDeclErr gres@(gre : _)
  = addErrAt (getSrcSpan (last sorted_names)) $
    -- Report the error at the later location
    vcat [text "Multiple declarations of" <+>
             quotes (ppr (nameOccName name)),
             -- NB. print the OccName, not the Name, because the
             -- latter might not be in scope in the RdrEnv and so will
             -- be printed qualified.
          text "Declared at:" <+>
                   vcat (map (ppr . nameSrcLoc) sorted_names)]
  where
    name = gre_name gre
    sorted_names = sortWith nameSrcLoc (map gre_name gres)



missingImportListWarn :: ModuleName -> SDoc
missingImportListWarn mod
  = text "The module" <+> quotes (ppr mod) <+> ptext (sLit "does not have an explicit import list")

missingImportListItem :: IE RdrName -> SDoc
missingImportListItem ie
  = text "The import item" <+> quotes (ppr ie) <+> ptext (sLit "does not have an explicit import list")

moduleWarn :: ModuleName -> WarningTxt -> SDoc
moduleWarn mod (WarningTxt _ txt)
  = sep [ text "Module" <+> quotes (ppr mod) <> ptext (sLit ":"),
          nest 2 (vcat (map (ppr . sl_fs . unLoc) txt)) ]
moduleWarn mod (DeprecatedTxt _ txt)
  = sep [ text "Module" <+> quotes (ppr mod)
                                <+> text "is deprecated:",
          nest 2 (vcat (map (ppr . sl_fs . unLoc) txt)) ]

packageImportErr :: SDoc
packageImportErr
  = text "Package-qualified imports are not enabled; use PackageImports"

-- This data decl will parse OK
--      data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--      data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName :: Embellished RdrName -> TcRn ()
checkConName name
  = checkErr (isRdrDataCon $ unEmb name) (badDataCon $ unEmb name)

badDataCon :: RdrName -> SDoc
badDataCon name
   = hsep [text "Illegal data constructor name", quotes (ppr name)]
