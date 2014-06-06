%
% (c) The University of Glasgow 2006-2008
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
{-# LANGUAGE CPP, NondecreasingIndentation #-}

-- | Module for constructing @ModIface@ values (interface files),
-- writing them to disk and comparing two versions to see if
-- recompilation is required.
module MkIface (
        mkUsedNames,
        mkDependencies,
        mkIface,        -- Build a ModIface from a ModGuts,
                        -- including computing version information

        mkIfaceTc,

        writeIfaceFile, -- Write the interface file

        checkOldIface,  -- See if recompilation is required, by
                        -- comparing version information
        RecompileRequired(..), recompileRequired,

        tyThingToIfaceDecl -- Converting things to their Iface equivalents
 ) where
\end{code}

  -----------------------------------------------
          Recompilation checking
  -----------------------------------------------

A complete description of how recompilation checking works can be
found in the wiki commentary:

 http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance

Please read the above page for a top-down description of how this all
works.  Notes below cover specific issues related to the implementation.

Basic idea:

  * In the mi_usages information in an interface, we record the
    fingerprint of each free variable of the module

  * In mkIface, we compute the fingerprint of each exported thing A.f.
    For each external thing that A.f refers to, we include the fingerprint
    of the external reference when computing the fingerprint of A.f.  So
    if anything that A.f depends on changes, then A.f's fingerprint will
    change.
    Also record any dependent files added with
      * addDependentFile
      * #include
      * -optP-include

  * In checkOldIface we compare the mi_usages for the module with
    the actual fingerprint for all each thing recorded in mi_usages

\begin{code}
#include "HsVersions.h"

import IfaceSyn
import LoadIface
import FlagChecker

import Id
import IdInfo
import Demand
import Coercion( tidyCo )
import Annotations
import CoreSyn
import CoreFVs
import Class
import Kind
import TyCon
import CoAxiom
import ConLike
import DataCon
import PatSyn
import Type
import TcType
import TysPrim ( alphaTyVars )
import InstEnv
import FamInstEnv
import TcRnMonad
import HsSyn
import HscTypes
import Finder
import DynFlags
import VarEnv
import VarSet
import Var
import Name
import Avail
import RdrName
import NameEnv
import NameSet
import Module
import BinIface
import ErrUtils
import Digraph
import SrcLoc
import Outputable
import BasicTypes       hiding ( SuccessFlag(..) )
import UniqFM
import Unique
import Util             hiding ( eqListBy )
import FastString
import Maybes
import ListSetOps
import Binary
import Fingerprint
import Bag
import Exception

import Control.Monad
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.IORef
import System.Directory
import System.FilePath
\end{code}



%************************************************************************
%*                                                                      *
\subsection{Completing an interface}
%*                                                                      *
%************************************************************************

\begin{code}
mkIface :: HscEnv
        -> Maybe Fingerprint    -- The old fingerprint, if we have it
        -> ModDetails           -- The trimmed, tidied interface
        -> ModGuts              -- Usages, deprecations, etc
        -> IO (Messages,
               Maybe (ModIface, -- The new one
                      Bool))    -- True <=> there was an old Iface, and the
                                --          new one is identical, so no need
                                --          to write it

mkIface hsc_env maybe_old_fingerprint mod_details
         ModGuts{     mg_module       = this_mod,
                      mg_boot         = is_boot,
                      mg_used_names   = used_names,
                      mg_used_th      = used_th,
                      mg_deps         = deps,
                      mg_dir_imps     = dir_imp_mods,
                      mg_rdr_env      = rdr_env,
                      mg_fix_env      = fix_env,
                      mg_warns        = warns,
                      mg_hpc_info     = hpc_info,
                      mg_safe_haskell = safe_mode,
                      mg_trust_pkg    = self_trust,
                      mg_dependent_files = dependent_files
                    }
        = mkIface_ hsc_env maybe_old_fingerprint
                   this_mod is_boot used_names used_th deps rdr_env fix_env
                   warns hpc_info dir_imp_mods self_trust dependent_files
                   safe_mode mod_details

-- | make an interface from the results of typechecking only.  Useful
-- for non-optimising compilation, or where we aren't generating any
-- object code at all ('HscNothing').
mkIfaceTc :: HscEnv
          -> Maybe Fingerprint  -- The old fingerprint, if we have it
          -> SafeHaskellMode    -- The safe haskell mode
          -> ModDetails         -- gotten from mkBootModDetails, probably
          -> TcGblEnv           -- Usages, deprecations, etc
          -> IO (Messages, Maybe (ModIface, Bool))
mkIfaceTc hsc_env maybe_old_fingerprint safe_mode mod_details
  tc_result@TcGblEnv{ tcg_mod = this_mod,
                      tcg_src = hsc_src,
                      tcg_imports = imports,
                      tcg_rdr_env = rdr_env,
                      tcg_fix_env = fix_env,
                      tcg_warns = warns,
                      tcg_hpc = other_hpc_info,
                      tcg_th_splice_used = tc_splice_used,
                      tcg_dependent_files = dependent_files
                    }
  = do
          let used_names = mkUsedNames tc_result
          deps <- mkDependencies tc_result
          let hpc_info = emptyHpcInfo other_hpc_info
          used_th <- readIORef tc_splice_used
          dep_files <- (readIORef dependent_files)
          mkIface_ hsc_env maybe_old_fingerprint
                   this_mod (isHsBoot hsc_src) used_names used_th deps rdr_env
                   fix_env warns hpc_info (imp_mods imports)
                   (imp_trust_own_pkg imports) dep_files safe_mode mod_details


mkUsedNames :: TcGblEnv -> NameSet
mkUsedNames TcGblEnv{ tcg_dus = dus } = allUses dus

-- | Extract information from the rename and typecheck phases to produce
-- a dependencies information for the module being compiled.
mkDependencies :: TcGblEnv -> IO Dependencies
mkDependencies
          TcGblEnv{ tcg_mod = mod,
                    tcg_imports = imports,
                    tcg_th_used = th_var
                  }
 = do
      -- Template Haskell used?
      th_used <- readIORef th_var
      let dep_mods = eltsUFM (delFromUFM (imp_dep_mods imports) (moduleName mod))
                -- M.hi-boot can be in the imp_dep_mods, but we must remove
                -- it before recording the modules on which this one depends!
                -- (We want to retain M.hi-boot in imp_dep_mods so that
                --  loadHiBootInterface can see if M's direct imports depend
                --  on M.hi-boot, and hence that we should do the hi-boot consistency
                --  check.)

          pkgs | th_used   = insertList thPackageId (imp_dep_pkgs imports)
               | otherwise = imp_dep_pkgs imports

          -- Set the packages required to be Safe according to Safe Haskell.
          -- See Note [RnNames . Tracking Trust Transitively]
          sorted_pkgs = sortBy stablePackageIdCmp pkgs
          trust_pkgs  = imp_trust_pkgs imports
          dep_pkgs'   = map (\x -> (x, x `elem` trust_pkgs)) sorted_pkgs

      return Deps { dep_mods   = sortBy (stableModuleNameCmp `on` fst) dep_mods,
                    dep_pkgs   = dep_pkgs',
                    dep_orphs  = sortBy stableModuleCmp (imp_orphs  imports),
                    dep_finsts = sortBy stableModuleCmp (imp_finsts imports) }
                    -- sort to get into canonical order
                    -- NB. remember to use lexicographic ordering

mkIface_ :: HscEnv -> Maybe Fingerprint -> Module -> IsBootInterface
         -> NameSet -> Bool -> Dependencies -> GlobalRdrEnv
         -> NameEnv FixItem -> Warnings -> HpcInfo
         -> ImportedMods -> Bool
         -> [FilePath]
         -> SafeHaskellMode
         -> ModDetails
         -> IO (Messages, Maybe (ModIface, Bool))
mkIface_ hsc_env maybe_old_fingerprint
         this_mod is_boot used_names used_th deps rdr_env fix_env src_warns
         hpc_info dir_imp_mods pkg_trust_req dependent_files safe_mode
         ModDetails{  md_insts     = insts,
                      md_fam_insts = fam_insts,
                      md_rules     = rules,
                      md_anns      = anns,
                      md_vect_info = vect_info,
                      md_types     = type_env,
                      md_exports   = exports }
-- NB:  notice that mkIface does not look at the bindings
--      only at the TypeEnv.  The previous Tidy phase has
--      put exactly the info into the TypeEnv that we want
--      to expose in the interface

  = do
    usages  <- mkUsageInfo hsc_env this_mod dir_imp_mods used_names dependent_files

    let entities = typeEnvElts type_env
        decls  = [ tyThingToIfaceDecl entity
                 | entity <- entities,
                   let name = getName entity,
                   not (isImplicitTyThing entity),
                      -- No implicit Ids and class tycons in the interface file
                   not (isWiredInName name),
                      -- Nor wired-in things; the compiler knows about them anyhow
                   nameIsLocalOrFrom this_mod name  ]
                      -- Sigh: see Note [Root-main Id] in TcRnDriver

        fixities    = [(occ,fix) | FixItem occ fix <- nameEnvElts fix_env]
        warns       = src_warns
        iface_rules = map (coreRuleToIfaceRule this_mod) rules
        iface_insts = map instanceToIfaceInst insts
        iface_fam_insts = map famInstToIfaceFamInst fam_insts
        iface_vect_info = flattenVectInfo vect_info
        trust_info  = setSafeMode safe_mode
        annotations = map mkIfaceAnnotation anns

        intermediate_iface = ModIface {
              mi_module      = this_mod,
              mi_boot        = is_boot,
              mi_deps        = deps,
              mi_usages      = usages,
              mi_exports     = mkIfaceExports exports,

              -- Sort these lexicographically, so that
              -- the result is stable across compilations
              mi_insts       = sortBy cmp_inst     iface_insts,
              mi_fam_insts   = sortBy cmp_fam_inst iface_fam_insts,
              mi_rules       = sortBy cmp_rule     iface_rules,

              mi_vect_info   = iface_vect_info,

              mi_fixities    = fixities,
              mi_warns       = warns,
              mi_anns        = annotations,
              mi_globals     = maybeGlobalRdrEnv rdr_env,

              -- Left out deliberately: filled in by addFingerprints
              mi_iface_hash  = fingerprint0,
              mi_mod_hash    = fingerprint0,
              mi_flag_hash   = fingerprint0,
              mi_exp_hash    = fingerprint0,
              mi_used_th     = used_th,
              mi_orphan_hash = fingerprint0,
              mi_orphan      = False, -- Always set by addFingerprints, but
                                      -- it's a strict field, so we can't omit it.
              mi_finsts      = False, -- Ditto
              mi_decls       = deliberatelyOmitted "decls",
              mi_hash_fn     = deliberatelyOmitted "hash_fn",
              mi_hpc         = isHpcUsed hpc_info,
              mi_trust       = trust_info,
              mi_trust_pkg   = pkg_trust_req,

              -- And build the cached values
              mi_warn_fn     = mkIfaceWarnCache warns,
              mi_fix_fn      = mkIfaceFixCache fixities }

    (new_iface, no_change_at_all)
          <- {-# SCC "versioninfo" #-}
                   addFingerprints hsc_env maybe_old_fingerprint
                                   intermediate_iface decls

    -- Warn about orphans
    let warn_orphs      = wopt Opt_WarnOrphans dflags
        warn_auto_orphs = wopt Opt_WarnAutoOrphans dflags
        orph_warnings   --- Laziness means no work done unless -fwarn-orphans
          | warn_orphs || warn_auto_orphs = rule_warns `unionBags` inst_warns
          | otherwise                     = emptyBag
        errs_and_warns = (orph_warnings, emptyBag)
        unqual = mkPrintUnqualified dflags rdr_env
        inst_warns = listToBag [ instOrphWarn dflags unqual d
                               | (d,i) <- insts `zip` iface_insts
                               , isNothing (ifInstOrph i) ]
        rule_warns = listToBag [ ruleOrphWarn dflags unqual this_mod r
                               | r <- iface_rules
                               , isNothing (ifRuleOrph r)
                               , if ifRuleAuto r then warn_auto_orphs
                                                 else warn_orphs ]

    if errorsFound dflags errs_and_warns
      then return ( errs_and_warns, Nothing )
      else do
        -- Debug printing
        dumpIfSet_dyn dflags Opt_D_dump_hi "FINAL INTERFACE"
                      (pprModIface new_iface)

        -- bug #1617: on reload we weren't updating the PrintUnqualified
        -- correctly.  This stems from the fact that the interface had
        -- not changed, so addFingerprints returns the old ModIface
        -- with the old GlobalRdrEnv (mi_globals).
        let final_iface = new_iface{ mi_globals = maybeGlobalRdrEnv rdr_env }

        return (errs_and_warns, Just (final_iface, no_change_at_all))
  where
     cmp_rule     = comparing ifRuleName
     -- Compare these lexicographically by OccName, *not* by unique,
     -- because the latter is not stable across compilations:
     cmp_inst     = comparing (nameOccName . ifDFun)
     cmp_fam_inst = comparing (nameOccName . ifFamInstTcName)

     dflags = hsc_dflags hsc_env

     -- We only fill in mi_globals if the module was compiled to byte
     -- code.  Otherwise, the compiler may not have retained all the
     -- top-level bindings and they won't be in the TypeEnv (see
     -- Desugar.addExportFlagsAndRules).  The mi_globals field is used
     -- by GHCi to decide whether the module has its full top-level
     -- scope available. (#5534)
     maybeGlobalRdrEnv :: GlobalRdrEnv -> Maybe GlobalRdrEnv
     maybeGlobalRdrEnv rdr_env
         | targetRetainsAllBindings (hscTarget dflags) = Just rdr_env
         | otherwise                                   = Nothing

     deliberatelyOmitted :: String -> a
     deliberatelyOmitted x = panic ("Deliberately omitted: " ++ x)

     ifFamInstTcName = ifFamInstFam

     flattenVectInfo (VectInfo { vectInfoVar            = vVar
                               , vectInfoTyCon          = vTyCon
                               , vectInfoParallelVars     = vParallelVars
                               , vectInfoParallelTyCons = vParallelTyCons
                               }) =
       IfaceVectInfo
       { ifaceVectInfoVar            = [Var.varName v | (v, _  ) <- varEnvElts  vVar]
       , ifaceVectInfoTyCon          = [tyConName t   | (t, t_v) <- nameEnvElts vTyCon, t /= t_v]
       , ifaceVectInfoTyConReuse     = [tyConName t   | (t, t_v) <- nameEnvElts vTyCon, t == t_v]
       , ifaceVectInfoParallelVars   = [Var.varName v | v <- varSetElems vParallelVars]
       , ifaceVectInfoParallelTyCons = nameSetToList vParallelTyCons
       }

-----------------------------
writeIfaceFile :: DynFlags -> FilePath -> ModIface -> IO ()
writeIfaceFile dflags hi_file_path new_iface
    = do createDirectoryIfMissing True (takeDirectory hi_file_path)
         writeBinIface dflags hi_file_path new_iface


-- -----------------------------------------------------------------------------
-- Look up parents and versions of Names

-- This is like a global version of the mi_hash_fn field in each ModIface.
-- Given a Name, it finds the ModIface, and then uses mi_hash_fn to get
-- the parent and version info.

mkHashFun
        :: HscEnv                       -- needed to look up versions
        -> ExternalPackageState         -- ditto
        -> (Name -> Fingerprint)
mkHashFun hsc_env eps
  = \name ->
      let
        mod = ASSERT2( isExternalName name, ppr name ) nameModule name
        occ = nameOccName name
        iface = lookupIfaceByModule (hsc_dflags hsc_env) hpt pit mod `orElse`
                   pprPanic "lookupVers2" (ppr mod <+> ppr occ)
      in
        snd (mi_hash_fn iface occ `orElse`
                  pprPanic "lookupVers1" (ppr mod <+> ppr occ))
  where
      hpt = hsc_HPT hsc_env
      pit = eps_PIT eps

-- ---------------------------------------------------------------------------
-- Compute fingerprints for the interface

addFingerprints
        :: HscEnv
        -> Maybe Fingerprint -- the old fingerprint, if any
        -> ModIface          -- The new interface (lacking decls)
        -> [IfaceDecl]       -- The new decls
        -> IO (ModIface,     -- Updated interface
               Bool)         -- True <=> no changes at all;
                             -- no need to write Iface

addFingerprints hsc_env mb_old_fingerprint iface0 new_decls
 = do
   eps <- hscEPS hsc_env
   let
        -- The ABI of a declaration represents everything that is made
        -- visible about the declaration that a client can depend on.
        -- see IfaceDeclABI below.
       declABI :: IfaceDecl -> IfaceDeclABI
       declABI decl = (this_mod, decl, extras)
        where extras = declExtras fix_fn ann_fn non_orph_rules non_orph_insts
                                  non_orph_fis decl

       edges :: [(IfaceDeclABI, Unique, [Unique])]
       edges = [ (abi, getUnique (ifName decl), out)
               | decl <- new_decls
               , let abi = declABI decl
               , let out = localOccs $ freeNamesDeclABI abi
               ]

       name_module n = ASSERT2( isExternalName n, ppr n ) nameModule n
       localOccs = map (getUnique . getParent . getOccName)
                        . filter ((== this_mod) . name_module)
                        . nameSetToList
          where getParent occ = lookupOccEnv parent_map occ `orElse` occ

        -- maps OccNames to their parents in the current module.
        -- e.g. a reference to a constructor must be turned into a reference
        -- to the TyCon for the purposes of calculating dependencies.
       parent_map :: OccEnv OccName
       parent_map = foldr extend emptyOccEnv new_decls
          where extend d env =
                  extendOccEnvList env [ (b,n) | b <- ifaceDeclImplicitBndrs d ]
                  where n = ifName d

        -- strongly-connected groups of declarations, in dependency order
       groups = stronglyConnCompFromEdgedVertices edges

       global_hash_fn = mkHashFun hsc_env eps

        -- how to output Names when generating the data to fingerprint.
        -- Here we want to output the fingerprint for each top-level
        -- Name, whether it comes from the current module or another
        -- module.  In this way, the fingerprint for a declaration will
        -- change if the fingerprint for anything it refers to (transitively)
        -- changes.
       mk_put_name :: (OccEnv (OccName,Fingerprint))
                   -> BinHandle -> Name -> IO  ()
       mk_put_name local_env bh name
          | isWiredInName name  =  putNameLiterally bh name
           -- wired-in names don't have fingerprints
          | otherwise
          = ASSERT2( isExternalName name, ppr name )
            let hash | nameModule name /= this_mod =  global_hash_fn name
                     | otherwise = snd (lookupOccEnv local_env (getOccName name)
                           `orElse` pprPanic "urk! lookup local fingerprint"
                                       (ppr name)) -- (undefined,fingerprint0))
                -- This panic indicates that we got the dependency
                -- analysis wrong, because we needed a fingerprint for
                -- an entity that wasn't in the environment.  To debug
                -- it, turn the panic into a trace, uncomment the
                -- pprTraces below, run the compile again, and inspect
                -- the output and the generated .hi file with
                -- --show-iface.
            in put_ bh hash

        -- take a strongly-connected group of declarations and compute
        -- its fingerprint.

       fingerprint_group :: (OccEnv (OccName,Fingerprint),
                             [(Fingerprint,IfaceDecl)])
                         -> SCC IfaceDeclABI
                         -> IO (OccEnv (OccName,Fingerprint),
                                [(Fingerprint,IfaceDecl)])

       fingerprint_group (local_env, decls_w_hashes) (AcyclicSCC abi)
          = do let hash_fn = mk_put_name local_env
                   decl = abiDecl abi
               -- pprTrace "fingerprinting" (ppr (ifName decl) ) $ do
               hash <- computeFingerprint hash_fn abi
               env' <- extend_hash_env local_env (hash,decl)
               return (env', (hash,decl) : decls_w_hashes)

       fingerprint_group (local_env, decls_w_hashes) (CyclicSCC abis)
          = do let decls = map abiDecl abis
               local_env1 <- foldM extend_hash_env local_env
                                   (zip (repeat fingerprint0) decls)
               let hash_fn = mk_put_name local_env1
               -- pprTrace "fingerprinting" (ppr (map ifName decls) ) $ do
               let stable_abis = sortBy cmp_abiNames abis
                -- put the cycle in a canonical order
               hash <- computeFingerprint hash_fn stable_abis
               let pairs = zip (repeat hash) decls
               local_env2 <- foldM extend_hash_env local_env pairs
               return (local_env2, pairs ++ decls_w_hashes)

       -- we have fingerprinted the whole declaration, but we now need
       -- to assign fingerprints to all the OccNames that it binds, to
       -- use when referencing those OccNames in later declarations.
       --
       extend_hash_env :: OccEnv (OccName,Fingerprint)
                       -> (Fingerprint,IfaceDecl)
                       -> IO (OccEnv (OccName,Fingerprint))
       extend_hash_env env0 (hash,d) = do
          return (foldr (\(b,fp) env -> extendOccEnv env b (b,fp)) env0
                 (ifaceDeclFingerprints hash d))

   --
   (local_env, decls_w_hashes) <-
       foldM fingerprint_group (emptyOccEnv, []) groups

   -- when calculating fingerprints, we always need to use canonical
   -- ordering for lists of things.  In particular, the mi_deps has various
   -- lists of modules and suchlike, so put these all in canonical order:
   let sorted_deps = sortDependencies (mi_deps iface0)

   -- the export hash of a module depends on the orphan hashes of the
   -- orphan modules below us in the dependency tree.  This is the way
   -- that changes in orphans get propagated all the way up the
   -- dependency tree.  We only care about orphan modules in the current
   -- package, because changes to orphans outside this package will be
   -- tracked by the usage on the ABI hash of package modules that we import.
   let orph_mods = filter ((== this_pkg) . modulePackageId)
                   $ dep_orphs sorted_deps
   dep_orphan_hashes <- getOrphanHashes hsc_env orph_mods

   orphan_hash <- computeFingerprint (mk_put_name local_env)
                      (map ifDFun orph_insts, orph_rules, orph_fis)

   -- the export list hash doesn't depend on the fingerprints of
   -- the Names it mentions, only the Names themselves, hence putNameLiterally.
   export_hash <- computeFingerprint putNameLiterally
                      (mi_exports iface0,
                       orphan_hash,
                       dep_orphan_hashes,
                       dep_pkgs (mi_deps iface0),
                        -- dep_pkgs: see "Package Version Changes" on
                        -- wiki/Commentary/Compiler/RecompilationAvoidance
                       mi_trust iface0)
                        -- Make sure change of Safe Haskell mode causes recomp.

   -- put the declarations in a canonical order, sorted by OccName
   let sorted_decls = Map.elems $ Map.fromList $
                          [(ifName d, e) | e@(_, d) <- decls_w_hashes]

   -- the flag hash depends on:
   --   - (some of) dflags
   -- it returns two hashes, one that shouldn't change
   -- the abi hash and one that should
   flag_hash <- fingerprintDynFlags dflags this_mod putNameLiterally

   -- the ABI hash depends on:
   --   - decls
   --   - export list
   --   - orphans
   --   - deprecations
   --   - vect info
   --   - flag abi hash
   mod_hash <- computeFingerprint putNameLiterally
                      (map fst sorted_decls,
                       export_hash,  -- includes orphan_hash
                       mi_warns iface0,
                       mi_vect_info iface0)

   -- The interface hash depends on:
   --   - the ABI hash, plus
   --   - the module level annotations,
   --   - usages
   --   - deps (home and external packages, dependent files)
   --   - hpc
   iface_hash <- computeFingerprint putNameLiterally
                      (mod_hash,
                       ann_fn (mkVarOcc "module"),  -- See mkIfaceAnnCache
                       mi_usages iface0,
                       sorted_deps,
                       mi_hpc iface0)

   let
    no_change_at_all = Just iface_hash == mb_old_fingerprint

    final_iface = iface0 {
                mi_mod_hash    = mod_hash,
                mi_iface_hash  = iface_hash,
                mi_exp_hash    = export_hash,
                mi_orphan_hash = orphan_hash,
                mi_flag_hash   = flag_hash,
                mi_orphan      = not (   null orph_rules
                                      && null orph_insts
                                      && null orph_fis
                                      && isNoIfaceVectInfo (mi_vect_info iface0)),
                mi_finsts      = not . null $ mi_fam_insts iface0,
                mi_decls       = sorted_decls,
                mi_hash_fn     = lookupOccEnv local_env }
   --
   return (final_iface, no_change_at_all)

  where
    this_mod = mi_module iface0
    dflags = hsc_dflags hsc_env
    this_pkg = thisPackage dflags
    (non_orph_insts, orph_insts) = mkOrphMap ifInstOrph    (mi_insts iface0)
    (non_orph_rules, orph_rules) = mkOrphMap ifRuleOrph    (mi_rules iface0)
    (non_orph_fis,   orph_fis)   = mkOrphMap ifFamInstOrph (mi_fam_insts iface0)
    fix_fn = mi_fix_fn iface0
    ann_fn = mkIfaceAnnCache (mi_anns iface0)

getOrphanHashes :: HscEnv -> [Module] -> IO [Fingerprint]
getOrphanHashes hsc_env mods = do
  eps <- hscEPS hsc_env
  let
    hpt        = hsc_HPT hsc_env
    pit        = eps_PIT eps
    dflags     = hsc_dflags hsc_env
    get_orph_hash mod =
          case lookupIfaceByModule dflags hpt pit mod of
            Nothing    -> pprPanic "moduleOrphanHash" (ppr mod)
            Just iface -> mi_orphan_hash iface
  --
  return (map get_orph_hash mods)


sortDependencies :: Dependencies -> Dependencies
sortDependencies d
 = Deps { dep_mods   = sortBy (compare `on` (moduleNameFS.fst)) (dep_mods d),
          dep_pkgs   = sortBy (stablePackageIdCmp `on` fst) (dep_pkgs d),
          dep_orphs  = sortBy stableModuleCmp (dep_orphs d),
          dep_finsts = sortBy stableModuleCmp (dep_finsts d) }
\end{code}


\begin{code}
-- | Creates cached lookup for the 'mi_anns' field of ModIface
-- Hackily, we use "module" as the OccName for any module-level annotations
mkIfaceAnnCache :: [IfaceAnnotation] -> OccName -> [AnnPayload]
mkIfaceAnnCache anns
  = \n -> lookupOccEnv env n `orElse` []
  where
    pair (IfaceAnnotation target value) =
      (case target of
          NamedTarget occn -> occn
          ModuleTarget _   -> mkVarOcc "module"
      , [value])
    -- flipping (++), so the first argument is always short
    env = mkOccEnv_C (flip (++)) (map pair anns)
\end{code}

%************************************************************************
%*                                                                      *
          The ABI of an IfaceDecl
%*                                                                      *
%************************************************************************

Note [The ABI of an IfaceDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ABI of a declaration consists of:

   (a) the full name of the identifier (inc. module and package,
       because these are used to construct the symbol name by which
       the identifier is known externally).

   (b) the declaration itself, as exposed to clients.  That is, the
       definition of an Id is included in the fingerprint only if
       it is made available as an unfolding in the interface.

   (c) the fixity of the identifier
   (d) for Ids: rules
   (e) for classes: instances, fixity & rules for methods
   (f) for datatypes: instances, fixity & rules for constrs

Items (c)-(f) are not stored in the IfaceDecl, but instead appear
elsewhere in the interface file.  But they are *fingerprinted* with
the declaration itself. This is done by grouping (c)-(f) in IfaceDeclExtras,
and fingerprinting that as part of the declaration.

\begin{code}
type IfaceDeclABI = (Module, IfaceDecl, IfaceDeclExtras)

data IfaceDeclExtras
  = IfaceIdExtras IfaceIdExtras

  | IfaceDataExtras
       Fixity                   -- Fixity of the tycon itself
       [IfaceInstABI]           -- Local class and family instances of this tycon
                                -- See Note [Orphans] in IfaceSyn
       [AnnPayload]             -- Annotations of the type itself
       [IfaceIdExtras]          -- For each constructor: fixity, RULES and annotations

  | IfaceClassExtras
       Fixity                   -- Fixity of the class itself
       [IfaceInstABI]           -- Local instances of this class *or*
                                --   of its associated data types
                                -- See Note [Orphans] in IfaceSyn
       [AnnPayload]             -- Annotations of the type itself
       [IfaceIdExtras]          -- For each class method: fixity, RULES and annotations

  | IfaceSynExtras   Fixity [IfaceInstABI] [AnnPayload]

  | IfaceOtherDeclExtras

data IfaceIdExtras
  = IdExtras
       Fixity                   -- Fixity of the Id
       [IfaceRule]              -- Rules for the Id
       [AnnPayload]             -- Annotations for the Id

-- When hashing a class or family instance, we hash only the
-- DFunId or CoAxiom, because that depends on all the
-- information about the instance.
--
type IfaceInstABI = IfExtName   -- Name of DFunId or CoAxiom that is evidence for the instance

abiDecl :: IfaceDeclABI -> IfaceDecl
abiDecl (_, decl, _) = decl

cmp_abiNames :: IfaceDeclABI -> IfaceDeclABI -> Ordering
cmp_abiNames abi1 abi2 = ifName (abiDecl abi1) `compare`
                         ifName (abiDecl abi2)

freeNamesDeclABI :: IfaceDeclABI -> NameSet
freeNamesDeclABI (_mod, decl, extras) =
  freeNamesIfDecl decl `unionNameSets` freeNamesDeclExtras extras

freeNamesDeclExtras :: IfaceDeclExtras -> NameSet
freeNamesDeclExtras (IfaceIdExtras id_extras)
  = freeNamesIdExtras id_extras
freeNamesDeclExtras (IfaceDataExtras  _ insts _ subs)
  = unionManyNameSets (mkNameSet insts : map freeNamesIdExtras subs)
freeNamesDeclExtras (IfaceClassExtras _ insts _ subs)
  = unionManyNameSets (mkNameSet insts : map freeNamesIdExtras subs)
freeNamesDeclExtras (IfaceSynExtras _ insts _)
  = mkNameSet insts
freeNamesDeclExtras IfaceOtherDeclExtras
  = emptyNameSet

freeNamesIdExtras :: IfaceIdExtras -> NameSet
freeNamesIdExtras (IdExtras _ rules _) = unionManyNameSets (map freeNamesIfRule rules)

instance Outputable IfaceDeclExtras where
  ppr IfaceOtherDeclExtras       = empty
  ppr (IfaceIdExtras  extras)    = ppr_id_extras extras
  ppr (IfaceSynExtras fix finsts anns) = vcat [ppr fix, ppr finsts, ppr anns]
  ppr (IfaceDataExtras fix insts anns stuff) = vcat [ppr fix, ppr_insts insts, ppr anns,
                                                ppr_id_extras_s stuff]
  ppr (IfaceClassExtras fix insts anns stuff) = vcat [ppr fix, ppr_insts insts, ppr anns,
                                                 ppr_id_extras_s stuff]

ppr_insts :: [IfaceInstABI] -> SDoc
ppr_insts _ = ptext (sLit "<insts>")

ppr_id_extras_s :: [IfaceIdExtras] -> SDoc
ppr_id_extras_s stuff = vcat (map ppr_id_extras stuff)

ppr_id_extras :: IfaceIdExtras -> SDoc
ppr_id_extras (IdExtras fix rules anns) = ppr fix $$ vcat (map ppr rules) $$ vcat (map ppr anns)

-- This instance is used only to compute fingerprints
instance Binary IfaceDeclExtras where
  get _bh = panic "no get for IfaceDeclExtras"
  put_ bh (IfaceIdExtras extras) = do
   putByte bh 1; put_ bh extras
  put_ bh (IfaceDataExtras fix insts anns cons) = do
   putByte bh 2; put_ bh fix; put_ bh insts; put_ bh anns; put_ bh cons
  put_ bh (IfaceClassExtras fix insts anns methods) = do
   putByte bh 3; put_ bh fix; put_ bh insts; put_ bh anns; put_ bh methods
  put_ bh (IfaceSynExtras fix finsts anns) = do
   putByte bh 4; put_ bh fix; put_ bh finsts; put_ bh anns
  put_ bh IfaceOtherDeclExtras = putByte bh 5

instance Binary IfaceIdExtras where
  get _bh = panic "no get for IfaceIdExtras"
  put_ bh (IdExtras fix rules anns)= do { put_ bh fix; put_ bh rules; put_ bh anns }

declExtras :: (OccName -> Fixity)
           -> (OccName -> [AnnPayload])
           -> OccEnv [IfaceRule]
           -> OccEnv [IfaceClsInst]
           -> OccEnv [IfaceFamInst]
           -> IfaceDecl
           -> IfaceDeclExtras

declExtras fix_fn ann_fn rule_env inst_env fi_env decl
  = case decl of
      IfaceId{} -> IfaceIdExtras (id_extras n)
      IfaceData{ifCons=cons} ->
                     IfaceDataExtras (fix_fn n)
                        (map ifFamInstAxiom (lookupOccEnvL fi_env n) ++
                         map ifDFun         (lookupOccEnvL inst_env n))
                        (ann_fn n)
                        (map (id_extras . ifConOcc) (visibleIfConDecls cons))
      IfaceClass{ifSigs=sigs, ifATs=ats} ->
                     IfaceClassExtras (fix_fn n)
                        (map ifDFun $ (concatMap at_extras ats)
                                    ++ lookupOccEnvL inst_env n)
                           -- Include instances of the associated types
                           -- as well as instances of the class (Trac #5147)
                        (ann_fn n)
                        [id_extras op | IfaceClassOp op _ _ <- sigs]
      IfaceSyn{} -> IfaceSynExtras (fix_fn n)
                        (map ifFamInstAxiom (lookupOccEnvL fi_env n))
                        (ann_fn n)
      _other -> IfaceOtherDeclExtras
  where
        n = ifName decl
        id_extras occ = IdExtras (fix_fn occ) (lookupOccEnvL rule_env occ) (ann_fn occ)
        at_extras (IfaceAT decl _) = lookupOccEnvL inst_env (ifName decl)


lookupOccEnvL :: OccEnv [v] -> OccName -> [v]
lookupOccEnvL env k = lookupOccEnv env k `orElse` []

-- used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: BinHandle -> Name -> IO ()
putNameLiterally bh name = ASSERT( isExternalName name )
  do
    put_ bh $! nameModule name
    put_ bh $! nameOccName name

{-
-- for testing: use the md5sum command to generate fingerprints and
-- compare the results against our built-in version.
  fp' <- oldMD5 dflags bh
  if fp /= fp' then pprPanic "computeFingerprint" (ppr fp <+> ppr fp')
               else return fp

oldMD5 dflags bh = do
  tmp <- newTempName dflags "bin"
  writeBinMem bh tmp
  tmp2 <- newTempName dflags "md5"
  let cmd = "md5sum " ++ tmp ++ " >" ++ tmp2
  r <- system cmd
  case r of
    ExitFailure _ -> throwGhcExceptionIO (PhaseFailed cmd r)
    ExitSuccess -> do
        hash_str <- readFile tmp2
        return $! readHexFingerprint hash_str
-}

instOrphWarn :: DynFlags -> PrintUnqualified -> ClsInst -> WarnMsg
instOrphWarn dflags unqual inst
  = mkWarnMsg dflags (getSrcSpan inst) unqual $
    hang (ptext (sLit "Orphan instance:")) 2 (pprInstanceHdr inst)

ruleOrphWarn :: DynFlags -> PrintUnqualified -> Module -> IfaceRule -> WarnMsg
ruleOrphWarn dflags unqual mod rule
  = mkWarnMsg dflags silly_loc unqual $
    ptext (sLit "Orphan rule:") <+> ppr rule
  where
    silly_loc = srcLocSpan (mkSrcLoc (moduleNameFS (moduleName mod)) 1 1)
    -- We don't have a decent SrcSpan for a Rule, not even the CoreRule
    -- Could readily be fixed by adding a SrcSpan to CoreRule, if we wanted to

----------------------
-- mkOrphMap partitions instance decls or rules into
--      (a) an OccEnv for ones that are not orphans,
--          mapping the local OccName to a list of its decls
--      (b) a list of orphan decls
mkOrphMap :: (decl -> Maybe OccName)    -- (Just occ) for a non-orphan decl, keyed by occ
                                        -- Nothing for an orphan decl
          -> [decl]                     -- Sorted into canonical order
          -> (OccEnv [decl],            -- Non-orphan decls associated with their key;
                                        --      each sublist in canonical order
              [decl])                   -- Orphan decls; in canonical order
mkOrphMap get_key decls
  = foldl go (emptyOccEnv, []) decls
  where
    go (non_orphs, orphs) d
        | Just occ <- get_key d
        = (extendOccEnv_Acc (:) singleton non_orphs occ d, orphs)
        | otherwise = (non_orphs, d:orphs)
\end{code}


%************************************************************************
%*                                                                      *
       Keeping track of what we've slurped, and fingerprints
%*                                                                      *
%************************************************************************

\begin{code}
mkUsageInfo :: HscEnv -> Module -> ImportedMods -> NameSet -> [FilePath] -> IO [Usage]
mkUsageInfo hsc_env this_mod dir_imp_mods used_names dependent_files
  = do
    eps <- hscEPS hsc_env
    hashes <- mapM getFileHash dependent_files
    let mod_usages = mk_mod_usage_info (eps_PIT eps) hsc_env this_mod
                                       dir_imp_mods used_names
    let usages = mod_usages ++ [ UsageFile { usg_file_path = f
                                           , usg_file_hash = hash }
                               | (f, hash) <- zip dependent_files hashes ]
    usages `seqList` return usages
    -- seq the list of Usages returned: occasionally these
    -- don't get evaluated for a while and we can end up hanging on to
    -- the entire collection of Ifaces.

mk_mod_usage_info :: PackageIfaceTable
              -> HscEnv
              -> Module
              -> ImportedMods
              -> NameSet
              -> [Usage]
mk_mod_usage_info pit hsc_env this_mod direct_imports used_names
  = mapMaybe mkUsage usage_mods
  where
    hpt = hsc_HPT hsc_env
    dflags = hsc_dflags hsc_env
    this_pkg = thisPackage dflags

    used_mods    = moduleEnvKeys ent_map
    dir_imp_mods = moduleEnvKeys direct_imports
    all_mods     = used_mods ++ filter (`notElem` used_mods) dir_imp_mods
    usage_mods   = sortBy stableModuleCmp all_mods
                        -- canonical order is imported, to avoid interface-file
                        -- wobblage.

    -- ent_map groups together all the things imported and used
    -- from a particular module
    ent_map :: ModuleEnv [OccName]
    ent_map  = foldNameSet add_mv emptyModuleEnv used_names
     where
      add_mv name mv_map
        | isWiredInName name = mv_map  -- ignore wired-in names
        | otherwise
        = case nameModule_maybe name of
             Nothing  -> ASSERT2( isSystemName name, ppr name ) mv_map
                -- See Note [Internal used_names]

             Just mod -> -- This lambda function is really just a
                         -- specialised (++); originally came about to
                         -- avoid quadratic behaviour (trac #2680)
                         extendModuleEnvWith (\_ xs -> occ:xs) mv_map mod [occ]
                where occ = nameOccName name

    -- We want to create a Usage for a home module if
    --  a) we used something from it; has something in used_names
    --  b) we imported it, even if we used nothing from it
    --     (need to recompile if its export list changes: export_fprint)
    mkUsage :: Module -> Maybe Usage
    mkUsage mod
      | isNothing maybe_iface           -- We can't depend on it if we didn't
                                        -- load its interface.
      || mod == this_mod                -- We don't care about usages of
                                        -- things in *this* module
      = Nothing

      | modulePackageId mod /= this_pkg
      = Just UsagePackageModule{ usg_mod      = mod,
                                 usg_mod_hash = mod_hash,
                                 usg_safe     = imp_safe }
        -- for package modules, we record the module hash only

      | (null used_occs
          && isNothing export_hash
          && not is_direct_import
          && not finsts_mod)
      = Nothing                 -- Record no usage info
        -- for directly-imported modules, we always want to record a usage
        -- on the orphan hash.  This is what triggers a recompilation if
        -- an orphan is added or removed somewhere below us in the future.

      | otherwise
      = Just UsageHomeModule {
                      usg_mod_name = moduleName mod,
                      usg_mod_hash = mod_hash,
                      usg_exports  = export_hash,
                      usg_entities = Map.toList ent_hashs,
                      usg_safe     = imp_safe }
      where
        maybe_iface  = lookupIfaceByModule dflags hpt pit mod
                -- In one-shot mode, the interfaces for home-package
                -- modules accumulate in the PIT not HPT.  Sigh.

        Just iface   = maybe_iface
        finsts_mod   = mi_finsts    iface
        hash_env     = mi_hash_fn   iface
        mod_hash     = mi_mod_hash  iface
        export_hash | depend_on_exports = Just (mi_exp_hash iface)
                    | otherwise         = Nothing

        (is_direct_import, imp_safe)
            = case lookupModuleEnv direct_imports mod of
                Just ((_,_,_,safe):_xs) -> (True, safe)
                Just _                  -> pprPanic "mkUsage: empty direct import" empty
                Nothing                 -> (False, safeImplicitImpsReq dflags)
                -- Nothing case is for implicit imports like 'System.IO' when 'putStrLn'
                -- is used in the source code. We require them to be safe in Safe Haskell

        used_occs = lookupModuleEnv ent_map mod `orElse` []

        -- Making a Map here ensures that (a) we remove duplicates
        -- when we have usages on several subordinates of a single parent,
        -- and (b) that the usages emerge in a canonical order, which
        -- is why we use Map rather than OccEnv: Map works
        -- using Ord on the OccNames, which is a lexicographic ordering.
        ent_hashs :: Map OccName Fingerprint
        ent_hashs = Map.fromList (map lookup_occ used_occs)

        lookup_occ occ =
            case hash_env occ of
                Nothing -> pprPanic "mkUsage" (ppr mod <+> ppr occ <+> ppr used_names)
                Just r  -> r

        depend_on_exports = is_direct_import
        {- True
              Even if we used 'import M ()', we have to register a
              usage on the export list because we are sensitive to
              changes in orphan instances/rules.
           False
              In GHC 6.8.x we always returned true, and in
              fact it recorded a dependency on *all* the
              modules underneath in the dependency tree.  This
              happens to make orphans work right, but is too
              expensive: it'll read too many interface files.
              The 'isNothing maybe_iface' check above saved us
              from generating many of these usages (at least in
              one-shot mode), but that's even more bogus!
        -}
\end{code}

\begin{code}
mkIfaceAnnotation :: Annotation -> IfaceAnnotation
mkIfaceAnnotation (Annotation { ann_target = target, ann_value = payload }) 
  = IfaceAnnotation {
        ifAnnotatedTarget = fmap nameOccName target,
        ifAnnotatedValue = payload
    }
\end{code}

\begin{code}
mkIfaceExports :: [AvailInfo] -> [IfaceExport]  -- Sort to make canonical
mkIfaceExports exports
  = sortBy stableAvailCmp (map sort_subs exports)
  where
    sort_subs :: AvailInfo -> AvailInfo
    sort_subs (Avail n) = Avail n
    sort_subs (AvailTC n []) = AvailTC n []
    sort_subs (AvailTC n (m:ms))
       | n==m      = AvailTC n (m:sortBy stableNameCmp ms)
       | otherwise = AvailTC n (sortBy stableNameCmp (m:ms))
       -- Maintain the AvailTC Invariant
\end{code}

Note [Orignal module]
~~~~~~~~~~~~~~~~~~~~~
Consider this:
        module X where { data family T }
        module Y( T(..) ) where { import X; data instance T Int = MkT Int }
The exported Avail from Y will look like
        X.T{X.T, Y.MkT}
That is, in Y,
  - only MkT is brought into scope by the data instance;
  - but the parent (used for grouping and naming in T(..) exports) is X.T
  - and in this case we export X.T too

In the result of MkIfaceExports, the names are grouped by defining module,
so we may need to split up a single Avail into multiple ones.

Note [Internal used_names]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the used_names are External Names, but we can have Internal
Names too: see Note [Binders in Template Haskell] in Convert, and
Trac #5362 for an example.  Such Names are always
  - Such Names are always for locally-defined things, for which we
    don't gather usage info, so we can just ignore them in ent_map
  - They are always System Names, hence the assert, just as a double check.


%************************************************************************
%*                                                                      *
        Load the old interface file for this module (unless
        we have it already), and check whether it is up to date
%*                                                                      *
%************************************************************************

\begin{code}
data RecompileRequired
  = UpToDate
       -- ^ everything is up to date, recompilation is not required
  | MustCompile
       -- ^ The .hs file has been touched, or the .o/.hi file does not exist
  | RecompBecause String
       -- ^ The .o/.hi files are up to date, but something else has changed
       -- to force recompilation; the String says what (one-line summary)
   deriving Eq

recompileRequired :: RecompileRequired -> Bool
recompileRequired UpToDate = False
recompileRequired _ = True



-- | Top level function to check if the version of an old interface file
-- is equivalent to the current source file the user asked us to compile.
-- If the same, we can avoid recompilation. We return a tuple where the
-- first element is a bool saying if we should recompile the object file
-- and the second is maybe the interface file, where Nothng means to
-- rebuild the interface file not use the exisitng one.
checkOldIface
  :: HscEnv
  -> ModSummary
  -> SourceModified
  -> Maybe ModIface         -- Old interface from compilation manager, if any
  -> IO (RecompileRequired, Maybe ModIface)

checkOldIface hsc_env mod_summary source_modified maybe_iface
  = do  let dflags = hsc_dflags hsc_env
        showPass dflags $
            "Checking old interface for " ++
              (showPpr dflags $ ms_mod mod_summary)
        initIfaceCheck hsc_env $
            check_old_iface hsc_env mod_summary source_modified maybe_iface

check_old_iface
  :: HscEnv
  -> ModSummary
  -> SourceModified
  -> Maybe ModIface
  -> IfG (RecompileRequired, Maybe ModIface)

check_old_iface hsc_env mod_summary src_modified maybe_iface
  = let dflags = hsc_dflags hsc_env
        getIface =
            case maybe_iface of
                Just _  -> do
                    traceIf (text "We already have the old interface for" <+>
                      ppr (ms_mod mod_summary))
                    return maybe_iface
                Nothing -> loadIface

        loadIface = do
             let iface_path = msHiFilePath mod_summary
             read_result <- readIface (ms_mod mod_summary) iface_path
             case read_result of
                 Failed err -> do
                     traceIf (text "FYI: cannot read old interface file:" $$ nest 4 err)
                     return Nothing
                 Succeeded iface -> do
                     traceIf (text "Read the interface file" <+> text iface_path)
                     return $ Just iface

        src_changed
            | gopt Opt_ForceRecomp (hsc_dflags hsc_env) = True
            | SourceModified <- src_modified = True
            | otherwise = False
    in do
        when src_changed $
            traceHiDiffs (nest 4 $ text "Source file changed or recompilation check turned off")

        case src_changed of
            -- If the source has changed and we're in interactive mode,
            -- avoid reading an interface; just return the one we might
            -- have been supplied with.
            True | not (isObjectTarget $ hscTarget dflags) ->
                return (MustCompile, maybe_iface)

            -- Try and read the old interface for the current module
            -- from the .hi file left from the last time we compiled it
            True -> do
                maybe_iface' <- getIface
                return (MustCompile, maybe_iface')

            False -> do
                maybe_iface' <- getIface
                case maybe_iface' of
                    -- We can't retrieve the iface
                    Nothing    -> return (MustCompile, Nothing)

                    -- We have got the old iface; check its versions
                    -- even in the SourceUnmodifiedAndStable case we
                    -- should check versions because some packages
                    -- might have changed or gone away.
                    Just iface -> checkVersions hsc_env mod_summary iface

-- | Check if a module is still the same 'version'.
--
-- This function is called in the recompilation checker after we have
-- determined that the module M being checked hasn't had any changes
-- to its source file since we last compiled M. So at this point in general
-- two things may have changed that mean we should recompile M:
--   * The interface export by a dependency of M has changed.
--   * The compiler flags specified this time for M have changed
--     in a manner that is significant for recompilaiton.
-- We return not just if we should recompile the object file but also
-- if we should rebuild the interface file.
checkVersions :: HscEnv
              -> ModSummary
              -> ModIface       -- Old interface
              -> IfG (RecompileRequired, Maybe ModIface)
checkVersions hsc_env mod_summary iface
  = do { traceHiDiffs (text "Considering whether compilation is required for" <+>
                        ppr (mi_module iface) <> colon)

       ; recomp <- checkFlagHash hsc_env iface
       ; if recompileRequired recomp then return (recomp, Nothing) else do {
       ; recomp <- checkDependencies hsc_env mod_summary iface
       ; if recompileRequired recomp then return (recomp, Just iface) else do {

       -- Source code unchanged and no errors yet... carry on
       --
       -- First put the dependent-module info, read from the old
       -- interface, into the envt, so that when we look for
       -- interfaces we look for the right one (.hi or .hi-boot)
       --
       -- It's just temporary because either the usage check will succeed
       -- (in which case we are done with this module) or it'll fail (in which
       -- case we'll compile the module from scratch anyhow).
       --
       -- We do this regardless of compilation mode, although in --make mode
       -- all the dependent modules should be in the HPT already, so it's
       -- quite redundant
       ; updateEps_ $ \eps  -> eps { eps_is_boot = mod_deps }
       ; recomp <- checkList [checkModUsage this_pkg u | u <- mi_usages iface]
       ; return (recomp, Just iface)
    }}}
  where
    this_pkg = thisPackage (hsc_dflags hsc_env)
    -- This is a bit of a hack really
    mod_deps :: ModuleNameEnv (ModuleName, IsBootInterface)
    mod_deps = mkModDeps (dep_mods (mi_deps iface))

-- | Check the flags haven't changed
checkFlagHash :: HscEnv -> ModIface -> IfG RecompileRequired
checkFlagHash hsc_env iface = do
    let old_hash = mi_flag_hash iface
    new_hash <- liftIO $ fingerprintDynFlags (hsc_dflags hsc_env)
                                             (mi_module iface)
                                             putNameLiterally
    case old_hash == new_hash of
        True  -> up_to_date (ptext $ sLit "Module flags unchanged")
        False -> out_of_date_hash "flags changed"
                     (ptext $ sLit "  Module flags have changed")
                     old_hash new_hash

-- If the direct imports of this module are resolved to targets that
-- are not among the dependencies of the previous interface file,
-- then we definitely need to recompile.  This catches cases like
--   - an exposed package has been upgraded
--   - we are compiling with different package flags
--   - a home module that was shadowing a package module has been removed
--   - a new home module has been added that shadows a package module
-- See bug #1372.
--
-- Returns True if recompilation is required.
checkDependencies :: HscEnv -> ModSummary -> ModIface -> IfG RecompileRequired
checkDependencies hsc_env summary iface
 = checkList (map dep_missing (ms_imps summary ++ ms_srcimps summary))
  where
   prev_dep_mods = dep_mods (mi_deps iface)
   prev_dep_pkgs = dep_pkgs (mi_deps iface)

   this_pkg = thisPackage (hsc_dflags hsc_env)

   dep_missing (L _ (ImportDecl { ideclName = L _ mod, ideclPkgQual = pkg })) = do
     find_res <- liftIO $ findImportedModule hsc_env mod pkg
     let reason = moduleNameString mod ++ " changed"
     case find_res of
        Found _ mod
          | pkg == this_pkg
           -> if moduleName mod `notElem` map fst prev_dep_mods
                 then do traceHiDiffs $
                           text "imported module " <> quotes (ppr mod) <>
                           text " not among previous dependencies"
                         return (RecompBecause reason)
                 else
                         return UpToDate
          | otherwise
           -> if pkg `notElem` (map fst prev_dep_pkgs)
                 then do traceHiDiffs $
                           text "imported module " <> quotes (ppr mod) <>
                           text " is from package " <> quotes (ppr pkg) <>
                           text ", which is not among previous dependencies"
                         return (RecompBecause reason)
                 else
                         return UpToDate
           where pkg = modulePackageId mod
        _otherwise  -> return (RecompBecause reason)

needInterface :: Module -> (ModIface -> IfG RecompileRequired)
              -> IfG RecompileRequired
needInterface mod continue
  = do  -- Load the imported interface if possible
    let doc_str = sep [ptext (sLit "need version info for"), ppr mod]
    traceHiDiffs (text "Checking usages for module" <+> ppr mod)

    mb_iface <- loadInterface doc_str mod ImportBySystem
        -- Load the interface, but don't complain on failure;
        -- Instead, get an Either back which we can test

    case mb_iface of
      Failed _ -> do
        traceHiDiffs (sep [ptext (sLit "Couldn't load interface for module"),
                           ppr mod])
        return MustCompile
                  -- Couldn't find or parse a module mentioned in the
                  -- old interface file.  Don't complain: it might
                  -- just be that the current module doesn't need that
                  -- import and it's been deleted
      Succeeded iface -> continue iface


-- | Given the usage information extracted from the old
-- M.hi file for the module being compiled, figure out
-- whether M needs to be recompiled.
checkModUsage :: PackageId -> Usage -> IfG RecompileRequired
checkModUsage _this_pkg UsagePackageModule{
                                usg_mod = mod,
                                usg_mod_hash = old_mod_hash }
  = needInterface mod $ \iface -> do
    let reason = moduleNameString (moduleName mod) ++ " changed"
    checkModuleFingerprint reason old_mod_hash (mi_mod_hash iface)
        -- We only track the ABI hash of package modules, rather than
        -- individual entity usages, so if the ABI hash changes we must
        -- recompile.  This is safe but may entail more recompilation when
        -- a dependent package has changed.

checkModUsage this_pkg UsageHomeModule{
                                usg_mod_name = mod_name,
                                usg_mod_hash = old_mod_hash,
                                usg_exports = maybe_old_export_hash,
                                usg_entities = old_decl_hash }
  = do
    let mod = mkModule this_pkg mod_name
    needInterface mod $ \iface -> do

    let
        new_mod_hash    = mi_mod_hash    iface
        new_decl_hash   = mi_hash_fn     iface
        new_export_hash = mi_exp_hash    iface

        reason = moduleNameString mod_name ++ " changed"

        -- CHECK MODULE
    recompile <- checkModuleFingerprint reason old_mod_hash new_mod_hash
    if not (recompileRequired recompile)
      then return UpToDate
      else do

        -- CHECK EXPORT LIST
        checkMaybeHash reason maybe_old_export_hash new_export_hash
            (ptext (sLit "  Export list changed")) $ do

        -- CHECK ITEMS ONE BY ONE
        recompile <- checkList [ checkEntityUsage reason new_decl_hash u
                               | u <- old_decl_hash]
        if recompileRequired recompile
          then return recompile     -- This one failed, so just bail out now
          else up_to_date (ptext (sLit "  Great!  The bits I use are up to date"))


checkModUsage _this_pkg UsageFile{ usg_file_path = file,
                                   usg_file_hash = old_hash } =
  liftIO $
    handleIO handle $ do
      new_hash <- getFileHash file
      if (old_hash /= new_hash)
         then return recomp
         else return UpToDate
 where
   recomp = RecompBecause (file ++ " changed")
   handle =
#ifdef DEBUG
       \e -> pprTrace "UsageFile" (text (show e)) $ return recomp
#else
       \_ -> return recomp -- if we can't find the file, just recompile, don't fail
#endif

------------------------
checkModuleFingerprint :: String -> Fingerprint -> Fingerprint
                       -> IfG RecompileRequired
checkModuleFingerprint reason old_mod_hash new_mod_hash
  | new_mod_hash == old_mod_hash
  = up_to_date (ptext (sLit "Module fingerprint unchanged"))

  | otherwise
  = out_of_date_hash reason (ptext (sLit "  Module fingerprint has changed"))
                     old_mod_hash new_mod_hash

------------------------
checkMaybeHash :: String -> Maybe Fingerprint -> Fingerprint -> SDoc
               -> IfG RecompileRequired -> IfG RecompileRequired
checkMaybeHash reason maybe_old_hash new_hash doc continue
  | Just hash <- maybe_old_hash, hash /= new_hash
  = out_of_date_hash reason doc hash new_hash
  | otherwise
  = continue

------------------------
checkEntityUsage :: String
                 -> (OccName -> Maybe (OccName, Fingerprint))
                 -> (OccName, Fingerprint)
                 -> IfG RecompileRequired
checkEntityUsage reason new_hash (name,old_hash)
  = case new_hash name of

        Nothing       ->        -- We used it before, but it ain't there now
                          out_of_date reason (sep [ptext (sLit "No longer exported:"), ppr name])

        Just (_, new_hash)      -- It's there, but is it up to date?
          | new_hash == old_hash -> do traceHiDiffs (text "  Up to date" <+> ppr name <+> parens (ppr new_hash))
                                       return UpToDate
          | otherwise            -> out_of_date_hash reason (ptext (sLit "  Out of date:") <+> ppr name)
                                                     old_hash new_hash

up_to_date :: SDoc -> IfG RecompileRequired
up_to_date  msg = traceHiDiffs msg >> return UpToDate

out_of_date :: String -> SDoc -> IfG RecompileRequired
out_of_date reason msg = traceHiDiffs msg >> return (RecompBecause reason)

out_of_date_hash :: String -> SDoc -> Fingerprint -> Fingerprint -> IfG RecompileRequired
out_of_date_hash reason msg old_hash new_hash
  = out_of_date reason (hsep [msg, ppr old_hash, ptext (sLit "->"), ppr new_hash])

----------------------
checkList :: [IfG RecompileRequired] -> IfG RecompileRequired
-- This helper is used in two places
checkList []             = return UpToDate
checkList (check:checks) = do recompile <- check
                              if recompileRequired recompile
                                then return recompile
                                else checkList checks
\end{code}

%************************************************************************
%*                                                                      *
                Converting things to their Iface equivalents
%*                                                                      *
%************************************************************************

\begin{code}
tyThingToIfaceDecl :: TyThing -> IfaceDecl
tyThingToIfaceDecl (AnId id)      = idToIfaceDecl id
tyThingToIfaceDecl (ATyCon tycon) = tyConToIfaceDecl emptyTidyEnv tycon
tyThingToIfaceDecl (ACoAxiom ax)  = coAxiomToIfaceDecl ax
tyThingToIfaceDecl (AConLike cl)  = case cl of
    RealDataCon dc -> dataConToIfaceDecl dc -- for ppr purposes only
    PatSynCon ps   -> patSynToIfaceDecl ps

--------------------------
idToIfaceDecl :: Id -> IfaceDecl
-- The Id is already tidied, so that locally-bound names
-- (lambdas, for-alls) already have non-clashing OccNames
-- We can't tidy it here, locally, because it may have
-- free variables in its type or IdInfo
idToIfaceDecl id
  = IfaceId { ifName      = getOccName id,
              ifType      = toIfaceType (idType id),
              ifIdDetails = toIfaceIdDetails (idDetails id),
              ifIdInfo    = toIfaceIdInfo (idInfo id) }

--------------------------
dataConToIfaceDecl :: DataCon -> IfaceDecl
dataConToIfaceDecl dataCon
  = IfaceId { ifName      = getOccName dataCon,
              ifType      = toIfaceType (dataConUserType dataCon),
              ifIdDetails = IfVanillaId,
              ifIdInfo    = NoInfo }

--------------------------
patSynToIfaceDecl :: PatSyn -> IfaceDecl
patSynToIfaceDecl ps
  = IfacePatSyn { ifName          = getOccName . getName $ ps
                , ifPatMatcher    = matcher
                , ifPatWrapper    = wrapper
                , ifPatIsInfix    = patSynIsInfix ps
                , ifPatUnivTvs    = toIfaceTvBndrs univ_tvs'
                , ifPatExTvs      = toIfaceTvBndrs ex_tvs'
                , ifPatProvCtxt   = tidyToIfaceContext env2 prov_theta
                , ifPatReqCtxt    = tidyToIfaceContext env2 req_theta
                , ifPatArgs       = map (tidyToIfaceType env2) args
                , ifPatTy         = tidyToIfaceType env2 rhs_ty
                }
  where
    (univ_tvs, ex_tvs, prov_theta, req_theta, args, rhs_ty) = patSynSig ps
    (env1, univ_tvs') = tidyTyVarBndrs emptyTidyEnv univ_tvs
    (env2, ex_tvs')   = tidyTyVarBndrs env1 ex_tvs

    matcher = idName (patSynMatcher ps)
    wrapper = fmap idName (patSynWrapper ps)


--------------------------
coAxiomToIfaceDecl :: CoAxiom br -> IfaceDecl
-- We *do* tidy Axioms, because they are not (and cannot
-- conveniently be) built in tidy form
coAxiomToIfaceDecl ax@(CoAxiom { co_ax_tc = tycon, co_ax_branches = branches
                               , co_ax_role = role })
 = IfaceAxiom { ifName       = name
              , ifTyCon      = toIfaceTyCon tycon
              , ifRole       = role
              , ifAxBranches = brListMap (coAxBranchToIfaceBranch
                                            (brListMap coAxBranchLHS branches))
                                         branches }
 where
   name = getOccName ax

-- 2nd parameter is the list of branch LHSs, for conversion from incompatible branches
-- to incompatible indices
-- See Note [Storing compatibility] in CoAxiom
coAxBranchToIfaceBranch :: [[Type]] -> CoAxBranch -> IfaceAxBranch
coAxBranchToIfaceBranch lhs_s
                        branch@(CoAxBranch { cab_incomps = incomps })
  = (coAxBranchToIfaceBranch' branch) { ifaxbIncomps = iface_incomps }
  where
    iface_incomps = map (expectJust "iface_incomps"
                        . (flip findIndex lhs_s
                          . eqTypes)
                        . coAxBranchLHS) incomps

-- use this one for standalone branches without incompatibles
coAxBranchToIfaceBranch' :: CoAxBranch -> IfaceAxBranch
coAxBranchToIfaceBranch' (CoAxBranch { cab_tvs = tvs, cab_lhs = lhs
                                     , cab_roles = roles, cab_rhs = rhs })
  = IfaceAxBranch { ifaxbTyVars = toIfaceTvBndrs tv_bndrs
                  , ifaxbLHS    = tidyToIfaceTcArgs env1 lhs
                  , ifaxbRoles  = roles
                  , ifaxbRHS    = tidyToIfaceType env1 rhs
                  , ifaxbIncomps = [] }
  where
    (env1, tv_bndrs) = tidyTyClTyVarBndrs emptyTidyEnv tvs
    -- Don't re-bind in-scope tyvars
    -- See Note [CoAxBranch type variables] in CoAxiom

-----------------
tyConToIfaceDecl :: TidyEnv -> TyCon -> IfaceDecl
-- We *do* tidy TyCons, because they are not (and cannot
-- conveniently be) built in tidy form
tyConToIfaceDecl env tycon
  | Just clas <- tyConClass_maybe tycon
  = classToIfaceDecl env clas

  | Just syn_rhs <- synTyConRhs_maybe tycon
  = IfaceSyn {  ifName    = getOccName tycon,
                ifTyVars  = toIfaceTvBndrs tyvars,
                ifRoles   = tyConRoles tycon,
                ifSynRhs  = to_ifsyn_rhs syn_rhs,
                ifSynKind = tidyToIfaceType env1 (synTyConResKind tycon) }

  | isAlgTyCon tycon
  = IfaceData { ifName    = getOccName tycon,
                ifCType   = tyConCType tycon,
                ifTyVars  = toIfaceTvBndrs tyvars,
                ifRoles   = tyConRoles tycon,
                ifCtxt    = tidyToIfaceContext env1 (tyConStupidTheta tycon),
                ifCons    = ifaceConDecls (algTyConRhs tycon),
                ifRec     = boolToRecFlag (isRecursiveTyCon tycon),
                ifGadtSyntax = isGadtSyntaxTyCon tycon,
                ifPromotable = isJust (promotableTyCon_maybe tycon),
                ifParent  = parent }

  | isForeignTyCon tycon
  = IfaceForeign { ifName    = getOccName tycon,
                   ifExtName = tyConExtName tycon }

  | otherwise
  -- For pretty printing purposes only.
  = IfaceData { ifName       = getOccName tycon,
                ifCType      = Nothing,
                ifTyVars     = funAndPrimTyVars,
                ifRoles      = tyConRoles tycon,
                ifCtxt       = [],
                ifCons       = IfDataTyCon [],
                ifRec        = boolToRecFlag False,
                ifGadtSyntax = False,
                ifPromotable = False,
                ifParent     = IfNoParent }
  where
    (env1, tyvars) = tidyTyClTyVarBndrs env (tyConTyVars tycon)

    funAndPrimTyVars = toIfaceTvBndrs $ take (tyConArity tycon) alphaTyVars

    parent = case tyConFamInstSig_maybe tycon of
               Just (tc, ty, ax) -> IfDataInstance (coAxiomName ax)
                                                   (toIfaceTyCon tc)
                                                   (toIfaceTcArgs tc ty)
               Nothing           -> IfNoParent

    to_ifsyn_rhs OpenSynFamilyTyCon        = IfaceOpenSynFamilyTyCon
    to_ifsyn_rhs (ClosedSynFamilyTyCon ax) = IfaceClosedSynFamilyTyCon axn ibr
      where defs = fromBranchList $ coAxiomBranches ax
            ibr  = map coAxBranchToIfaceBranch' defs
            axn  = coAxiomName ax
    to_ifsyn_rhs AbstractClosedSynFamilyTyCon
      = IfaceAbstractClosedSynFamilyTyCon

    to_ifsyn_rhs (SynonymTyCon ty)
      = IfaceSynonymTyCon (tidyToIfaceType env1 ty)

    to_ifsyn_rhs (BuiltInSynFamTyCon {})
      = pprPanic "toIfaceDecl: BuiltInFamTyCon" (ppr tycon)


    ifaceConDecls (NewTyCon { data_con = con })     = IfNewTyCon  (ifaceConDecl con)
    ifaceConDecls (DataTyCon { data_cons = cons })  = IfDataTyCon (map ifaceConDecl cons)
    ifaceConDecls (DataFamilyTyCon {})              = IfDataFamTyCon
    ifaceConDecls (AbstractTyCon distinct)          = IfAbstractTyCon distinct
        -- The last case happens when a TyCon has been trimmed during tidying
        -- Furthermore, tyThingToIfaceDecl is also used
        -- in TcRnDriver for GHCi, when browsing a module, in which case the
        -- AbstractTyCon case is perfectly sensible.

    ifaceConDecl data_con
        = IfCon   { ifConOcc     = getOccName (dataConName data_con),
                    ifConInfix   = dataConIsInfix data_con,
                    ifConWrapper = isJust (dataConWrapId_maybe data_con),
                    ifConUnivTvs = toIfaceTvBndrs univ_tvs',
                    ifConExTvs   = toIfaceTvBndrs ex_tvs',
                    ifConEqSpec  = map to_eq_spec eq_spec,
                    ifConCtxt    = tidyToIfaceContext env2 theta,
                    ifConArgTys  = map (tidyToIfaceType env2) arg_tys,
                    ifConFields  = map getOccName
                                       (dataConFieldLabels data_con),
                    ifConStricts = map (toIfaceBang env2) (dataConRepBangs data_con) }
        where
          (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _) = dataConFullSig data_con

          -- Start with 'emptyTidyEnv' not 'env1', because the type of the
          -- data constructor is fully standalone
          (env1, univ_tvs') = tidyTyVarBndrs emptyTidyEnv univ_tvs
          (env2, ex_tvs')   = tidyTyVarBndrs env1 ex_tvs
          to_eq_spec (tv,ty) = (toIfaceTyVar (tidyTyVar env2 tv), tidyToIfaceType env2 ty)

toIfaceBang :: TidyEnv -> HsBang -> IfaceBang
toIfaceBang _    HsNoBang            = IfNoBang
toIfaceBang _   (HsUnpack Nothing)   = IfUnpack
toIfaceBang env (HsUnpack (Just co)) = IfUnpackCo (toIfaceCoercion (tidyCo env co))
toIfaceBang _   HsStrict             = IfStrict
toIfaceBang _   (HsUserBang {})      = panic "toIfaceBang"

classToIfaceDecl :: TidyEnv -> Class -> IfaceDecl
classToIfaceDecl env clas
  = IfaceClass { ifCtxt   = tidyToIfaceContext env1 sc_theta,
                 ifName   = getOccName (classTyCon clas),
                 ifTyVars = toIfaceTvBndrs clas_tyvars',
                 ifRoles  = tyConRoles (classTyCon clas),
                 ifFDs    = map toIfaceFD clas_fds,
                 ifATs    = map toIfaceAT clas_ats,
                 ifSigs   = map toIfaceClassOp op_stuff,
                 ifMinDef = fmap getOccName (classMinimalDef clas),
                 ifRec    = boolToRecFlag (isRecursiveTyCon tycon) }
  where
    (clas_tyvars, clas_fds, sc_theta, _, clas_ats, op_stuff)
      = classExtraBigSig clas
    tycon = classTyCon clas

    (env1, clas_tyvars') = tidyTyVarBndrs env clas_tyvars

    toIfaceAT :: ClassATItem -> IfaceAT
    toIfaceAT (tc, defs)
      = IfaceAT (tyConToIfaceDecl env1 tc) (map coAxBranchToIfaceBranch' defs)

    toIfaceClassOp (sel_id, def_meth)
        = ASSERT(sel_tyvars == clas_tyvars)
          IfaceClassOp (getOccName sel_id) (toDmSpec def_meth)
                       (tidyToIfaceType env1 op_ty)
        where
                -- Be careful when splitting the type, because of things
                -- like         class Foo a where
                --                op :: (?x :: String) => a -> a
                -- and          class Baz a where
                --                op :: (Ord a) => a -> a
          (sel_tyvars, rho_ty) = splitForAllTys (idType sel_id)
          op_ty                = funResultTy rho_ty

    toDmSpec NoDefMeth      = NoDM
    toDmSpec (GenDefMeth _) = GenericDM
    toDmSpec (DefMeth _)    = VanillaDM

    toIfaceFD (tvs1, tvs2) = (map (getFS . tidyTyVar env1) tvs1,
                              map (getFS . tidyTyVar env1) tvs2)

--------------------------
tidyToIfaceType :: TidyEnv -> Type -> IfaceType
tidyToIfaceType env ty = toIfaceType (tidyType env ty)

tidyToIfaceTcArgs :: TidyEnv -> [Type] -> IfaceTcArgs
tidyToIfaceTcArgs _ [] = ITC_Nil
tidyToIfaceTcArgs env (t:ts)
  | isKind t  = ITC_Kind  (tidyToIfaceType env t) (tidyToIfaceTcArgs env ts)
  | otherwise = ITC_Type  (tidyToIfaceType env t) (tidyToIfaceTcArgs env ts)

tidyToIfaceContext :: TidyEnv -> ThetaType -> IfaceContext
tidyToIfaceContext env theta = map (tidyToIfaceType env) theta

tidyTyClTyVarBndrs :: TidyEnv -> [TyVar] -> (TidyEnv, [TyVar])
tidyTyClTyVarBndrs env tvs = mapAccumL tidyTyClTyVarBndr env tvs

tidyTyClTyVarBndr :: TidyEnv -> TyVar -> (TidyEnv, TyVar)
-- If the type variable "binder" is in scope, don't re-bind it
-- In a class decl, for example, the ATD binders mention
-- (amd must mention) the class tyvars
tidyTyClTyVarBndr env@(_, subst) tv
 | Just tv' <- lookupVarEnv subst tv = (env, tv')
 | otherwise                         = tidyTyVarBndr env tv

tidyTyVar :: TidyEnv -> TyVar -> TyVar
tidyTyVar (_, subst) tv = lookupVarEnv subst tv `orElse` tv
   -- TcType.tidyTyVarOcc messes around with FlatSkols

getFS :: NamedThing a => a -> FastString
getFS x = occNameFS (getOccName x)

--------------------------
instanceToIfaceInst :: ClsInst -> IfaceClsInst
instanceToIfaceInst (ClsInst { is_dfun = dfun_id, is_flag = oflag
                             , is_cls_nm = cls_name, is_cls = cls
                             , is_tys = tys, is_tcs = mb_tcs })
  = ASSERT( cls_name == className cls )
    IfaceClsInst { ifDFun    = dfun_name,
                ifOFlag   = oflag,
                ifInstCls = cls_name,
                ifInstTys = map do_rough mb_tcs,
                ifInstOrph = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

    dfun_name = idName dfun_id
    mod       = ASSERT( isExternalName dfun_name ) nameModule dfun_name
    is_local name = nameIsLocalOrFrom mod name

        -- Compute orphanhood.  See Note [Orphans] in IfaceSyn
    (tvs, fds) = classTvsFds cls
    arg_names = [filterNameSet is_local (orphNamesOfType ty) | ty <- tys]

    -- See Note [When exactly is an instance decl an orphan?] in IfaceSyn
    orph | is_local cls_name = Just (nameOccName cls_name)
         | all isJust mb_ns  = ASSERT( not (null mb_ns) ) head mb_ns
         | otherwise         = Nothing

    mb_ns :: [Maybe OccName]    -- One for each fundep; a locally-defined name
                                -- that is not in the "determined" arguments
    mb_ns | null fds   = [choose_one arg_names]
          | otherwise  = map do_one fds
    do_one (_ltvs, rtvs) = choose_one [ns | (tv,ns) <- tvs `zip` arg_names
                                          , not (tv `elem` rtvs)]

    choose_one :: [NameSet] -> Maybe OccName
    choose_one nss = case nameSetToList (unionManyNameSets nss) of
                        []      -> Nothing
                        (n : _) -> Just (nameOccName n)

--------------------------
famInstToIfaceFamInst :: FamInst -> IfaceFamInst
famInstToIfaceFamInst (FamInst { fi_axiom    = axiom,
                                 fi_fam      = fam,
                                 fi_tcs      = roughs })
  = IfaceFamInst { ifFamInstAxiom    = coAxiomName axiom
                 , ifFamInstFam      = fam
                 , ifFamInstTys      = map do_rough roughs
                 , ifFamInstOrph     = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

    fam_decl = tyConName $ coAxiomTyCon axiom
    mod = ASSERT( isExternalName (coAxiomName axiom) )
          nameModule (coAxiomName axiom)
    is_local name = nameIsLocalOrFrom mod name

    lhs_names = filterNameSet is_local (orphNamesOfCoCon axiom)

    orph | is_local fam_decl
         = Just (nameOccName fam_decl)

         | not (isEmptyNameSet lhs_names)
         = Just (nameOccName (head (nameSetToList lhs_names)))


         | otherwise
         = Nothing

--------------------------
toIfaceLetBndr :: Id -> IfaceLetBndr
toIfaceLetBndr id  = IfLetBndr (occNameFS (getOccName id))
                               (toIfaceType (idType id))
                               (toIfaceIdInfo (idInfo id))
  -- Put into the interface file any IdInfo that CoreTidy.tidyLetBndr
  -- has left on the Id.  See Note [IdInfo on nested let-bindings] in IfaceSyn

--------------------------
toIfaceIdDetails :: IdDetails -> IfaceIdDetails
toIfaceIdDetails VanillaId                      = IfVanillaId
toIfaceIdDetails (DFunId ns _)                  = IfDFunId ns
toIfaceIdDetails (RecSelId { sel_naughty = n
                           , sel_tycon = tc })  = IfRecSelId (toIfaceTyCon tc) n
toIfaceIdDetails other                          = pprTrace "toIfaceIdDetails" (ppr other)
                                                  IfVanillaId   -- Unexpected

toIfaceIdInfo :: IdInfo -> IfaceIdInfo
toIfaceIdInfo id_info
  = case catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo,
                    inline_hsinfo,  unfold_hsinfo] of
       []    -> NoInfo
       infos -> HasInfo infos
               -- NB: strictness and arity must appear in the list before unfolding
               -- See TcIface.tcUnfolding
  where
    ------------  Arity  --------------
    arity_info = arityInfo id_info
    arity_hsinfo | arity_info == 0 = Nothing
                 | otherwise       = Just (HsArity arity_info)

    ------------ Caf Info --------------
    caf_info   = cafInfo id_info
    caf_hsinfo = case caf_info of
                   NoCafRefs -> Just HsNoCafRefs
                   _other    -> Nothing

    ------------  Strictness  --------------
        -- No point in explicitly exporting TopSig
    sig_info = strictnessInfo id_info
    strict_hsinfo | not (isNopSig sig_info) = Just (HsStrictness sig_info)
                  | otherwise               = Nothing

    ------------  Unfolding  --------------
    unfold_hsinfo = toIfUnfolding loop_breaker (unfoldingInfo id_info)
    loop_breaker  = isStrongLoopBreaker (occInfo id_info)

    ------------  Inline prag  --------------
    inline_prag = inlinePragInfo id_info
    inline_hsinfo | isDefaultInlinePragma inline_prag = Nothing
                  | otherwise = Just (HsInline inline_prag)

--------------------------
toIfUnfolding :: Bool -> Unfolding -> Maybe IfaceInfoItem
toIfUnfolding lb (CoreUnfolding { uf_tmpl = rhs, uf_arity = arity
                                , uf_src = src, uf_guidance = guidance })
  = Just $ HsUnfold lb $
    case src of
        InlineStable
          -> case guidance of
               UnfWhen unsat_ok boring_ok -> IfInlineRule arity unsat_ok boring_ok if_rhs
               _other                     -> IfCoreUnfold True if_rhs
        InlineCompulsory -> IfCompulsory if_rhs
        InlineRhs        -> IfCoreUnfold False if_rhs
        -- Yes, even if guidance is UnfNever, expose the unfolding
        -- If we didn't want to expose the unfolding, TidyPgm would
        -- have stuck in NoUnfolding.  For supercompilation we want
        -- to see that unfolding!
  where
    if_rhs = toIfaceExpr rhs

toIfUnfolding lb (DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = Just (HsUnfold lb (IfDFunUnfold (map toIfaceBndr bndrs) (map toIfaceExpr args)))
      -- No need to serialise the data constructor;
      -- we can recover it from the type of the dfun

toIfUnfolding _ _
  = Nothing

--------------------------
coreRuleToIfaceRule :: Module -> CoreRule -> IfaceRule
coreRuleToIfaceRule _ (BuiltinRule { ru_fn = fn})
  = pprTrace "toHsRule: builtin" (ppr fn) $
    bogusIfaceRule fn

coreRuleToIfaceRule mod rule@(Rule { ru_name = name, ru_fn = fn,
                                     ru_act = act, ru_bndrs = bndrs,
                                     ru_args = args, ru_rhs = rhs,
                                     ru_auto = auto })
  = IfaceRule { ifRuleName  = name, ifActivation = act,
                ifRuleBndrs = map toIfaceBndr bndrs,
                ifRuleHead  = fn,
                ifRuleArgs  = map do_arg args,
                ifRuleRhs   = toIfaceExpr rhs,
                ifRuleAuto  = auto,
                ifRuleOrph  = orph }
  where
        -- For type args we must remove synonyms from the outermost
        -- level.  Reason: so that when we read it back in we'll
        -- construct the same ru_rough field as we have right now;
        -- see tcIfaceRule
    do_arg (Type ty)     = IfaceType (toIfaceType (deNoteType ty))
    do_arg (Coercion co) = IfaceCo   (toIfaceCoercion co)
    do_arg arg           = toIfaceExpr arg

        -- Compute orphanhood.  See Note [Orphans] in IfaceSyn
        -- A rule is an orphan only if none of the variables
        -- mentioned on its left-hand side are locally defined
    lhs_names = nameSetToList (ruleLhsOrphNames rule)

    orph = case filter (nameIsLocalOrFrom mod) lhs_names of
                        (n : _) -> Just (nameOccName n)
                        []      -> Nothing

bogusIfaceRule :: Name -> IfaceRule
bogusIfaceRule id_name
  = IfaceRule { ifRuleName = fsLit "bogus", ifActivation = NeverActive,
        ifRuleBndrs = [], ifRuleHead = id_name, ifRuleArgs = [],
        ifRuleRhs = IfaceExt id_name, ifRuleOrph = Nothing, ifRuleAuto = True }

---------------------
toIfaceExpr :: CoreExpr -> IfaceExpr
toIfaceExpr (Var v)         = toIfaceVar v
toIfaceExpr (Lit l)         = IfaceLit l
toIfaceExpr (Type ty)       = IfaceType (toIfaceType ty)
toIfaceExpr (Coercion co)   = IfaceCo   (toIfaceCoercion co)
toIfaceExpr (Lam x b)       = IfaceLam (toIfaceBndr x) (toIfaceExpr b)
toIfaceExpr (App f a)       = toIfaceApp f [a]
toIfaceExpr (Case s x ty as)
  | null as                 = IfaceECase (toIfaceExpr s) (toIfaceType ty)
  | otherwise               = IfaceCase (toIfaceExpr s) (getFS x) (map toIfaceAlt as)
toIfaceExpr (Let b e)       = IfaceLet (toIfaceBind b) (toIfaceExpr e)
toIfaceExpr (Cast e co)     = IfaceCast (toIfaceExpr e) (toIfaceCoercion co)
toIfaceExpr (Tick t e) 
  | Just t' <- toIfaceTickish t = IfaceTick t' (toIfaceExpr e) 
  | otherwise                   = toIfaceExpr e

---------------------
toIfaceTickish :: Tickish Id -> Maybe IfaceTickish
toIfaceTickish (ProfNote cc tick push) = Just (IfaceSCC cc tick push)
toIfaceTickish (HpcTick modl ix)       = Just (IfaceHpcTick modl ix)
toIfaceTickish (Breakpoint {})         = Nothing 
   -- Ignore breakpoints, since they are relevant only to GHCi, and 
   -- should not be serialised (Trac #8333)

---------------------
toIfaceBind :: Bind Id -> IfaceBinding
toIfaceBind (NonRec b r) = IfaceNonRec (toIfaceLetBndr b) (toIfaceExpr r)
toIfaceBind (Rec prs)    = IfaceRec [(toIfaceLetBndr b, toIfaceExpr r) | (b,r) <- prs]

---------------------
toIfaceAlt :: (AltCon, [Var], CoreExpr)
           -> (IfaceConAlt, [FastString], IfaceExpr)
toIfaceAlt (c,bs,r) = (toIfaceCon c, map getFS bs, toIfaceExpr r)

---------------------
toIfaceCon :: AltCon -> IfaceConAlt
toIfaceCon (DataAlt dc) = IfaceDataAlt (getName dc)
toIfaceCon (LitAlt l)   = IfaceLitAlt l
toIfaceCon DEFAULT      = IfaceDefault

---------------------
toIfaceApp :: Expr CoreBndr -> [Arg CoreBndr] -> IfaceExpr
toIfaceApp (App f a) as = toIfaceApp f (a:as)
toIfaceApp (Var v) as
  = case isDataConWorkId_maybe v of
        -- We convert the *worker* for tuples into IfaceTuples
        Just dc |  isTupleTyCon tc && saturated
                -> IfaceTuple (tupleTyConSort tc) tup_args
          where
            val_args  = dropWhile isTypeArg as
            saturated = val_args `lengthIs` idArity v
            tup_args  = map toIfaceExpr val_args
            tc        = dataConTyCon dc

        _ -> mkIfaceApps (toIfaceVar v) as

toIfaceApp e as = mkIfaceApps (toIfaceExpr e) as

mkIfaceApps :: IfaceExpr -> [CoreExpr] -> IfaceExpr
mkIfaceApps f as = foldl (\f a -> IfaceApp f (toIfaceExpr a)) f as

---------------------
toIfaceVar :: Id -> IfaceExpr
toIfaceVar v
    | Just fcall <- isFCallId_maybe v            = IfaceFCall fcall (toIfaceType (idType v))
       -- Foreign calls have special syntax
    | isExternalName name                        = IfaceExt name
    | otherwise                                  = IfaceLcl (getFS name)
  where name = idName v
\end{code}
