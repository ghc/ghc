%
% (c) The University of Glasgow 2006-2008
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module MkIface ( 
        mkUsedNames,
        mkDependencies,
        mkIface,        -- Build a ModIface from a ModGuts, 
                        -- including computing version information

        mkIfaceTc,

        writeIfaceFile, -- Write the interface file

        checkOldIface,  -- See if recompilation is required, by
                        -- comparing version information

        tyThingToIfaceDecl -- Converting things to their Iface equivalents
 ) where
\end{code}

  -----------------------------------------------
          Recompilation checking
  -----------------------------------------------

A complete description of how recompilation checking works can be
found in the wiki commentary:

 http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance

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

  * In checkOldIface we compare the mi_usages for the module with
    the actual fingerprint for all each thing recorded in mi_usages

\begin{code}
#include "HsVersions.h"

import IfaceSyn
import LoadIface
import Id
import IdInfo
import Demand
import Annotations
import CoreSyn
import CoreFVs
import Class
import Kind
import TyCon
import DataCon
import Type
import TcType
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

import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
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
         ModGuts{     mg_module     = this_mod,
                      mg_boot       = is_boot,
                      mg_used_names = used_names,
                      mg_deps       = deps,
                      mg_dir_imps   = dir_imp_mods,
                      mg_rdr_env    = rdr_env,
                      mg_fix_env    = fix_env,
                      mg_warns      = warns,
                      mg_hpc_info   = hpc_info,
                      mg_trust_pkg  = self_trust }
        = mkIface_ hsc_env maybe_old_fingerprint
                   this_mod is_boot used_names deps rdr_env fix_env
                   warns hpc_info dir_imp_mods self_trust mod_details

-- | make an interface from the results of typechecking only.  Useful
-- for non-optimising compilation, or where we aren't generating any
-- object code at all ('HscNothing').
mkIfaceTc :: HscEnv
          -> Maybe Fingerprint	-- The old fingerprint, if we have it
          -> ModDetails		-- gotten from mkBootModDetails, probably
          -> TcGblEnv		-- Usages, deprecations, etc
	  -> IO (Messages, Maybe (ModIface, Bool))
mkIfaceTc hsc_env maybe_old_fingerprint mod_details
  tc_result@TcGblEnv{ tcg_mod = this_mod,
                      tcg_src = hsc_src,
                      tcg_imports = imports,
                      tcg_rdr_env = rdr_env,
                      tcg_fix_env = fix_env,
                      tcg_warns = warns,
                      tcg_hpc = other_hpc_info
                    }
  = do
          let used_names = mkUsedNames tc_result
          deps <- mkDependencies tc_result
          let hpc_info = emptyHpcInfo other_hpc_info
          mkIface_ hsc_env maybe_old_fingerprint
                   this_mod (isHsBoot hsc_src) used_names deps rdr_env 
                   fix_env warns hpc_info (imp_mods imports)
                   (imp_trust_own_pkg imports) mod_details
        

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
         -> NameSet -> Dependencies -> GlobalRdrEnv
         -> NameEnv FixItem -> Warnings -> HpcInfo
         -> ImportedMods -> Bool
         -> ModDetails
         -> IO (Messages, Maybe (ModIface, Bool))
mkIface_ hsc_env maybe_old_fingerprint 
         this_mod is_boot used_names deps rdr_env fix_env src_warns hpc_info
         dir_imp_mods pkg_trust_req
	 ModDetails{  md_insts 	   = insts, 
		      md_fam_insts = fam_insts,
		      md_rules 	   = rules,
		      md_anns	   = anns,
                      md_vect_info = vect_info,
		      md_types 	   = type_env,
		      md_exports   = exports }
-- NB:	notice that mkIface does not look at the bindings
--	only at the TypeEnv.  The previous Tidy phase has
--	put exactly the info into the TypeEnv that we want
--	to expose in the interface

  = do	{ usages <- mkUsageInfo hsc_env this_mod dir_imp_mods used_names

	; let	{ entities = typeEnvElts type_env ;
                  decls  = [ tyThingToIfaceDecl entity
			   | entity <- entities,
			     let name = getName entity,
                             not (isImplicitTyThing entity),
	                        -- No implicit Ids and class tycons in the interface file
			     not (isWiredInName name),
	                        -- Nor wired-in things; the compiler knows about them anyhow
			     nameIsLocalOrFrom this_mod name  ]
				-- Sigh: see Note [Root-main Id] in TcRnDriver

		; fixities    = [(occ,fix) | FixItem occ fix <- nameEnvElts fix_env]
		; warns       = src_warns
		; iface_rules = map (coreRuleToIfaceRule this_mod) rules
		; iface_insts = map instanceToIfaceInst insts
		; iface_fam_insts = map famInstToIfaceFamInst fam_insts
                ; iface_vect_info = flattenVectInfo vect_info
                ; trust_info  = (setSafeMode . safeHaskell) dflags

	        ; intermediate_iface = ModIface { 
			mi_module   = this_mod,
			mi_boot     = is_boot,
			mi_deps     = deps,
			mi_usages   = usages,
			mi_exports  = mkIfaceExports exports,
	
			-- Sort these lexicographically, so that
			-- the result is stable across compilations
			mi_insts    = sortLe le_inst iface_insts,
			mi_fam_insts= sortLe le_fam_inst iface_fam_insts,
			mi_rules    = sortLe le_rule iface_rules,

                        mi_vect_info = iface_vect_info,

			mi_fixities = fixities,
			mi_warns  = warns,
			mi_anns     = mkIfaceAnnotations anns,
			mi_globals  = Just rdr_env,

			-- Left out deliberately: filled in by addVersionInfo
			mi_iface_hash = fingerprint0,
			mi_mod_hash  = fingerprint0,
 			mi_exp_hash  = fingerprint0,
 			mi_orphan_hash = fingerprint0,
			mi_orphan    = False,	-- Always set by addVersionInfo, but
						-- it's a strict field, so we can't omit it.
                        mi_finsts    = False,   -- Ditto
			mi_decls     = deliberatelyOmitted "decls",
			mi_hash_fn   = deliberatelyOmitted "hash_fn",
			mi_hpc       = isHpcUsed hpc_info,
			mi_trust     = trust_info,
			mi_trust_pkg = pkg_trust_req,

			-- And build the cached values
			mi_warn_fn = mkIfaceWarnCache warns,
			mi_fix_fn = mkIfaceFixCache fixities }
		}

        ; (new_iface, no_change_at_all) 
	        <- {-# SCC "versioninfo" #-}
			 addFingerprints hsc_env maybe_old_fingerprint
                                         intermediate_iface decls

		-- Warn about orphans
	; let warn_orphs      = dopt Opt_WarnOrphans dflags
              warn_auto_orphs = dopt Opt_WarnAutoOrphans dflags
              orph_warnings   --- Laziness means no work done unless -fwarn-orphans
	        | warn_orphs || warn_auto_orphs = rule_warns `unionBags` inst_warns
	        | otherwise 	       	        = emptyBag
	      errs_and_warns = (orph_warnings, emptyBag)
	      unqual = mkPrintUnqualified dflags rdr_env
	      inst_warns = listToBag [ instOrphWarn unqual d 
	      		   	     | (d,i) <- insts `zip` iface_insts
	      		   	     , isNothing (ifInstOrph i) ]
	      rule_warns = listToBag [ ruleOrphWarn unqual this_mod r 
	      		    	     | r <- iface_rules
	      		   	     , isNothing (ifRuleOrph r)
                                     , if ifRuleAuto r then warn_auto_orphs
                                                       else warn_orphs ]

	; if errorsFound dflags errs_and_warns
            then return ( errs_and_warns, Nothing )
            else do {

	   	-- Debug printing
	; dumpIfSet_dyn dflags Opt_D_dump_hi "FINAL INTERFACE" 
			(pprModIface new_iface)

                -- bug #1617: on reload we weren't updating the PrintUnqualified
                -- correctly.  This stems from the fact that the interface had
                -- not changed, so addVersionInfo returns the old ModIface
                -- with the old GlobalRdrEnv (mi_globals).
        ; let final_iface = new_iface{ mi_globals = Just rdr_env }

	; return (errs_and_warns, Just (final_iface, no_change_at_all)) }}
  where
     r1 `le_rule`     r2 = ifRuleName      r1    <=    ifRuleName      r2
     i1 `le_inst`     i2 = ifDFun          i1 `le_occ` ifDFun          i2  
     i1 `le_fam_inst` i2 = ifFamInstTcName i1 `le_occ` ifFamInstTcName i2

     le_occ :: Name -> Name -> Bool
	-- Compare lexicographically by OccName, *not* by unique, because 
	-- the latter is not stable across compilations
     le_occ n1 n2 = nameOccName n1 <= nameOccName n2

     dflags = hsc_dflags hsc_env

     deliberatelyOmitted :: String -> a
     deliberatelyOmitted x = panic ("Deliberately omitted: " ++ x)

     ifFamInstTcName = ifaceTyConName . ifFamInstTyCon

     flattenVectInfo (VectInfo { vectInfoVar          = vVar
                               , vectInfoTyCon        = vTyCon
                               , vectInfoScalarVars   = vScalarVars
                               , vectInfoScalarTyCons = vScalarTyCons
                               }) = 
       IfaceVectInfo
       { ifaceVectInfoVar          = [Var.varName v | (v, _  ) <- varEnvElts  vVar]
       , ifaceVectInfoTyCon        = [tyConName t   | (t, t_v) <- nameEnvElts vTyCon, t /= t_v]
       , ifaceVectInfoTyConReuse   = [tyConName t   | (t, t_v) <- nameEnvElts vTyCon, t == t_v]
       , ifaceVectInfoScalarVars   = [Var.varName v | v <- varSetElems vScalarVars]
       , ifaceVectInfoScalarTyCons = nameSetToList vScalarTyCons
       } 

-----------------------------
writeIfaceFile :: DynFlags -> ModLocation -> ModIface -> IO ()
writeIfaceFile dflags location new_iface
    = do createDirectoryHierarchy (takeDirectory hi_file_path)
         writeBinIface dflags hi_file_path new_iface
    where hi_file_path = ml_hi_file location


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
        -> ModIface	     -- The new interface (lacking decls)
        -> [IfaceDecl]       -- The new decls
        -> IO (ModIface,     -- Updated interface
               Bool)	     -- True <=> no changes at all; 
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
        where extras = declExtras fix_fn non_orph_rules non_orph_insts decl

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
                  extendOccEnvList env [ (b,n) | b <- ifaceDeclSubBndrs d ]
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
                     | otherwise = 
                        snd (lookupOccEnv local_env (getOccName name)
                           `orElse` pprPanic "urk! lookup local fingerprint" 
                                       (ppr name)) -- (undefined,fingerprint0))
                -- This panic indicates that we got the dependency
                -- analysis wrong, because we needed a fingerprint for
                -- an entity that wasn't in the environment.  To debug
                -- it, turn the panic into a trace, uncomment the
                -- pprTraces below, run the compile again, and inspect
                -- the output and the generated .hi file with
                -- --show-iface.
            in 
            put_ bh hash

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
               return (extend_hash_env (hash,decl) local_env,
                       (hash,decl) : decls_w_hashes)

       fingerprint_group (local_env, decls_w_hashes) (CyclicSCC abis)
          = do let decls = map abiDecl abis
                   local_env' = foldr extend_hash_env local_env 
                                   (zip (repeat fingerprint0) decls)
                   hash_fn = mk_put_name local_env'
               -- pprTrace "fingerprinting" (ppr (map ifName decls) ) $ do
               let stable_abis = sortBy cmp_abiNames abis
                -- put the cycle in a canonical order
               hash <- computeFingerprint hash_fn stable_abis
               let pairs = zip (repeat hash) decls
               return (foldr extend_hash_env local_env pairs,
                       pairs ++ decls_w_hashes)

       extend_hash_env :: (Fingerprint,IfaceDecl)
                       -> OccEnv (OccName,Fingerprint)
                       -> OccEnv (OccName,Fingerprint)
       extend_hash_env (hash,d) env0 = foldr add_imp env1 (ifaceDeclSubBndrs d)
        where
          decl_name = ifName d
          item = (decl_name, hash)
          env1 = extendOccEnv env0 decl_name item
          add_imp bndr env = extendOccEnv env bndr item
            
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
                      (map ifDFun orph_insts, orph_rules, fam_insts)

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

   -- the ABI hash depends on:
   --   - decls
   --   - export list
   --   - orphans
   --   - deprecations
   --   - XXX vect info?
   mod_hash <- computeFingerprint putNameLiterally
                      (map fst sorted_decls,
                       export_hash,
                       orphan_hash,
                       mi_warns iface0)

   -- The interface hash depends on:
   --    - the ABI hash, plus
   --    - usages
   --    - deps
   --    - hpc
   iface_hash <- computeFingerprint putNameLiterally
                      (mod_hash, 
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
                mi_orphan      = not (null orph_rules && null orph_insts),
                mi_finsts      = not . null $ mi_fam_insts iface0,
                mi_decls       = sorted_decls,
                mi_hash_fn     = lookupOccEnv local_env }
   --
   return (final_iface, no_change_at_all)

  where
    this_mod = mi_module iface0
    dflags = hsc_dflags hsc_env
    this_pkg = thisPackage dflags
    (non_orph_insts, orph_insts) = mkOrphMap ifInstOrph (mi_insts iface0)
    (non_orph_rules, orph_rules) = mkOrphMap ifRuleOrph (mi_rules iface0)
        -- ToDo: shouldn't we be splitting fam_insts into orphans and
        -- non-orphans?
    fam_insts = mi_fam_insts iface0
    fix_fn = mi_fix_fn iface0


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


%************************************************************************
%*		                					*
          The ABI of an IfaceDecl       									
%*	       	     							*
%************************************************************************

Note [The ABI of an IfaceDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ABI of a declaration consists of:

   (a) the full name of the identifier (inc. module and package,
       because these are used to construct the symbol name by which
       the identifier is known externally).

   (b) the declaration itself, as exposed to clients.  That is, the
       definition of an Id is included in the fingerprint only if
       it is made available as as unfolding in the interface.

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
  = IfaceIdExtras    Fixity [IfaceRule]
  | IfaceDataExtras  Fixity [IfaceInstABI] [(Fixity,[IfaceRule])]
  | IfaceClassExtras Fixity [IfaceInstABI] [(Fixity,[IfaceRule])]
  | IfaceSynExtras   Fixity
  | IfaceOtherDeclExtras

abiDecl :: IfaceDeclABI -> IfaceDecl
abiDecl (_, decl, _) = decl

cmp_abiNames :: IfaceDeclABI -> IfaceDeclABI -> Ordering
cmp_abiNames abi1 abi2 = ifName (abiDecl abi1) `compare` 
                         ifName (abiDecl abi2)

freeNamesDeclABI :: IfaceDeclABI -> NameSet
freeNamesDeclABI (_mod, decl, extras) =
  freeNamesIfDecl decl `unionNameSets` freeNamesDeclExtras extras

freeNamesDeclExtras :: IfaceDeclExtras -> NameSet
freeNamesDeclExtras (IfaceIdExtras    _ rules)
  = unionManyNameSets (map freeNamesIfRule rules)
freeNamesDeclExtras (IfaceDataExtras  _ insts subs)
  = unionManyNameSets (mkNameSet insts : map freeNamesSub subs)
freeNamesDeclExtras (IfaceClassExtras _ insts subs)
  = unionManyNameSets (mkNameSet insts : map freeNamesSub subs)
freeNamesDeclExtras (IfaceSynExtras _)
  = emptyNameSet
freeNamesDeclExtras IfaceOtherDeclExtras
  = emptyNameSet

freeNamesSub :: (Fixity,[IfaceRule]) -> NameSet
freeNamesSub (_,rules) = unionManyNameSets (map freeNamesIfRule rules)

instance Outputable IfaceDeclExtras where
  ppr IfaceOtherDeclExtras       = empty
  ppr (IfaceIdExtras  fix rules) = ppr_id_extras fix rules
  ppr (IfaceSynExtras fix)       = ppr fix
  ppr (IfaceDataExtras fix insts stuff)  = vcat [ppr fix, ppr_insts insts,
                                                 ppr_id_extras_s stuff]
  ppr (IfaceClassExtras fix insts stuff) = vcat [ppr fix, ppr_insts insts,
                                                 ppr_id_extras_s stuff]

ppr_insts :: [IfaceInstABI] -> SDoc
ppr_insts _ = ptext (sLit "<insts>")

ppr_id_extras_s :: [(Fixity, [IfaceRule])] -> SDoc
ppr_id_extras_s stuff = vcat [ppr_id_extras f r | (f,r)<- stuff]

ppr_id_extras :: Fixity -> [IfaceRule] -> SDoc
ppr_id_extras fix rules = ppr fix $$ vcat (map ppr rules)

-- This instance is used only to compute fingerprints
instance Binary IfaceDeclExtras where
  get _bh = panic "no get for IfaceDeclExtras"
  put_ bh (IfaceIdExtras fix rules) = do
   putByte bh 1; put_ bh fix; put_ bh rules
  put_ bh (IfaceDataExtras fix insts cons) = do
   putByte bh 2; put_ bh fix; put_ bh insts; put_ bh cons
  put_ bh (IfaceClassExtras fix insts methods) = do
   putByte bh 3; put_ bh fix; put_ bh insts; put_ bh methods
  put_ bh (IfaceSynExtras fix) = do
   putByte bh 4; put_ bh fix
  put_ bh IfaceOtherDeclExtras = do
   putByte bh 5

declExtras :: (OccName -> Fixity)
           -> OccEnv [IfaceRule]
           -> OccEnv [IfaceInst]
           -> IfaceDecl
           -> IfaceDeclExtras

declExtras fix_fn rule_env inst_env decl
  = case decl of
      IfaceId{} -> IfaceIdExtras (fix_fn n) 
                        (lookupOccEnvL rule_env n)
      IfaceData{ifCons=cons} -> 
                     IfaceDataExtras (fix_fn n)
                        (map ifDFun $ lookupOccEnvL inst_env n)
                        (map (id_extras . ifConOcc) (visibleIfConDecls cons))
      IfaceClass{ifSigs=sigs} -> 
                     IfaceClassExtras (fix_fn n)
                        (map ifDFun $ lookupOccEnvL inst_env n)
                        [id_extras op | IfaceClassOp op _ _ <- sigs]
      IfaceSyn{} -> IfaceSynExtras (fix_fn n)
      _other -> IfaceOtherDeclExtras
  where
        n = ifName decl
        id_extras occ = (fix_fn occ, lookupOccEnvL rule_env occ)

--
-- When hashing an instance, we hash only the DFunId, because that
-- depends on all the information about the instance.
--
type IfaceInstABI = IfExtName

lookupOccEnvL :: OccEnv [v] -> OccName -> [v]
lookupOccEnvL env k = lookupOccEnv env k `orElse` []

-- used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: BinHandle -> Name -> IO ()
putNameLiterally bh name = ASSERT( isExternalName name ) 
  do { put_ bh $! nameModule name
     ; put_ bh $! nameOccName name }

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
    ExitFailure _ -> ghcError (PhaseFailed cmd r)
    ExitSuccess -> do
        hash_str <- readFile tmp2
        return $! readHexFingerprint hash_str
-}

instOrphWarn :: PrintUnqualified -> Instance -> WarnMsg
instOrphWarn unqual inst
  = mkWarnMsg (getSrcSpan inst) unqual $
    hang (ptext (sLit "Warning: orphan instance:")) 2 (pprInstanceHdr inst)

ruleOrphWarn :: PrintUnqualified -> Module -> IfaceRule -> WarnMsg
ruleOrphWarn unqual mod rule
  = mkWarnMsg silly_loc unqual $
    ptext (sLit "Orphan rule:") <+> ppr rule
  where
    silly_loc = srcLocSpan (mkSrcLoc (moduleNameFS (moduleName mod)) 1 1)
    -- We don't have a decent SrcSpan for a Rule, not even the CoreRule
    -- Could readily be fixed by adding a SrcSpan to CoreRule, if we wanted to

----------------------
-- mkOrphMap partitions instance decls or rules into
-- 	(a) an OccEnv for ones that are not orphans, 
--	    mapping the local OccName to a list of its decls
--	(b) a list of orphan decls
mkOrphMap :: (decl -> Maybe OccName)	-- (Just occ) for a non-orphan decl, keyed by occ
					-- Nothing for an orphan decl
	  -> [decl] 			-- Sorted into canonical order
	  -> (OccEnv [decl],	 	-- Non-orphan decls associated with their key;
					--	each sublist in canonical order
	      [decl])			-- Orphan decls; in canonical order
mkOrphMap get_key decls
  = foldl go (emptyOccEnv, []) decls
  where
    go (non_orphs, orphs) d
	| Just occ <- get_key d
	= (extendOccEnv_Acc (:) singleton non_orphs occ d, orphs)
	| otherwise = (non_orphs, d:orphs)
\end{code}


%************************************************************************
%*		                					*
       Keeping track of what we've slurped, and fingerprints
%*	       	     							*
%************************************************************************

\begin{code}
mkUsageInfo :: HscEnv -> Module -> ImportedMods -> NameSet -> IO [Usage]
mkUsageInfo hsc_env this_mod dir_imp_mods used_names
  = do	{ eps <- hscEPS hsc_env
	; let usages = mk_usage_info (eps_PIT eps) hsc_env this_mod
				     dir_imp_mods used_names
	; usages `seqList`  return usages }
	 -- seq the list of Usages returned: occasionally these
	 -- don't get evaluated for a while and we can end up hanging on to
	 -- the entire collection of Ifaces.

mk_usage_info :: PackageIfaceTable
              -> HscEnv
              -> Module
              -> ImportedMods
              -> NameSet
              -> [Usage]
mk_usage_info pit hsc_env this_mod direct_imports used_names
  = mapCatMaybes mkUsage usage_mods
  where
    hpt = hsc_HPT hsc_env
    dflags = hsc_dflags hsc_env
    this_pkg = thisPackage dflags

    used_mods    = moduleEnvKeys ent_map
    dir_imp_mods = (moduleEnvKeys direct_imports)
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
             Nothing  -> pprPanic "mkUsageInfo: internal name?" (ppr name)
             Just mod -> -- This lambda function is really just a
                         -- specialised (++); originally came about to
                         -- avoid quadratic behaviour (trac #2680)
                         extendModuleEnvWith (\_ xs -> occ:xs) mv_map mod [occ]
    		   where occ = nameOccName name
    
    -- We want to create a Usage for a home module if 
    --	a) we used something from it; has something in used_names
    --	b) we imported it, even if we used nothing from it
    --	   (need to recompile if its export list changes: export_fprint)
    mkUsage :: Module -> Maybe Usage
    mkUsage mod
      | isNothing maybe_iface		-- We can't depend on it if we didn't
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
      = Nothing			-- Record no usage info
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
mkIfaceAnnotations :: [Annotation] -> [IfaceAnnotation]
mkIfaceAnnotations = map mkIfaceAnnotation

mkIfaceAnnotation :: Annotation -> IfaceAnnotation
mkIfaceAnnotation (Annotation { ann_target = target, ann_value = serialized }) = IfaceAnnotation { 
        ifAnnotatedTarget = fmap nameOccName target,
        ifAnnotatedValue = serialized
    }
\end{code}

\begin{code}
mkIfaceExports :: [AvailInfo]
               -> [(Module, [GenAvailInfo OccName])]
                  -- Group by module and sort by occurrence
mkIfaceExports exports
  = [ (mod, Map.elems avails)
    | (mod, avails) <- sortBy (stableModuleCmp `on` fst)
                              (moduleEnvToList groupFM)
                       -- NB. the Map.toList is in a random order,
                       -- because Ord Module is not a predictable
                       -- ordering.  Hence we perform a final sort
                       -- using the stable Module ordering.
    ]
  where
	-- Group by the module where the exported entities are defined
	-- (which may not be the same for all Names in an Avail)
	-- Deliberately use Map rather than UniqFM so we
	-- get a canonical ordering
    groupFM :: ModuleEnv (Map FastString (GenAvailInfo OccName))
    groupFM = foldl add emptyModuleEnv exports

    add_one :: ModuleEnv (Map FastString (GenAvailInfo OccName))
	    -> Module -> GenAvailInfo OccName
	    -> ModuleEnv (Map FastString (GenAvailInfo OccName))
    add_one env mod avail 
      -- XXX Is there a need to flip Map.union here?
      =  extendModuleEnvWith (flip Map.union) env mod 
		(Map.singleton (occNameFS (availName avail)) avail)

	-- NB: we should not get T(X) and T(Y) in the export list
	--     else the Map.union will simply discard one!  They
	--     should have been combined by now.
    add env (Avail n)
      = ASSERT( isExternalName n ) 
        add_one env (nameModule n) (Avail (nameOccName n))

    add env (AvailTC tc ns)
      = ASSERT( all isExternalName ns ) 
	foldl add_for_mod env mods
      where
	tc_occ = nameOccName tc
	mods   = nub (map nameModule ns)
		-- Usually just one, but see Note [Original module]

	add_for_mod env mod
	    = add_one env mod (AvailTC tc_occ (sort names_from_mod))
              -- NB. sort the children, we need a canonical order
	    where
	      names_from_mod = [nameOccName n | n <- ns, nameModule n == mod]
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


%************************************************************************
%*									*
	Load the old interface file for this module (unless
	we have it aleady), and check whether it is up to date
	
%*									*
%************************************************************************

\begin{code}
checkOldIface :: HscEnv
	      -> ModSummary
	      -> Bool 			-- Source unchanged
	      -> Maybe ModIface 	-- Old interface from compilation manager, if any
	      -> IO (RecompileRequired, Maybe ModIface)

checkOldIface hsc_env mod_summary source_unchanged maybe_iface
  = do  showPass (hsc_dflags hsc_env) $
            "Checking old interface for " ++ (showSDoc $ ppr $ ms_mod mod_summary)
        initIfaceCheck hsc_env $
            check_old_iface hsc_env mod_summary source_unchanged maybe_iface

check_old_iface :: HscEnv -> ModSummary -> Bool -> Maybe ModIface
                -> IfG (Bool, Maybe ModIface)
check_old_iface hsc_env mod_summary src_unchanged maybe_iface
  = let src_changed = not src_unchanged
        dflags = hsc_dflags hsc_env
        getIface =
             case maybe_iface of
                 Just _  -> do
                     traceIf (text "We already have the old interface for" <+> ppr (ms_mod mod_summary))
                     return maybe_iface
                 Nothing -> do
                     let iface_path = msHiFilePath mod_summary
                     read_result <- readIface (ms_mod mod_summary) iface_path False
                     case read_result of
                         Failed err -> do
                             traceIf (text "FYI: cannont read old interface file:" $$ nest 4 err)
                             return Nothing
                         Succeeded iface -> do
                             traceIf (text "Read the interface file" <+> text iface_path)
                             return $ Just iface

    in do
        when src_changed
             (traceHiDiffs (nest 4 (text "Source file changed or recompilation check turned off")))

         -- If the source has changed and we're in interactive mode, avoid reading
         -- an interface; just return the one we might have been supplied with.
        if not (isObjectTarget $ hscTarget dflags) && src_changed
            then return (outOfDate, maybe_iface)
            else do
                -- Try and read the old interface for the current module
                -- from the .hi file left from the last time we compiled it
                maybe_iface' <- getIface
                case maybe_iface' of
                    Nothing -> return (outOfDate, maybe_iface')
                    Just iface -> do
                        -- We have got the old iface; check its versions
                        recomp <- checkVersions hsc_env src_unchanged mod_summary iface
                        return recomp
\end{code}

@recompileRequired@ is called from the HscMain.   It checks whether
a recompilation is required.  It needs access to the persistent state,
finder, etc, because it may have to load lots of interface files to
check their versions.

\begin{code}
type RecompileRequired = Bool
upToDate, outOfDate :: Bool
upToDate  = False	-- Recompile not required
outOfDate = True	-- Recompile required

-- | Check the safe haskell flags haven't changed
--   (e.g different flag on command line now)
safeHsChanged :: HscEnv -> ModIface -> Bool
safeHsChanged hsc_env iface
  = (getSafeMode $ mi_trust iface) /= (safeHaskell $ hsc_dflags hsc_env)

checkVersions :: HscEnv
	      -> Bool		-- True <=> source unchanged
              -> ModSummary
	      -> ModIface 	-- Old interface
	      -> IfG (RecompileRequired, Maybe ModIface)
checkVersions hsc_env source_unchanged mod_summary iface
  | not source_unchanged
  = let iface' = if safeHsChanged hsc_env iface then Nothing else Just iface
    in return (outOfDate, iface')

  | otherwise
  = do { traceHiDiffs (text "Considering whether compilation is required for" <+>
                        ppr (mi_module iface) <> colon)

       ; recomp <- checkDependencies hsc_env mod_summary iface
       ; if recomp then return (outOfDate, Just iface) else do {
       ; if trust_dif then return (outOfDate, Nothing) else do {

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
    this_pkg  = thisPackage (hsc_dflags hsc_env)
    trust_dif = safeHsChanged hsc_env iface
    -- This is a bit of a hack really
    mod_deps :: ModuleNameEnv (ModuleName, IsBootInterface)
    mod_deps = mkModDeps (dep_mods (mi_deps iface))


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
 = orM (map dep_missing (ms_imps summary ++ ms_srcimps summary))
  where
   prev_dep_mods = dep_mods (mi_deps iface)
   prev_dep_pkgs = dep_pkgs (mi_deps iface)

   this_pkg = thisPackage (hsc_dflags hsc_env)

   orM = foldr f (return False)
    where f m rest = do b <- m; if b then return True else rest

   dep_missing (L _ (ImportDecl (L _ mod) pkg _ _ _ _ _)) = do
     find_res <- liftIO $ findImportedModule hsc_env mod pkg
     case find_res of
        Found _ mod
          | pkg == this_pkg
           -> if moduleName mod `notElem` map fst prev_dep_mods
                 then do traceHiDiffs $
                           text "imported module " <> quotes (ppr mod) <>
                           text " not among previous dependencies"
                         return outOfDate
                 else
                         return upToDate
          | otherwise
           -> if pkg `notElem` (map fst prev_dep_pkgs)
                 then do traceHiDiffs $
                           text "imported module " <> quotes (ppr mod) <>
                           text " is from package " <> quotes (ppr pkg) <>
                           text ", which is not among previous dependencies"
                         return outOfDate
                 else
                         return upToDate
           where pkg = modulePackageId mod
        _otherwise  -> return outOfDate

needInterface :: Module -> (ModIface -> IfG RecompileRequired)
              -> IfG RecompileRequired
needInterface mod continue
  = do	-- Load the imported interface if possible
    let doc_str = sep [ptext (sLit "need version info for"), ppr mod]
    traceHiDiffs (text "Checking usages for module" <+> ppr mod)

    mb_iface <- loadInterface doc_str mod ImportBySystem
	-- Load the interface, but don't complain on failure;
	-- Instead, get an Either back which we can test

    case mb_iface of
      Failed _ ->  (out_of_date (sep [ptext (sLit "Couldn't load interface for module"),
                                      ppr mod]))
                  -- Couldn't find or parse a module mentioned in the
                  -- old interface file.  Don't complain: it might
                  -- just be that the current module doesn't need that
                  -- import and it's been deleted
      Succeeded iface -> continue iface


checkModUsage :: PackageId ->Usage -> IfG RecompileRequired
-- Given the usage information extracted from the old
-- M.hi file for the module being compiled, figure out
-- whether M needs to be recompiled.

checkModUsage _this_pkg UsagePackageModule{
                                usg_mod = mod,
                                usg_mod_hash = old_mod_hash }
  = needInterface mod $ \iface -> do
    checkModuleFingerprint old_mod_hash (mi_mod_hash iface)
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
	new_decl_hash 	= mi_hash_fn     iface
	new_export_hash = mi_exp_hash    iface

	-- CHECK MODULE
    recompile <- checkModuleFingerprint old_mod_hash new_mod_hash
    if not recompile then return upToDate else do
				 
	-- CHECK EXPORT LIST
    checkMaybeHash maybe_old_export_hash new_export_hash
        (ptext (sLit "  Export list changed")) $ do

	-- CHECK ITEMS ONE BY ONE
    recompile <- checkList [ checkEntityUsage new_decl_hash u 
                           | u <- old_decl_hash]
    if recompile 
      then return outOfDate	-- This one failed, so just bail out now
      else up_to_date (ptext (sLit "  Great!  The bits I use are up to date"))

------------------------
checkModuleFingerprint :: Fingerprint -> Fingerprint -> IfG Bool
checkModuleFingerprint old_mod_hash new_mod_hash
  | new_mod_hash == old_mod_hash
  = up_to_date (ptext (sLit "Module fingerprint unchanged"))

  | otherwise
  = out_of_date_hash (ptext (sLit "  Module fingerprint has changed"))
		     old_mod_hash new_mod_hash

------------------------
checkMaybeHash :: Maybe Fingerprint -> Fingerprint -> SDoc
               -> IfG RecompileRequired -> IfG RecompileRequired
checkMaybeHash maybe_old_hash new_hash doc continue
  | Just hash <- maybe_old_hash, hash /= new_hash
  = out_of_date_hash doc hash new_hash
  | otherwise
  = continue

------------------------
checkEntityUsage :: (OccName -> Maybe (OccName, Fingerprint))
                 -> (OccName, Fingerprint)
                 -> IfG Bool
checkEntityUsage new_hash (name,old_hash)
  = case new_hash name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  out_of_date (sep [ptext (sLit "No longer exported:"), ppr name])

	Just (_, new_hash) 	-- It's there, but is it up to date?
	  | new_hash == old_hash -> do traceHiDiffs (text "  Up to date" <+> ppr name <+> parens (ppr new_hash))
			  	       return upToDate
	  | otherwise	 	 -> out_of_date_hash (ptext (sLit "  Out of date:") <+> ppr name)
						     old_hash new_hash

up_to_date, out_of_date :: SDoc -> IfG Bool
up_to_date  msg = traceHiDiffs msg >> return upToDate
out_of_date msg = traceHiDiffs msg >> return outOfDate

out_of_date_hash :: SDoc -> Fingerprint -> Fingerprint -> IfG Bool
out_of_date_hash msg old_hash new_hash 
  = out_of_date (hsep [msg, ppr old_hash, ptext (sLit "->"), ppr new_hash])

----------------------
checkList :: [IfG RecompileRequired] -> IfG RecompileRequired
-- This helper is used in two places
checkList []		 = return upToDate
checkList (check:checks) = do recompile <- check
                              if recompile
                                then return outOfDate
                                else checkList checks
\end{code}

%************************************************************************
%*				 					*
		Converting things to their Iface equivalents
%*				 					*
%************************************************************************

\begin{code}
tyThingToIfaceDecl :: TyThing -> IfaceDecl
-- Assumption: the thing is already tidied, so that locally-bound names
-- 	       (lambdas, for-alls) already have non-clashing OccNames
-- Reason: Iface stuff uses OccNames, and the conversion here does
--	   not do tidying on the way
tyThingToIfaceDecl (AnId id)
  = IfaceId { ifName      = getOccName id,
	      ifType      = toIfaceType (idType id),
	      ifIdDetails = toIfaceIdDetails (idDetails id),
	      ifIdInfo    = toIfaceIdInfo (idInfo id) }

tyThingToIfaceDecl (AClass clas)
  = IfaceClass { ifCtxt	  = toIfaceContext sc_theta,
		 ifName	  = getOccName clas,
		 ifTyVars = toIfaceTvBndrs clas_tyvars,
		 ifFDs    = map toIfaceFD clas_fds,
		 ifATs	  = map (tyThingToIfaceDecl . ATyCon) clas_ats,
		 ifSigs	  = map toIfaceClassOp op_stuff,
	  	 ifRec    = boolToRecFlag (isRecursiveTyCon tycon) }
  where
    (clas_tyvars, clas_fds, sc_theta, _, clas_ats, op_stuff) 
      = classExtraBigSig clas
    tycon = classTyCon clas

    toIfaceClassOp (sel_id, def_meth)
	= ASSERT(sel_tyvars == clas_tyvars)
	  IfaceClassOp (getOccName sel_id) (toDmSpec def_meth) (toIfaceType op_ty)
	where
		-- Be careful when splitting the type, because of things
		-- like  	class Foo a where
		--		  op :: (?x :: String) => a -> a
		-- and  	class Baz a where
		--		  op :: (Ord a) => a -> a
	  (sel_tyvars, rho_ty) = splitForAllTys (idType sel_id)
	  op_ty		       = funResultTy rho_ty

    toDmSpec NoDefMeth      = NoDM
    toDmSpec (GenDefMeth _) = GenericDM
    toDmSpec (DefMeth _)    = VanillaDM

    toIfaceFD (tvs1, tvs2) = (map getFS tvs1, map getFS tvs2)

tyThingToIfaceDecl (ATyCon tycon)
  | isSynTyCon tycon
  = IfaceSyn {	ifName    = getOccName tycon,
		ifTyVars  = toIfaceTvBndrs tyvars,
		ifSynRhs  = syn_rhs,
	  	ifSynKind = syn_ki,
                ifFamInst = famInstToIface (tyConFamInst_maybe tycon)
             }

  | isAlgTyCon tycon
  = IfaceData {	ifName    = getOccName tycon,
		ifTyVars  = toIfaceTvBndrs tyvars,
		ifCtxt    = toIfaceContext (tyConStupidTheta tycon),
		ifCons    = ifaceConDecls (algTyConRhs tycon),
	  	ifRec     = boolToRecFlag (isRecursiveTyCon tycon),
		ifGadtSyntax = isGadtSyntaxTyCon tycon,
		ifFamInst = famInstToIface (tyConFamInst_maybe tycon)}

  | isForeignTyCon tycon
  = IfaceForeign { ifName    = getOccName tycon,
	    	   ifExtName = tyConExtName tycon }

  | otherwise = pprPanic "toIfaceDecl" (ppr tycon)
  where
    tyvars = tyConTyVars tycon
    (syn_rhs, syn_ki) 
       = case synTyConRhs tycon of
	    SynFamilyTyCon  -> (Nothing,               toIfaceType (synTyConResKind tycon))
	    SynonymTyCon ty -> (Just (toIfaceType ty), toIfaceType (typeKind ty))

    ifaceConDecls (NewTyCon { data_con = con })     = 
      IfNewTyCon  (ifaceConDecl con)
    ifaceConDecls (DataTyCon { data_cons = cons })  = 
      IfDataTyCon (map ifaceConDecl cons)
    ifaceConDecls DataFamilyTyCon {}                = IfOpenDataTyCon
    ifaceConDecls AbstractTyCon			    = IfAbstractTyCon
	-- The last case happens when a TyCon has been trimmed during tidying
	-- Furthermore, tyThingToIfaceDecl is also used
	-- in TcRnDriver for GHCi, when browsing a module, in which case the
	-- AbstractTyCon case is perfectly sensible.

    ifaceConDecl data_con 
	= IfCon   { ifConOcc   	 = getOccName (dataConName data_con),
		    ifConInfix 	 = dataConIsInfix data_con,
		    ifConWrapper = isJust (dataConWrapId_maybe data_con),
		    ifConUnivTvs = toIfaceTvBndrs univ_tvs,
		    ifConExTvs   = toIfaceTvBndrs ex_tvs,
		    ifConEqSpec  = to_eq_spec eq_spec,
		    ifConCtxt    = toIfaceContext theta,
		    ifConArgTys  = map toIfaceType arg_tys,
		    ifConFields  = map getOccName 
				       (dataConFieldLabels data_con),
		    ifConStricts = dataConStrictMarks data_con }
        where
          (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _) = dataConFullSig data_con

    to_eq_spec spec = [(getOccName tv, toIfaceType ty) | (tv,ty) <- spec]

    famInstToIface Nothing                    = Nothing
    famInstToIface (Just (famTyCon, instTys)) = 
      Just (toIfaceTyCon famTyCon, map toIfaceType instTys)

tyThingToIfaceDecl c@(ACoAxiom _) = pprPanic "tyThingToIfaceDecl (ACoCon _)" (ppr c)

tyThingToIfaceDecl (ADataCon dc)
 = pprPanic "toIfaceDecl" (ppr dc)	-- Should be trimmed out earlier


getFS :: NamedThing a => a -> FastString
getFS x = occNameFS (getOccName x)

--------------------------
instanceToIfaceInst :: Instance -> IfaceInst
instanceToIfaceInst (Instance { is_dfun = dfun_id, is_flag = oflag,
                                is_cls = cls_name, is_tcs = mb_tcs })
  = ASSERT( cls_name == className cls )
    IfaceInst { ifDFun    = dfun_name,
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
    (_, _, cls, tys) = tcSplitDFunTy (idType dfun_id)
		-- Slightly awkward: we need the Class to get the fundeps
    (tvs, fds) = classTvsFds cls
    arg_names = [filterNameSet is_local (orphNamesOfType ty) | ty <- tys]
    orph | is_local cls_name = Just (nameOccName cls_name)
	 | all isJust mb_ns  = ASSERT( not (null mb_ns) ) head mb_ns
	 | otherwise	     = Nothing
    
    mb_ns :: [Maybe OccName]	-- One for each fundep; a locally-defined name
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
famInstToIfaceFamInst (FamInst { fi_tycon = tycon,
                                 fi_fam = fam,
                                 fi_tcs = mb_tcs })
  = IfaceFamInst { ifFamInstTyCon  = toIfaceTyCon tycon
		 , ifFamInstFam    = fam
		 , ifFamInstTys    = map do_rough mb_tcs }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

--------------------------
toIfaceLetBndr :: Id -> IfaceLetBndr
toIfaceLetBndr id  = IfLetBndr (occNameFS (getOccName id))
			       (toIfaceType (idType id)) 
			       (toIfaceIdInfo (idInfo id))
  -- Put into the interface file any IdInfo that CoreTidy.tidyLetBndr 
  -- has left on the Id.  See Note [IdInfo on nested let-bindings] in IfaceSyn

--------------------------
toIfaceIdDetails :: IdDetails -> IfaceIdDetails
toIfaceIdDetails VanillaId 		        = IfVanillaId
toIfaceIdDetails (DFunId {})                    = IfDFunId 
toIfaceIdDetails (RecSelId { sel_naughty = n
		 	   , sel_tycon = tc })  = IfRecSelId (toIfaceTyCon tc) n
toIfaceIdDetails other	     		        = pprTrace "toIfaceIdDetails" (ppr other) 
                                                  IfVanillaId   -- Unexpected

toIfaceIdInfo :: IdInfo -> IfaceIdInfo
toIfaceIdInfo id_info
  = case catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo, 
	            inline_hsinfo,  unfold_hsinfo] of
       []    -> NoInfo
       infos -> HasInfo infos
	       -- NB: strictness must appear in the list before unfolding
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
    strict_hsinfo = case strictnessInfo id_info of
			Just sig | not (isTopSig sig) -> Just (HsStrictness sig)
			_other			      -> Nothing

    ------------  Unfolding  --------------
    unfold_hsinfo = toIfUnfolding loop_breaker (unfoldingInfo id_info) 
    loop_breaker  = isNonRuleLoopBreaker (occInfo id_info)
					
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
	InlineWrapper w | isExternalName n -> IfExtWrapper arity n
	                | otherwise        -> IfLclWrapper arity (getFS n)
			where
                          n = idName w
        InlineCompulsory -> IfCompulsory if_rhs
        InlineRhs        -> IfCoreUnfold False if_rhs
	-- Yes, even if guidance is UnfNever, expose the unfolding
	-- If we didn't want to expose the unfolding, TidyPgm would
	-- have stuck in NoUnfolding.  For supercompilation we want 
	-- to see that unfolding!
  where
    if_rhs = toIfaceExpr rhs

toIfUnfolding lb (DFunUnfolding _ar _con ops)
  = Just (HsUnfold lb (IfDFunUnfold (map toIfaceExpr ops)))
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
    do_arg (Type ty) = IfaceType (toIfaceType (deNoteType ty))
    do_arg (Coercion co) = IfaceType (coToIfaceType co)
                           
    do_arg arg       = toIfaceExpr arg

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
toIfaceExpr (Coercion co)   = IfaceCo   (coToIfaceType co)
toIfaceExpr (Lam x b)       = IfaceLam (toIfaceBndr x) (toIfaceExpr b)
toIfaceExpr (App f a)       = toIfaceApp f [a]
toIfaceExpr (Case s x _ as) = IfaceCase (toIfaceExpr s) (getFS x) (map toIfaceAlt as)
toIfaceExpr (Let b e)       = IfaceLet (toIfaceBind b) (toIfaceExpr e)
toIfaceExpr (Cast e co)     = IfaceCast (toIfaceExpr e) (coToIfaceType co)
toIfaceExpr (Note n e)      = IfaceNote (toIfaceNote n) (toIfaceExpr e)

---------------------
toIfaceNote :: Note -> IfaceNote
toIfaceNote (SCC cc)      = IfaceSCC cc
toIfaceNote (CoreNote s)  = IfaceCoreNote s

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
toIfaceCon (DataAlt dc) | isTupleTyCon tc = IfaceTupleAlt (tupleTyConBoxity tc)
	   		| otherwise       = IfaceDataAlt (getName dc)
	   		where
	   		  tc = dataConTyCon dc
	   
toIfaceCon (LitAlt l) = IfaceLitAlt l
toIfaceCon DEFAULT    = IfaceDefault

---------------------
toIfaceApp :: Expr CoreBndr -> [Arg CoreBndr] -> IfaceExpr
toIfaceApp (App f a) as = toIfaceApp f (a:as)
toIfaceApp (Var v) as
  = case isDataConWorkId_maybe v of
	-- We convert the *worker* for tuples into IfaceTuples
	Just dc |  isTupleTyCon tc && saturated 
		-> IfaceTuple (tupleTyConBoxity tc) tup_args
	  where
	    val_args  = dropWhile isTypeArg as
	    saturated = val_args `lengthIs` idArity v
	    tup_args  = map toIfaceExpr val_args
	    tc	      = dataConTyCon dc

        _ -> mkIfaceApps (toIfaceVar v) as

toIfaceApp e as = mkIfaceApps (toIfaceExpr e) as

mkIfaceApps :: IfaceExpr -> [CoreExpr] -> IfaceExpr
mkIfaceApps f as = foldl (\f a -> IfaceApp f (toIfaceExpr a)) f as

---------------------
toIfaceVar :: Id -> IfaceExpr
toIfaceVar v 
  | Just fcall <- isFCallId_maybe v = IfaceFCall fcall (toIfaceType (idType v))
	  -- Foreign calls have special syntax
  | isExternalName name		    = IfaceExt name
  | Just (TickBox m ix) <- isTickBoxOp_maybe v
				    = IfaceTick m ix
  | otherwise			    = IfaceLcl (getFS name)
  where
    name = idName v
\end{code}
