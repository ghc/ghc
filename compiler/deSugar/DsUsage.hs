{-# LANGUAGE CPP #-}

module DsUsage (
    -- * Dependency/fingerprinting code (used by MkIface)
    mkUsageInfo, mkUsedNames, mkDependencies
    ) where

#include "HsVersions.h"

import GhcPrelude

import DynFlags
import HscTypes
import TcRnTypes
import Name
import NameSet
import Module
import Outputable
import Util
import UniqSet
import UniqFM
import Fingerprint
import Maybes

import Data.List
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

{- Note [Module self-dependency]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RnNames.calculateAvails asserts the invariant that a module must not occur in
its own dep_orphs or dep_finsts. However, if we aren't careful this can occur
in the presence of hs-boot files: Consider that we have two modules, A and B,
both with hs-boot files,

    A.hs contains a SOURCE import of B B.hs-boot contains a SOURCE import of A
    A.hs-boot declares an orphan instance A.hs defines the orphan instance

In this case, B's dep_orphs will contain A due to its SOURCE import of A.
Consequently, A will contain itself in its imp_orphs due to its import of B.
This fact would end up being recorded in A's interface file. This would then
break the invariant asserted by calculateAvails that a module does not itself in
its dep_orphs. This was the cause of Trac #14128.

-}

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
      let dep_mods = modDepsElts (delFromUFM (imp_dep_mods imports)
                                             (moduleName mod))
                -- M.hi-boot can be in the imp_dep_mods, but we must remove
                -- it before recording the modules on which this one depends!
                -- (We want to retain M.hi-boot in imp_dep_mods so that
                --  loadHiBootInterface can see if M's direct imports depend
                --  on M.hi-boot, and hence that we should do the hi-boot consistency
                --  check.)

          dep_orphs = filter (/= mod) (imp_orphs imports)
                -- We must also remove self-references from imp_orphs. See
                -- Note [Module self-dependency]

          pkgs | th_used   = Set.insert (toInstalledUnitId thUnitId) (imp_dep_pkgs imports)
               | otherwise = imp_dep_pkgs imports

          -- Set the packages required to be Safe according to Safe Haskell.
          -- See Note [RnNames . Tracking Trust Transitively]
          sorted_pkgs = sort (Set.toList pkgs)
          trust_pkgs  = imp_trust_pkgs imports
          dep_pkgs'   = map (\x -> (x, x `Set.member` trust_pkgs)) sorted_pkgs

      return Deps { dep_mods   = dep_mods,
                    dep_pkgs   = dep_pkgs',
                    dep_orphs  = dep_orphs,
                    dep_finsts = sortBy stableModuleCmp (imp_finsts imports) }
                    -- sort to get into canonical order
                    -- NB. remember to use lexicographic ordering

mkUsedNames :: TcGblEnv -> NameSet
mkUsedNames TcGblEnv{ tcg_dus = dus } = allUses dus

mkUsageInfo :: HscEnv -> Module -> ImportedMods -> NameSet -> [FilePath] -> [(Module, Fingerprint)] -> IO [Usage]
mkUsageInfo hsc_env this_mod dir_imp_mods used_names dependent_files merged
  = do
    eps <- hscEPS hsc_env
    hashes <- mapM getFileHash dependent_files
    let mod_usages = mk_mod_usage_info (eps_PIT eps) hsc_env this_mod
                                       dir_imp_mods used_names
        usages = mod_usages ++ [ UsageFile { usg_file_path = f
                                           , usg_file_hash = hash }
                               | (f, hash) <- zip dependent_files hashes ]
                            ++ [ UsageMergedRequirement
                                    { usg_mod = mod,
                                      usg_mod_hash = hash
                                    }
                               | (mod, hash) <- merged ]
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
    ent_map  = nonDetFoldUniqSet add_mv emptyModuleEnv used_names
     -- nonDetFoldUFM is OK here. If you follow the logic, we sort by OccName
     -- in ent_hashs
     where
      add_mv name mv_map
        | isWiredInName name = mv_map  -- ignore wired-in names
        | otherwise
        = case nameModule_maybe name of
             Nothing  -> ASSERT2( isSystemName name, ppr name ) mv_map
                -- See Note [Internal used_names]

             Just mod ->
                -- See Note [Identity versus semantic module]
                let mod' = if isHoleModule mod
                            then mkModule this_pkg (moduleName mod)
                            else mod
                -- This lambda function is really just a
                -- specialised (++); originally came about to
                -- avoid quadratic behaviour (trac #2680)
                in extendModuleEnvWith (\_ xs -> occ:xs) mv_map mod' [occ]
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

      | moduleUnitId mod /= this_pkg
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

        by_is_safe (ImportedByUser imv) = imv_is_safe imv
        by_is_safe _ = False
        (is_direct_import, imp_safe)
            = case lookupModuleEnv direct_imports mod of
                -- ezyang: I'm not sure if any is the correct
                -- metric here. If safety was guaranteed to be uniform
                -- across all imports, why did the old code only look
                -- at the first import?
                Just bys -> (True, any by_is_safe bys)
                Nothing  -> (False, safeImplicitImpsReq dflags)
                -- Nothing case is for references to entities which were
                -- not directly imported (NB: the "implicit" Prelude import
                -- counts as directly imported!  An entity is not directly
                -- imported if, e.g., we got a reference to it from a
                -- reexport of another module.)

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
