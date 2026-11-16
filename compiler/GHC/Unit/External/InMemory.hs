{-# LANGUAGE BangPatterns #-}

-- | GHC's default implementation of the 'ExternalPackageState': everything
-- that was loaded is kept in memory.
--
-- See Note [Abstract External Package State] in "GHC.Unit.External.Store".
module GHC.Unit.External.InMemory
   ( newInMemoryEps
   , InMemoryEps (..)
   , emptyInMemoryEps
   , inMemoryEps
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.External.Iface
import GHC.Unit.External.Store

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( InstEnv, emptyInstEnv, extendInstEnvList )
import GHC.Core.Opt.ConstantFold ( builtinRules )
import GHC.Core.Rules ( RuleBase, mkRuleBase, extendRuleBaseList )

import GHC.Linker.Types ( Linkable )

import GHC.Types.Annotations ( AnnEnv, emptyAnnEnv, extendAnnEnvList )
import GHC.Types.CompleteMatch
import GHC.Types.DefaultEnv ( DefaultEnv )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.TypeEnv
import GHC.Types.Unique.DSet

import GHC.Utils.Fingerprint ( Fingerprint )

import Data.IORef

-- | The state of the in-memory store.
data InMemoryEps
  = InMemoryEps {
        im_ext_ifaces :: !(ModuleEnv ExternalIface),
                -- ^ What we remember of the interfaces of modules in
                -- external packages that we have opened.
                -- See Note [The EPS does not keep ModIfaces].

        im_free_holes :: InstalledModuleEnv (UniqDSet ModuleName),
                -- ^ Cache for 'mi_free_holes'.  Ordinarily, we can rely on
                -- the 'im_ext_ifaces' for this information, EXCEPT that when
                -- we do dependency analysis, we need to look at the
                -- 'Dependencies' of our imports to determine what their
                -- precise free holes are ('moduleFreeHolesPrecise').  We
                -- don't want to repeatedly reread in the interface
                -- for every import, so cache it here.

        im_PTE :: !TypeEnv,
                -- ^ Result of typechecking all the external package
                -- interface files we have sucked in. The domain of
                -- the mapping is external-package modules

        im_iface_bytecode :: !(ModuleEnv (IO Linkable)),
                -- ^ See 'eps_lookupBytecode'

        im_known_keys   :: ModuleEnv (Fingerprint, KnownKeyNameMaps),
                -- ^ See 'eps_lookupKnownKeys'

        im_inst_env     :: !InstEnv,    -- ^ The total 'InstEnv' accumulated
                                        -- from all the external-package modules
        im_fam_inst_env :: !FamInstEnv, -- ^ The total 'FamInstEnv' accumulated
                                        -- from all the external-package modules
        im_rule_base    :: !RuleBase,   -- ^ The total 'RuleEnv' accumulated
                                        -- from all the external-package modules
        im_ann_env      :: !AnnEnv,     -- ^ The total 'AnnEnv' accumulated
                                        -- from all the external-package modules
        im_complete_matches :: !CompleteMatches,
                                        -- ^ The total 'CompleteMatches' accumulated
                                        -- from all the external-package modules

        im_mod_fam_inst_env :: !(ModuleEnv FamInstEnv),
                -- ^ The family instances accumulated from external
                -- packages, keyed off the module that declared them

        im_stats :: !EpsStats,
                -- ^ Statistics about what was loaded from external packages
        im_defaults :: !(ModuleEnv DefaultEnv)
                -- ^ Default declarations exported by external packages
  }

emptyInMemoryEps :: InMemoryEps
emptyInMemoryEps = InMemoryEps
  { im_ext_ifaces       = emptyModuleEnv
  , im_free_holes       = emptyInstalledModuleEnv
  , im_PTE              = emptyTypeEnv
  , im_iface_bytecode   = emptyModuleEnv
  , im_inst_env         = emptyInstEnv
  , im_fam_inst_env     = emptyFamInstEnv
  , im_rule_base        = mkRuleBase builtinRules
    -- Initialise the EPS rule pool with the built-in rules
  , im_mod_fam_inst_env = emptyModuleEnv
  , im_known_keys       = emptyModuleEnv
  , im_complete_matches = []
  , im_ann_env          = emptyAnnEnv
  , im_stats            = emptyEpsStats (length builtinRules)
  , im_defaults         = emptyModuleEnv
  }

-- | A fresh, empty in-memory EPS store.
newInMemoryEps :: IO ExternalPackageState
newInMemoryEps = inMemoryEps <$> newIORef emptyInMemoryEps

-- | The in-memory EPS store operating on the given state.
inMemoryEps :: IORef InMemoryEps -> ExternalPackageState
inMemoryEps ref = EPS
  { eps_lookupTyThing = \name -> do
      im <- readIORef ref
      pure $! lookupNameEnv (im_PTE im) name
  , eps_lookupIface = \mod -> do
      im <- readIORef ref
      pure $! lookupModuleEnv (im_ext_ifaces im) mod
  , eps_moduleTyThings = \mod -> do
      im <- readIORef ref
      pure [ thing | thing <- nonDetNameEnvElts (im_PTE im)
                   , nameModule (getName thing) == mod ]
  , eps_lookupModFamInstEnv = \mod -> query (\im -> lookupModuleEnv (im_mod_fam_inst_env im) mod)
  , eps_lookupDefaults      = \mod -> query (\im -> lookupModuleEnv (im_defaults im) mod)
  , eps_lookupBytecode      = \mod -> query (\im -> lookupModuleEnv (im_iface_bytecode im) mod)
  , eps_lookupKnownKeys     = \mod -> query (\im -> lookupModuleEnv (im_known_keys im) mod)
  , eps_lookupFreeHoles     = \imod -> query (\im -> lookupInstalledModuleEnv (im_free_holes im) imod)

  , eps_getInstEnv         = query im_inst_env
  , eps_getFamInstEnv      = query im_fam_inst_env
  , eps_getRuleBase        = query im_rule_base
  , eps_getAnnEnv          = query im_ann_env
  , eps_getCompleteMatches = query im_complete_matches
  , eps_getStats           = query im_stats

  , eps_addModule = \epm -> update $ \im ->
      let mod = epm_module epm in
      if elemModuleEnv mod (im_ext_ifaces im)
        then im
        else addModule epm im
  , eps_addFailed = \mod -> update $ \im ->
      if elemModuleEnv mod (im_ext_ifaces im)
        then im
        else im { im_ext_ifaces = extendModuleEnv (im_ext_ifaces im) mod
                                                  (emptyExternalIface mod) }
  , eps_addBootDecls = \mod decls -> update $ \im ->
      if elemModuleEnv mod (im_ext_ifaces im)
        then im
        else im { im_PTE = extendNameEnvList (im_PTE im) decls }
  , eps_addKnownKeys = \mod kk -> update $ \im ->
      im { im_known_keys = extendModuleEnv (im_known_keys im) mod kk }
  , eps_addFreeHoles = \imod holes -> update $ \im ->
      im { im_free_holes = extendInstalledModuleEnv (im_free_holes im) imod holes }
  , eps_bumpDeclsOut = update $ \im ->
      let stats = im_stats im
      in im { im_stats = stats { n_decls_out = n_decls_out stats + 1 } }
  }
  where
    query :: (InMemoryEps -> a) -> IO a
    query f = f <$> readIORef ref
    {-# INLINE query #-}

    -- Atomic, and forces the new state in order to avoid space leaks.
    update :: (InMemoryEps -> InMemoryEps) -> IO ()
    update f = atomicModifyIORef' ref (\im -> (f im, ()))
    {-# INLINE update #-}

addModule :: EpsModule -> InMemoryEps -> InMemoryEps
-- NB: we take the fields we need out of the 'EpsModule' by a match, so that
-- the lazy values we store (e.g. the 'FamInstEnv' of the module) don't retain
-- the 'EpsModule', and with it 'epm_iface_decls' (an unevaluated thunk which
-- retains the 'ModIface').
addModule (EpsModule { epm_module = !mod, epm_ext_iface = ext_iface
                     , epm_decls = decls, epm_insts = insts
                     , epm_fam_insts = fam_insts, epm_rules = rules
                     , epm_anns = anns, epm_complete_matches = complete_matches
                     , epm_defaults = defaults, epm_bytecode = bytecode }) im
  = im {
      im_ext_ifaces   = extendModuleEnv (im_ext_ifaces im) mod ext_iface,
      im_PTE          = extendNameEnvList (im_PTE im) decls,
      im_iface_bytecode = case bytecode of
                            -- Don't add an entry if the iface doesn't have
                            -- 'extra_decls' so 'get_link_deps' knows that it
                            -- should load object code.
                            Nothing     -> im_iface_bytecode im
                            Just action -> extendModuleEnv (im_iface_bytecode im) mod action,
      im_rule_base    = extendRuleBaseList (im_rule_base im) rules,
      im_complete_matches
                      = im_complete_matches im ++ complete_matches,
      im_inst_env     = extendInstEnvList (im_inst_env im) insts,
      im_fam_inst_env = extendFamInstEnvList (im_fam_inst_env im) fam_insts,
      im_ann_env      = extendAnnEnvList (im_ann_env im) anns,
      im_mod_fam_inst_env
                      = extendModuleEnv (im_mod_fam_inst_env im) mod
                          (extendFamInstEnvList emptyFamInstEnv fam_insts),
      im_stats        = addEpsInStats (im_stats im)
                          (length decls) (length insts) (length rules),
      im_defaults     = extendModuleEnv (im_defaults im) mod defaults
    }
