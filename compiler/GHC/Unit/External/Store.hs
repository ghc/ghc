-- | The interface between GHC and an implementation of the
-- 'ExternalPackageState' (an "EPS store").
--
-- This module is meant for implementors of an alternative EPS store (for
-- example one that keeps the declarations on disk to use less memory). The rest
-- of GHC only uses the abstract interface exported by "GHC.Unit.External".
--
-- See Note [Abstract External Package State].
module GHC.Unit.External.Store
   ( ExternalPackageState (..)
   , EpsModule (..)
   , EpsStats (..)
   , emptyEpsStats
   , addEpsInStats
   , KnownKeyNameMaps
   )
where

import GHC.Prelude

import GHC.Unit
import GHC.Unit.External.Iface

import GHC.Core          ( CoreRule )
import GHC.Core.Rules    ( RuleBase )
import GHC.Core.FamInstEnv ( FamInst, FamInstEnv )
import GHC.Core.InstEnv  ( ClsInst, InstEnv )

import GHC.Iface.Syntax  ( IfaceDecl )

import GHC.Linker.Types  ( Linkable )

import GHC.Types.Annotations ( AnnEnv, Annotation )
import GHC.Types.CompleteMatch ( CompleteMatches )
import GHC.Types.DefaultEnv ( DefaultEnv )
import GHC.Types.Name
import GHC.Types.TyThing ( TyThing )
import GHC.Types.Unique.DSet

import GHC.Utils.Fingerprint ( Fingerprint )

{- Note [Abstract External Package State]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The External Package State (EPS) remembers what we have loaded from the
interfaces of modules in external units: their 'ExternalIface's, the
(lazily typechecked) declarations, and the instance/rule/annotation/... pools.

The EPS is an abstract handle: an 'ExternalPackageState' is a record of
operations. GHC only ever queries the EPS or adds a freshly loaded module to
it. This lets GHC API users install a different implementation (with
'GHC.setExternalPackageStore'), e.g. one that keeps the declarations of
loaded modules on disk and rehydrates them on demand, trading speed for
memory. GHC's own implementation keeps everything in memory, see
"GHC.Unit.External.InMemory".

There are two kinds of queries:

  * Keyed queries: the 'TyThing' of a 'Name', the 'ExternalIface' of a
    'Module', per-module data (family instances, defaults, bytecode), the known
    key maps and the free-holes cache.

  * Snapshot queries for the pure environments that are consumed by pure code
    (instance lookup, type family reduction, the rule base of the simplifier,
    annotations, COMPLETE pragmas). A store returns the whole environment.

The contract a store must obey:

  * 'eps_addModule' is atomic and insert-if-absent: with -j, several threads
    may load the same module concurrently. Only the first one wins; later
    calls for the same module must be ignored.

  * Lookups can be called while a module is being added, and while the
    declarations handed to 'eps_addModule' are forced: the knot of the lazily
    typechecked declarations is tied through 'eps_lookupTyThing' (see
    'GHC.IfaceToCore.tcIfaceGlobal'). So a store must make the new entries
    visible to lookups before returning from 'eps_addModule', and must not
    hold a lock while forcing a 'TyThing'.

  * A store may return a fresh copy of a 'TyThing' that it has returned
    before (e.g. after rehydrating it from disk). GHC compares 'TyCon's,
    'Id's etc. by their 'Unique', so this is correct, but it loses sharing.

  * Modules of home units are never added to the EPS (see
    Note [Demand-loaded home interfaces] in GHC.Unit.Home.LoadedState).
-}

-- | A handle to the External Package State.
-- See Note [Abstract External Package State].
data ExternalPackageState = EPS
  { -- Keyed queries

    eps_lookupTyThing    :: !(Name -> IO (Maybe TyThing))
      -- ^ The 'TyThing' of a 'Name' of an external module whose interface was
      -- loaded (formerly the Package Type Environment, @eps_PTE@).
  , eps_lookupIface      :: !(Module -> IO (Maybe ExternalIface))
      -- ^ The 'ExternalIface' of a loaded module, or the negative cache entry
      -- added by 'eps_addFailed'.
  , eps_moduleTyThings   :: !(Module -> IO [TyThing])
      -- ^ All the 'TyThing's of a loaded module (used for error messages and
      -- the GHC API, so it may be slow).
  , eps_lookupModFamInstEnv :: !(Module -> IO (Maybe FamInstEnv))
      -- ^ The family instances declared by a loaded module.
  , eps_lookupDefaults   :: !(Module -> IO (Maybe DefaultEnv))
      -- ^ The @default@ declarations exported by a loaded module.
  , eps_lookupBytecode   :: !(Module -> IO (Maybe (IO Linkable)))
      -- ^ If the interface of a loaded module was written with
      -- @-fwrite-if-simplified-core@, an action compiling bytecode from the
      -- Core bindings.
      -- See Note [Interface Files with Core Definitions]
  , eps_lookupKnownKeys  :: !(Module -> IO (Maybe (Fingerprint, KnownKeyNameMaps)))
      -- ^ Cache of the known-entity maps of 'GHC.Essentials' modules, keyed by
      -- the module; the 'Fingerprint' is the ABI hash of its interface.
      -- See 'GHC.Iface.Load.loadKnownKeyOccMaps'.
  , eps_lookupFreeHoles  :: !(InstalledModule -> IO (Maybe (UniqDSet ModuleName)))
      -- ^ Cache for 'mi_free_holes', see 'GHC.Iface.Load.moduleFreeHolesPrecise'.

    -- Snapshot queries

  , eps_getInstEnv       :: !(IO InstEnv)
      -- ^ The class instances of all loaded modules.
  , eps_getFamInstEnv    :: !(IO FamInstEnv)
      -- ^ The family instances of all loaded modules.
  , eps_getRuleBase      :: !(IO RuleBase)
      -- ^ The rules of all loaded modules, plus the built-in rules.
  , eps_getAnnEnv        :: !(IO AnnEnv)
      -- ^ The annotations of all loaded modules.
  , eps_getCompleteMatches :: !(IO CompleteMatches)
      -- ^ The COMPLETE pragmas of all loaded modules.
  , eps_getStats         :: !(IO EpsStats)

    -- Updates

  , eps_addModule        :: !(EpsModule -> IO ())
      -- ^ Add a freshly loaded module. Atomic and insert-if-absent: if the
      -- module is already present (or failed to load), do nothing.
  , eps_addFailed        :: !(Module -> IO ())
      -- ^ Remember that we failed to load the interface of this module, so we
      -- don't try again (unless it is already present). Afterwards,
      -- 'eps_lookupIface' returns @'emptyExternalIface' mod@.
  , eps_addBootDecls     :: !(Module -> [(Name, TyThing)] -> IO ())
      -- ^ Add only the declarations of a module (unless it is already present).
      -- See Note [Loading your own hi-boot file] in GHC.Iface.Load.
  , eps_addKnownKeys     :: !(Module -> (Fingerprint, KnownKeyNameMaps) -> IO ())
  , eps_addFreeHoles     :: !(InstalledModule -> UniqDSet ModuleName -> IO ())
  , eps_bumpDeclsOut     :: !(IO ())
      -- ^ Count a declaration that was typechecked ('n_decls_out').
  }

-- | A freshly loaded module, as handed to 'eps_addModule'.
--
-- The lazily typechecked fields ('epm_decls', 'epm_insts', ...) are what
-- GHC's in-memory store keeps. A store that keeps declarations elsewhere can
-- instead serialise 'epm_iface_decls' and use 'epm_rehydrate' to typecheck a
-- declaration again when it is looked up. Nothing in here retains the
-- 'GHC.Unit.Module.ModIface.ModIface' of the module once 'epm_iface_decls' is
-- forced.
data EpsModule = EpsModule
  { epm_module           :: !Module
  , epm_location         :: !ModLocation
      -- ^ Where the interface file was found
  , epm_ext_iface        :: !ExternalIface
  , epm_decls            :: [(Name, TyThing)]
      -- ^ The declarations, including implicit things (data constructors,
      -- record selectors, class methods, ...); each 'TyThing' is typechecked
      -- lazily. Forcing the list itself is cheap.
  , epm_insts            :: [ClsInst]
  , epm_fam_insts        :: [FamInst]
  , epm_rules            :: [CoreRule]
  , epm_anns             :: [Annotation]
  , epm_complete_matches :: CompleteMatches
  , epm_defaults         :: DefaultEnv
  , epm_bytecode         :: Maybe (IO Linkable)
      -- ^ See 'eps_lookupBytecode'
  , epm_iface_decls      :: [(Fingerprint, IfaceDecl)]
      -- ^ The declarations as found in the interface file (lazily
      -- deserialised), with their hashes ('mi_decls'). Forcing this list
      -- retains the declarations, but not the rest of the interface.
  , epm_rehydrate        :: IfaceDecl -> IO [(Name, TyThing)]
      -- ^ Typecheck one of the 'epm_iface_decls' again, in the context of
      -- this module. Returns the declared thing and its implicit things, like
      -- 'epm_decls' does (and like it, typechecks each 'TyThing' lazily).
      -- The result shares nothing with 'epm_decls'.
  }

-- | Accumulated statistics about what we are putting into the 'ExternalPackageState'.
-- \"In\" means stuff that is just /read/ from interface files,
-- \"Out\" means actually sucked in and type-checked
data EpsStats = EpsStats { n_ifaces_in
                         , n_decls_in, n_decls_out
                         , n_rules_in, n_rules_out
                         , n_insts_in, n_insts_out :: !Int }

-- | Statistics of an EPS with nothing loaded yet, but the given number of
-- built-in rules.
emptyEpsStats :: Int -> EpsStats
emptyEpsStats n_builtin_rules
  = EpsStats { n_ifaces_in = 0
             , n_decls_in  = 0
             , n_decls_out = 0
             , n_insts_in  = 0
             , n_insts_out = 0
             , n_rules_in  = n_builtin_rules
             , n_rules_out = 0
             }

addEpsInStats :: EpsStats -> Int -> Int -> Int -> EpsStats
-- ^ Add stats for one newly-read interface
addEpsInStats stats n_decls n_insts n_rules
  = stats { n_ifaces_in = n_ifaces_in stats + 1
          , n_decls_in  = n_decls_in stats + n_decls
          , n_insts_in  = n_insts_in stats + n_insts
          , n_rules_in  = n_rules_in stats + n_rules }
