{-# LANGUAGE BangPatterns #-}

-- | The slim, long-lived representation of an interface loaded into the
-- 'GHC.Unit.External.ExternalPackageState'.
--
-- See Note [The EPS does not keep ModIfaces].
module GHC.Unit.External.Iface
   ( ExternalIface
   , mkExternalIface
   , emptyExternalIface
   , ei_module
     -- * Interfaces returned by 'GHC.Iface.Load.loadInterface'
   , LoadedIface (..)
   , loadedFullIface
   , li_module
   , li_sig_of
   , li_semantic_module
   , li_hsc_src
   , li_boot
   , li_mnwib
   , li_deps
   , li_exports
   , li_fixities
   , li_fix_fn
   , li_fix
   , li_warns
   , li_decl_warn_fn
   , li_export_warn_fn
   , li_hash_fn
   , li_mod_hash
   , li_orphan
   , li_finsts
   , li_orphan_hash
   , li_orphan_like_hash
   , li_export_avails_hash
   , li_trust
   , li_trust_pkg
   , li_docs
   , li_free_holes
   )
where

import GHC.Prelude

import GHC.Hs.Doc ( Docs )
import GHC.Hs.Extension ( GhcRn )
import GHC.Unit
import GHC.Unit.Module.Deps
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Warnings
import GHC.Iface.Syntax ( IfaceWarnings, fromIfaceWarnings )

import GHC.Types.Fixity
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.SafeHaskell
import GHC.Types.SourceFile
import GHC.Types.Unique.DSet

import GHC.Utils.Fingerprint

import Control.DeepSeq

{- Note [The EPS does not keep ModIfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Historically the EPS had a Package Interface Table (eps_PIT :: ModuleEnv
ModIface) with the (trimmed) ModIface of every external module loaded.  That
retained the lazily deserialised ModIface, and with it (via the lazy
deserialisation thunks) the whole byte buffer of the .hi file.

Instead the EPS now keeps an 'ExternalIface' per loaded module in
'eps_ext_ifaces'.  It contains only what is needed of an external module after
its declarations, instances, rules etc. have been put into the EPS pools:
exports, fixities, warnings, ABI hashes, dependencies, trust and docs.  No
field may capture the ModIface it was made from:

  * Most fields are cheap to compute and forced when the 'ExternalIface' is
    built.
  * The exports and docs are lazily deserialised thunks, taken out of the
    ModIface by matching on its constructor. They were read with
    'lazyGetCopy', so each only retains its own (copied) slice of the .hi file
    (see Note [Copying lazily read slices] in GHC.Utils.Binary). Most loaded
    modules are never imported directly, so we never need their exports.
  * The hash cache ('ei_hash_fn') is the lazy 'mi_hash_fn' of the ModIface. It
    retains the declarations until it is forced, as it did in the PIT. Building
    it eagerly for every loaded interface was expensive (it creates the
    OccNames of all implicit binders), and it is only needed for the modules
    whose entities we use (for usages and recompilation checking).

Dropped, compared to the old PIT:
  * mi_top_env: callers (TH reifyModule, GHCi) re-read the interface from disk.
  * mi_usages and self-recompilation info: never needed for external modules.

'GHC.Iface.Load.loadInterface' returns a 'LoadedIface': either the ModIface of a
home module (from the HUG), or the 'ExternalIface' from the EPS.  The li_*
accessors work on both.

GHC API users who need the full interface (e.g. its declarations) can install
'GHC.Driver.Hooks.ifaceReadHook': it is called with the full ModIface every
time an interface file is read from disk.
-}

-- | What the EPS remembers about an external interface.
-- See Note [The EPS does not keep ModIfaces].
data ExternalIface = ExternalIface
  { ei_module           :: !Module
  , ei_sig_of           :: !(Maybe Module)
  , ei_hsc_src          :: !HscSource
  , ei_deps             :: !Dependencies
  , ei_exports          :: [IfaceExport]
    -- ^ Lazy: only needed for modules that are imported directly. Retains
    -- only its own (copied) slice of the .hi file, see 'mkExternalIface'.
  , ei_fixities         :: ![(OccName, Fixity)]
  , ei_fix_env          :: !(OccEnv Fixity)
  , ei_warns            :: !IfaceWarnings
  , ei_decl_warn_fn     :: !(OccName -> Maybe (WarningTxt GhcRn))
  , ei_export_warn_fn   :: !(Name -> Maybe (WarningTxt GhcRn))
  , ei_hash_fn          :: OccName -> Maybe (OccName, Fingerprint)
    -- ^ Lazy: the hash cache of the 'ModIface' ('mi_hash_fn'). Building it
    -- is expensive (it makes the 'OccName's of all implicit binders), and it
    -- is only needed for the modules whose entities we use.
  , ei_abi_hashes       :: !IfaceAbiHashes
  , ei_trust            :: !IfaceTrustInfo
  , ei_trust_pkg        :: !Bool
  , ei_docs             :: Maybe Docs
    -- ^ Lazy on purpose: see Note [The EPS does not keep ModIfaces]
  }

-- | Build the 'ExternalIface' of a freshly loaded interface.
--
-- Must be applied to an interface whose 'mi_decls' are still present (i.e.
-- before they are replaced by panics), since the hash cache is rebuilt from
-- them.  The result does not retain the 'ModIface'.
mkExternalIface :: ModIface -> ExternalIface
mkExternalIface iface =
  let !fixities  = force (mi_fixities iface)
      !fix_env   = mkOccEnv fixities
      !warns     = force (mi_warns iface)
      !warns_rn  = fromIfaceWarnings warns
      -- Lazy fields: we take the (unevaluated) fields out of the ModIface
      -- by matching on its constructor, so that the thunks we keep do not
      -- retain the ModIface. The exports were read with 'lazyGetCopy', so
      -- their thunk only retains their own bytes. The hash cache retains the
      -- (already forced) declarations, as it did in the PIT.
      exports   = lazyExports iface
      hash_fn   = lazyHashFn iface
      !deps      = force (mi_deps iface)
      !abi       = mi_abi_hashes iface
      !trust     = force (mi_trust iface)
      !docs      = lazyDocs iface
  in ExternalIface
      { ei_module         = mi_module iface
      , ei_sig_of         = mi_sig_of iface
      , ei_hsc_src        = mi_hsc_src iface
      , ei_deps           = deps
      , ei_exports        = exports
      , ei_fixities       = fixities
      , ei_fix_env        = fix_env
      , ei_warns          = warns
      , ei_decl_warn_fn   = strictDeclWarnFn warns_rn
      , ei_export_warn_fn = strictExportWarnFn warns_rn
      , ei_hash_fn        = hash_fn
      , ei_abi_hashes     = abi
      , ei_trust          = trust
      , ei_trust_pkg      = mi_trust_pkg iface
      , ei_docs           = docs
      }

-- | Extract the (possibly unevaluated) docs field without leaving a selector
-- thunk that would retain the whole 'ModIface'.
lazyDocs :: ModIface -> Maybe Docs
lazyDocs ModIface { mi_docs = d } = d

lazyExports :: ModIface -> [IfaceExport]
lazyExports ModIface { mi_exports = e } = e

lazyHashFn :: ModIface -> OccName -> Maybe (OccName, Fingerprint)
lazyHashFn ModIface { mi_hash_fn = f } = f

strictDeclWarnFn :: Warnings GhcRn -> OccName -> Maybe (WarningTxt GhcRn)
strictDeclWarnFn (WarnAll t)    = \_ -> Just t
strictDeclWarnFn (WarnSome vs _) =
  let !env = mkOccEnv vs in env `seq` lookupOccEnv env

strictExportWarnFn :: Warnings GhcRn -> Name -> Maybe (WarningTxt GhcRn)
strictExportWarnFn (WarnAll _)     = const Nothing
strictExportWarnFn (WarnSome _ ds) =
  let !env = mkNameEnv ds in env `seq` lookupNameEnv env

-- | The negative cache entry: we failed to load the interface of this module.
-- Mirrors 'emptyFullModIface'.
emptyExternalIface :: Module -> ExternalIface
emptyExternalIface mod = mkExternalIface (emptyFullModIface mod)

----------------------------------------------------------------------------
-- LoadedIface

-- | An interface as returned by 'GHC.Iface.Load.loadInterface'.
data LoadedIface
  = LoadedHome !ModIface
    -- ^ A home-unit module, from the HUG ('--make' / GHCi)
  | LoadedExternal !ExternalIface
    -- ^ A module whose interface was loaded into the EPS (external modules;
    -- in one-shot mode also home modules)

-- | The full 'ModIface', if we have one.
loadedFullIface :: LoadedIface -> Maybe ModIface
loadedFullIface (LoadedHome i)     = Just i
loadedFullIface (LoadedExternal _) = Nothing

li_module :: LoadedIface -> Module
li_module (LoadedHome i)     = mi_module i
li_module (LoadedExternal e) = ei_module e

li_sig_of :: LoadedIface -> Maybe Module
li_sig_of (LoadedHome i)     = mi_sig_of i
li_sig_of (LoadedExternal e) = ei_sig_of e

li_semantic_module :: LoadedIface -> Module
li_semantic_module li = case li_sig_of li of
  Nothing -> li_module li
  Just m  -> m

li_hsc_src :: LoadedIface -> HscSource
li_hsc_src (LoadedHome i)     = mi_hsc_src i
li_hsc_src (LoadedExternal e) = ei_hsc_src e

li_boot :: LoadedIface -> IsBootInterface
li_boot li = if li_hsc_src li == HsBootFile then IsBoot else NotBoot

li_mnwib :: LoadedIface -> ModuleNameWithIsBoot
li_mnwib li = GWIB (moduleName (li_module li)) (li_boot li)

li_deps :: LoadedIface -> Dependencies
li_deps (LoadedHome i)     = mi_deps i
li_deps (LoadedExternal e) = ei_deps e

li_exports :: LoadedIface -> [IfaceExport]
li_exports (LoadedHome i)     = mi_exports i
li_exports (LoadedExternal e) = ei_exports e

li_fixities :: LoadedIface -> [(OccName, Fixity)]
li_fixities (LoadedHome i)     = mi_fixities i
li_fixities (LoadedExternal e) = ei_fixities e

li_fix_fn :: LoadedIface -> OccName -> Maybe Fixity
li_fix_fn (LoadedHome i)     = mi_fix_fn i
li_fix_fn (LoadedExternal e) = lookupOccEnv (ei_fix_env e)

li_fix :: LoadedIface -> OccName -> Fixity
li_fix li occ = case li_fix_fn li occ of
  Just f  -> f
  Nothing -> defaultFixity

li_warns :: LoadedIface -> IfaceWarnings
li_warns (LoadedHome i)     = mi_warns i
li_warns (LoadedExternal e) = ei_warns e

li_decl_warn_fn :: LoadedIface -> OccName -> Maybe (WarningTxt GhcRn)
li_decl_warn_fn (LoadedHome i)     = mi_decl_warn_fn i
li_decl_warn_fn (LoadedExternal e) = ei_decl_warn_fn e

li_export_warn_fn :: LoadedIface -> Name -> Maybe (WarningTxt GhcRn)
li_export_warn_fn (LoadedHome i)     = mi_export_warn_fn i
li_export_warn_fn (LoadedExternal e) = ei_export_warn_fn e

li_hash_fn :: LoadedIface -> OccName -> Maybe (OccName, Fingerprint)
li_hash_fn (LoadedHome i)     = mi_hash_fn i
li_hash_fn (LoadedExternal e) = ei_hash_fn e

li_abi_hashes :: LoadedIface -> IfaceAbiHashes
li_abi_hashes (LoadedHome i)     = mi_abi_hashes i
li_abi_hashes (LoadedExternal e) = ei_abi_hashes e

li_mod_hash :: LoadedIface -> Fingerprint
li_mod_hash = mi_abi_mod_hash . li_abi_hashes

li_orphan :: LoadedIface -> WhetherHasOrphans
li_orphan = mi_abi_orphan . li_abi_hashes

li_finsts :: LoadedIface -> WhetherHasFamInst
li_finsts = mi_abi_finsts . li_abi_hashes

li_orphan_hash :: LoadedIface -> Fingerprint
li_orphan_hash = mi_abi_orphan_hash . li_abi_hashes

li_orphan_like_hash :: LoadedIface -> Fingerprint
li_orphan_like_hash = mi_abi_orphan_like_hash . li_abi_hashes

li_export_avails_hash :: LoadedIface -> Fingerprint
li_export_avails_hash = mi_abi_export_avails_hash . li_abi_hashes

li_trust :: LoadedIface -> IfaceTrustInfo
li_trust (LoadedHome i)     = mi_trust i
li_trust (LoadedExternal e) = ei_trust e

li_trust_pkg :: LoadedIface -> Bool
li_trust_pkg (LoadedHome i)     = mi_trust_pkg i
li_trust_pkg (LoadedExternal e) = ei_trust_pkg e

li_docs :: LoadedIface -> Maybe Docs
li_docs (LoadedHome i)     = mi_docs i
li_docs (LoadedExternal e) = ei_docs e

-- | See 'mi_free_holes'.
li_free_holes :: LoadedIface -> UniqDSet ModuleName
li_free_holes li =
  case getModuleInstantiation (li_module li) of
    (_, Just indef)
        -> renameFreeHoles (mkUniqDSet cands) (instUnitInsts (moduleUnit indef))
    _   -> emptyUniqDSet
  where
    cands = dep_sig_mods (li_deps li)
