{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module GHC.Unit.Module.ModIface
   ( ModIface
   , ModIface_
      ( mi_mod_info
      , mi_module
      , mi_sig_of
      , mi_hsc_src
      , mi_iface_hash
      , mi_deps
      , mi_public
      , mi_exports
      , mi_fixities
      , mi_warns
      , mi_anns
      , mi_decls
      , mi_defaults
      , mi_simplified_core
      , mi_top_env
      , mi_insts
      , mi_fam_insts
      , mi_rules
      , mi_trust
      , mi_trust_pkg
      , mi_complete_matches
      , mi_docs
      , mi_abi_hashes
      , mi_ext_fields
      , mi_hi_bytes
      , mi_self_recomp_info
      , mi_fix_fn
      , mi_decl_warn_fn
      , mi_export_warn_fn
      , mi_hash_fn
      )
   , pattern ModIface
   , set_mi_mod_info
   , set_mi_module
   , set_mi_sig_of
   , set_mi_hsc_src
   , set_mi_self_recomp
   , set_mi_hi_bytes
   , set_mi_deps
   , set_mi_exports
   , set_mi_fixities
   , set_mi_warns
   , set_mi_anns
   , set_mi_insts
   , set_mi_fam_insts
   , set_mi_rules
   , set_mi_decls
   , set_mi_defaults
   , set_mi_simplified_core
   , set_mi_top_env
   , set_mi_trust
   , set_mi_trust_pkg
   , set_mi_complete_matches
   , set_mi_docs
   , set_mi_abi_hashes
   , set_mi_ext_fields
   , set_mi_caches
   , set_mi_decl_warn_fn
   , set_mi_export_warn_fn
   , set_mi_fix_fn
   , set_mi_hash_fn
   , completePartialModIface
   , IfaceBinHandle(..)
   , PartialModIface
   , IfaceAbiHashes (..)
   , IfaceSelfRecomp (..)
   , IfaceCache (..)
   , IfaceSimplifiedCore (..)
   , withSelfRecomp
   , IfaceDeclExts
   , IfaceAbiHashesExts
   , IfaceExport
   , IfacePublic_(..)
   , IfacePublic
   , PartialIfacePublic
   , IfaceModInfo(..)
   , WhetherHasOrphans
   , WhetherHasFamInst
   , IfaceTopEnv (..)
   , IfaceImport(..)
   , mi_boot
   , mi_fix
   , mi_semantic_module
   , mi_mod_info_semantic_module
   , mi_free_holes
   , mi_mnwib
   , mi_flag_hash
   , mi_opt_hash
   , mi_hpc_hash
   , mi_plugin_hash
   , mi_src_hash
   , mi_usages
   , mi_mod_hash
   , mi_orphan
   , mi_finsts
   , mi_exp_hash
   , mi_orphan_hash
   , renameFreeHoles
   , emptyPartialModIface
   , emptyFullModIface
   , mkIfaceHashCache
   , emptyIfaceHashCache
   , forceModIface
   )
where

import GHC.Prelude

import GHC.Hs

import GHC.Iface.Syntax
import GHC.Iface.Flags
import GHC.Iface.Ext.Fields
import GHC.Iface.Recomp.Types

import GHC.Unit
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.WholeCoreBindings (IfaceForeign (..))


import GHC.Types.Avail
import GHC.Types.Fixity
import GHC.Types.Fixity.Env
import GHC.Types.Name
import GHC.Types.SafeHaskell
import GHC.Types.SourceFile
import GHC.Types.Unique.DSet
import GHC.Types.Unique.FM

import GHC.Data.Maybe
import qualified GHC.Data.Strict as Strict

import GHC.Utils.Fingerprint
import GHC.Utils.Binary

import Control.DeepSeq
import Control.Exception


{- Note [Interface file stages]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interface files have two possible stages.

* A partial stage built from the result of the core pipeline.
* A fully instantiated form. Which also includes fingerprints and
  potentially information provided by backends.

We can build a full interface file two ways:
* Directly from a partial one:
  Then we omit backend information and mostly compute fingerprints.
* From a partial one + information produced by a backend.
  Then we store the provided information and fingerprint both.
-}

type PartialModIface = ModIface_ 'ModIfaceCore
type ModIface = ModIface_ 'ModIfaceFinal

type PartialIfacePublic = IfacePublic_ 'ModIfaceCore
type IfacePublic = IfacePublic_ 'ModIfaceFinal

-- | Extends a PartialModIface with hashes of the ABI.
--
-- * The mi_mod_hash is the hash of the entire ABI
-- * THe other fields are more specific hashes of parts of the ABI
data IfaceAbiHashes = IfaceAbiHashes
  { mi_abi_mod_hash :: !Fingerprint
    -- ^ Hash of the ABI only
  , mi_abi_orphan :: !WhetherHasOrphans
    -- ^ Whether this module has orphans
  , mi_abi_finsts :: !WhetherHasFamInst
    -- ^ Whether this module has family instances. See Note [The type family
    -- instance consistency story].
  , mi_abi_exp_hash :: !Fingerprint
    -- ^ Hash of export list
  , mi_abi_orphan_hash :: !Fingerprint
    -- ^ Hash for orphan rules, class and family instances combined
    -- NOT transitive
  }

data IfaceCache = IfaceCache
  { mi_cache_decl_warn_fn :: !(OccName -> Maybe (WarningTxt GhcRn))
    -- ^ Cached lookup for 'mi_warns' for declaration deprecations
  , mi_cache_export_warn_fn :: !(Name -> Maybe (WarningTxt GhcRn))
    -- ^ Cached lookup for 'mi_warns' for export deprecations
  , mi_cache_fix_fn :: !(OccName -> Maybe Fixity)
    -- ^ Cached lookup for 'mi_fixities'
  , mi_cache_hash_fn :: !(OccName -> Maybe (OccName, Fingerprint))
    -- ^ Cached lookup for 'mi_decls'. The @Nothing@ in 'mi_hash_fn' means that
    -- the thing isn't in decls. It's useful to know that when seeing if we are
    -- up to date wrt. the old interface. The 'OccName' is the parent of the
    -- name, if it has one.
  }

data ModIfacePhase
  = ModIfaceCore
  -- ^ Partial interface built based on output of core pipeline.
  | ModIfaceFinal

-- | Selects a IfaceDecl representation.
-- For fully instantiated interfaces we also maintain
-- a fingerprint, which is used for recompilation checks.
type family IfaceDeclExts (phase :: ModIfacePhase) = decl | decl -> phase where
  IfaceDeclExts 'ModIfaceCore = IfaceDecl
  IfaceDeclExts 'ModIfaceFinal = (Fingerprint, IfaceDecl)

type family IfaceAbiHashesExts (phase :: ModIfacePhase) = bk | bk -> phase where
  IfaceAbiHashesExts 'ModIfaceCore = ()
  IfaceAbiHashesExts 'ModIfaceFinal = IfaceAbiHashes

-- | In-memory byte array representation of a 'ModIface'.
--
-- See Note [Sharing of ModIface] for why we need this.
data IfaceBinHandle (phase :: ModIfacePhase) where
  -- | A partial 'ModIface' cannot be serialised to disk.
  PartialIfaceBinHandle :: IfaceBinHandle 'ModIfaceCore
  -- | Optional 'FullBinData' that can be serialised to disk directly.
  --
  -- See Note [Private fields in ModIface] for when this fields needs to be cleared
  -- (e.g., set to 'Nothing').
  FullIfaceBinHandle :: !(Strict.Maybe FullBinData) -> IfaceBinHandle 'ModIfaceFinal


withSelfRecomp :: ModIface_ phase -> r -> (IfaceSelfRecomp -> r) -> r
withSelfRecomp iface nk jk =
  case mi_self_recomp_info iface of
    Nothing -> nk
    Just x -> jk x



-- | A 'ModIface' summarises everything we know
-- about a compiled module.
--
-- See Note [Structure of ModIface] for information about what belongs in each field.
--
-- See Note [Strictness in ModIface] to learn about why all the fields are lazy.
--
-- See Note [Private fields in ModIface] to learn why we don't export any of the
-- fields.
data ModIface_ (phase :: ModIfacePhase)
  = PrivateModIface {
        mi_hi_bytes_ :: !(IfaceBinHandle phase),
                -- ^ A serialised in-memory buffer of this 'ModIface'.
                -- If this handle is given, we can avoid serialising the 'ModIface'
                -- when writing this 'ModIface' to disk, and write this buffer to disk instead.
                -- See Note [Sharing of ModIface].
        mi_iface_hash_  :: Fingerprint, -- A hash of the whole interface

        mi_mod_info_     :: IfaceModInfo,
                -- ^ Meta information about the module the interface file is for

        mi_deps_     :: Dependencies,
                -- ^ The dependencies of the module.  This is
                -- consulted for directly-imported modules, but not
                -- for anything else (hence lazy)
                -- MP: Needs to be refactored (#25844)

        mi_public_ :: IfacePublic_ phase,
                -- ^ The parts of interface which are used by other modules when
                -- importing this module. The main, original part of an interface.


        mi_self_recomp_ :: Maybe IfaceSelfRecomp,
                -- ^ Information needed for checking self-recompilation.
                -- See Note [Self recompilation information in interface files]

        mi_simplified_core_ :: Maybe IfaceSimplifiedCore,
                -- ^ The part of the interface written when `-fwrite-if-simplified-core` is enabled.
                -- These parts are used to restart bytecode generation.

        mi_docs_ :: Maybe Docs,
                -- ^ Docstrings and related data for use by haddock, the ghci
                -- @:doc@ command, and other tools.
                --
                -- @Just _@ @<=>@ the module was built with @-haddock@.

        mi_top_env_  :: IfaceTopEnv,
                -- ^ Just enough information to reconstruct the top level environment in
                -- the /original source/ code for this module. which
                -- is NOT the same as mi_exports, nor mi_decls (which
                -- may contains declarations for things not actually
                -- defined by the user).  Used for GHCi and for inspecting
                -- the contents of modules via the GHC API only.

        mi_ext_fields_ :: ExtensibleFields
                -- ^ Additional optional fields, where the Map key represents
                -- the field name, resulting in a (size, serialized data) pair.
                -- Because the data is intended to be serialized through the
                -- internal `Binary` class (increasing compatibility with types
                -- using `Name` and `FastString`, such as HIE), this format is
                -- chosen over `ByteString`s.
     }

-- | Meta information about the module the interface file is for
data IfaceModInfo = IfaceModInfo {
  mi_mod_info_module :: Module, -- ^ Name of the module we are for
  mi_mod_info_sig_of :: Maybe Module, -- ^ Are we a sig of another mod?
  mi_mod_info_hsc_src :: HscSource -- ^ Boot? Signature?
}

-- | The public interface of a module which are used by other modules when importing this module.
-- The ABI of a module.
data IfacePublic_ phase = IfacePublic {
        mi_exports_  :: [IfaceExport],
                -- ^ Exports
                -- Kept sorted by (mod,occ), to make version comparisons easier
                -- Records the modules that are the declaration points for things
                -- exported by this module, and the 'OccName's of those things

        mi_fixities_ :: [(OccName,Fixity)],
                -- ^ Fixities
                -- NOT STRICT!  we read this field lazily from the interface file

        mi_warns_    :: IfaceWarnings,
                -- ^ Warnings
                -- NOT STRICT!  we read this field lazily from the interface file

        mi_anns_     :: [IfaceAnnotation],
                -- ^ Annotations
                -- NOT STRICT!  we read this field lazily from the interface file


        mi_decls_    :: [IfaceDeclExts phase],
                -- ^ Type, class and variable declarations
                -- The hash of an Id changes if its fixity or deprecations change
                --      (as well as its type of course)
                -- Ditto data constructors, class operations, except that
                -- the hash of the parent class/tycon changes


        mi_defaults_ :: [IfaceDefault],
                -- ^ default declarations exported by the module


                -- Instance declarations and rules
        mi_insts_       :: [IfaceClsInst],     -- ^ Sorted class instance
        mi_fam_insts_   :: [IfaceFamInst],  -- ^ Sorted family instances
        mi_rules_       :: [IfaceRule],     -- ^ Sorted rules


        mi_trust_     :: IfaceTrustInfo,
                -- ^ Safe Haskell Trust information for this module.

        mi_trust_pkg_ :: Bool,
                -- ^ Do we require the package this module resides in be trusted
                -- to trust this module? This is used for the situation where a
                -- module is Safe (so doesn't require the package be trusted
                -- itself) but imports some trustworthy modules from its own
                -- package (which does require its own package be trusted).
                -- See Note [Trust Own Package] in GHC.Rename.Names
        mi_complete_matches_ :: [IfaceCompleteMatch],
                -- ^ {-# COMPLETE #-} declarations

        mi_caches_ :: IfaceCache,
                -- ^ Cached lookups of some parts of mi_public

        mi_abi_hashes_ :: (IfaceAbiHashesExts phase)
                -- ^ Either `()` or `IfaceAbiHashes` for
                -- a fully instantiated interface.
                -- These fields are hashes of different parts of the public interface.
}

mkIfacePublic :: [IfaceExport]
                  -> [IfaceDeclExts 'ModIfaceFinal]
                  -> [(OccName, Fixity)]
                  -> IfaceWarnings
                  -> [IfaceAnnotation]
                  -> [IfaceDefault]
                  -> [IfaceClsInst]
                  -> [IfaceFamInst]
                  -> [IfaceRule]
                  -> IfaceTrustInfo
                  -> Bool
                  -> [IfaceCompleteMatch]
                  -> IfaceAbiHashes
                  -> IfacePublic
mkIfacePublic exports decls fixities warns anns defaults insts fam_insts rules trust trust_pkg complete_matches abi_hashes = IfacePublic {
  mi_exports_ = exports,
  mi_decls_ = decls,
  mi_fixities_ = fixities,
  mi_warns_ = warns,
  mi_anns_ = anns,
  mi_defaults_ = defaults,
  mi_insts_ = insts,
  mi_fam_insts_ = fam_insts,
  mi_rules_ = rules,
  mi_trust_ = trust,
  mi_trust_pkg_ = trust_pkg,
  mi_complete_matches_ = complete_matches,
  mi_caches_ = IfaceCache {
    mi_cache_decl_warn_fn = mkIfaceDeclWarnCache $ fromIfaceWarnings warns,
    mi_cache_export_warn_fn = mkIfaceExportWarnCache $ fromIfaceWarnings warns,
    mi_cache_fix_fn = mkIfaceFixCache fixities,
    mi_cache_hash_fn = mkIfaceHashCache decls
  },
  mi_abi_hashes_ = abi_hashes
}

-- | The information needed to restart bytecode generation.
-- Enabled by `-fwrite-if-simplified-core`.
data IfaceSimplifiedCore = IfaceSimplifiedCore {
  mi_sc_extra_decls :: [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo]
  -- ^ Extra variable definitions which are **NOT** exposed but when
  -- combined with mi_decls allows us to restart code generation.
  -- See Note [Interface Files with Core Definitions] and Note [Interface File with Core: Sharing RHSs]
  , mi_sc_foreign :: IfaceForeign
  -- ^ Foreign stubs and files to supplement 'mi_extra_decls_'.
  -- See Note [Foreign stubs and TH bytecode linking]
}

-- Enough information to reconstruct the top level environment for a module
data IfaceTopEnv
  = IfaceTopEnv
  { ifaceTopExports :: DetOrdAvails -- ^ all top level things in this module, including unexported stuff
  , ifaceImports :: [IfaceImport]    -- ^ all the imports in this module
  }

instance NFData IfaceTopEnv where
  rnf (IfaceTopEnv a b) = rnf a `seq` rnf b

instance Binary IfaceTopEnv where
  put_ bh (IfaceTopEnv exports imports) = do
    put_ bh exports
    put_ bh imports
  get bh = do
    exports <- get bh
    imports <- get bh
    return (IfaceTopEnv exports imports)


{-
Note [Structure of ModIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ModIface structure is divided into several logical parts:

1. mi_mod_info: Basic module metadata (name, version, etc.)

2. mi_public: The public interface of the module, which includes:
   - Exports, declarations, fixities, warnings, annotations
   - Class and type family instances
   - Rewrite rules and COMPLETE pragmas
   - Safe Haskell and package trust information
   - ABI hashes for recompilation checking

4. mi_self_recomp: Information needed for self-recompilation checking
   (see Note [Self recompilation information in interface files])

5. mi_simplified_core: Optional simplified Core for bytecode generation
   (only present when -fwrite-if-simplified-core is enabled)

6. mi_docs: Optional documentation (only present when -haddock is enabled)

7. mi_top_env: Information about the top-level environment of the original source

8. mi_ext_fields: Additional fields for extensibility

This structure helps organize the interface data according to its purpose and usage
patterns. Different parts of the compiler use different fields. By separating them
logically in the interface we can arrange to only deserialize the fields that are needed.

Note [Strictness in ModIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ModIface is the Haskell representation of an interface (.hi) file.

* During compilation we write out ModIface values to disk for files
  that we have just compiled
* For packages that we depend on we load the ModIface from disk.

All fields in the ModIface are deliberately lazy because when we read
an interface file we don't always need all the parts. For example, an
interface file contains information about documentation which is often
not needed during compilation. This is achieved using the lazyPut/lazyGet pair.
If the field was strict then we would pointlessly load this information into memory.

On the other hand, if we create a ModIface but **don't** write it to
disk then to avoid space leaks we need to make sure to deepseq all these lazy fields
because the ModIface might live for a long time (for instance in a GHCi session).
That's why in GHC.Driver.Main.hscMaybeWriteIface there is the call to
forceModIface.
-}

mi_flag_hash :: ModIface_ phase -> Maybe (FingerprintWithValue IfaceDynFlags)
mi_flag_hash = fmap mi_sr_flag_hash . mi_self_recomp_

mi_opt_hash :: ModIface_ phase -> Maybe Fingerprint
mi_opt_hash = fmap mi_sr_opt_hash . mi_self_recomp_

mi_hpc_hash :: ModIface_ phase -> Maybe Fingerprint
mi_hpc_hash = fmap mi_sr_hpc_hash . mi_self_recomp_

mi_src_hash :: ModIface_ phase -> Maybe Fingerprint
mi_src_hash = fmap mi_sr_src_hash . mi_self_recomp_

mi_usages :: ModIface_ phase -> Maybe [Usage]
mi_usages = fmap mi_sr_usages . mi_self_recomp_

mi_plugin_hash :: ModIface_ phase -> Maybe Fingerprint
mi_plugin_hash = fmap mi_sr_plugin_hash . mi_self_recomp_

-- | Accessor for the module hash of the ABI from a ModIface.
mi_mod_hash :: ModIface -> Fingerprint
mi_mod_hash iface = mi_abi_mod_hash (mi_abi_hashes iface)

-- | Accessor for whether this module has orphans from a ModIface.
mi_orphan :: ModIface -> WhetherHasOrphans
mi_orphan iface = mi_abi_orphan (mi_abi_hashes iface)

-- | Accessor for whether this module has family instances from a ModIface.
mi_finsts :: ModIface -> WhetherHasFamInst
mi_finsts iface = mi_abi_finsts (mi_abi_hashes iface)

-- | Accessor for the hash of the export list from a ModIface.
mi_exp_hash :: ModIface -> Fingerprint
mi_exp_hash iface = mi_abi_exp_hash (mi_abi_hashes iface)

-- | Accessor for the hash of orphan rules, class and family instances combined from a ModIface.
mi_orphan_hash :: ModIface -> Fingerprint
mi_orphan_hash iface = mi_abi_orphan_hash (mi_abi_hashes iface)

-- | Old-style accessor for whether or not the ModIface came from an hs-boot
-- file.
mi_boot :: ModIface -> IsBootInterface
mi_boot iface = if mi_hsc_src iface == HsBootFile
    then IsBoot
    else NotBoot

mi_mnwib :: ModIface -> ModuleNameWithIsBoot
mi_mnwib iface = GWIB (moduleName $ mi_module iface) (mi_boot iface)

-- | Lookups up a (possibly cached) fixity from a 'ModIface'. If one cannot be
-- found, 'defaultFixity' is returned instead.
mi_fix :: ModIface -> OccName -> Fixity
mi_fix iface name = mi_fix_fn iface name `orElse` defaultFixity

-- | The semantic module for this interface; e.g., if it's a interface
-- for a signature, if 'mi_module' is @p[A=<A>]:A@, 'mi_semantic_module'
-- will be @<A>@.
mi_mod_info_semantic_module :: IfaceModInfo -> Module
mi_mod_info_semantic_module iface = case mi_mod_info_sig_of iface of
                            Nothing -> mi_mod_info_module iface
                            Just mod -> mod

mi_semantic_module :: ModIface_ a -> Module
mi_semantic_module iface = mi_mod_info_semantic_module (mi_mod_info iface)

-- | The "precise" free holes, e.g., the signatures that this
-- 'ModIface' depends on.
mi_free_holes :: ModIface -> UniqDSet ModuleName
mi_free_holes iface =
  case getModuleInstantiation (mi_module iface) of
    (_, Just indef)
        -- A mini-hack: we rely on the fact that 'renameFreeHoles'
        -- drops things that aren't holes.
        -> renameFreeHoles (mkUniqDSet cands) (instUnitInsts (moduleUnit indef))
    _   -> emptyUniqDSet
  where
    cands = dep_sig_mods $ mi_deps iface

-- | Given a set of free holes, and a unit identifier, rename
-- the free holes according to the instantiation of the unit
-- identifier.  For example, if we have A and B free, and
-- our unit identity is @p[A=<C>,B=impl:B]@, the renamed free
-- holes are just C.
renameFreeHoles :: UniqDSet ModuleName -> [(ModuleName, Module)] -> UniqDSet ModuleName
renameFreeHoles fhs insts =
    unionManyUniqDSets (map lookup_impl (uniqDSetToList fhs))
  where
    hmap = listToUFM insts
    lookup_impl mod_name
        | Just mod <- lookupUFM hmap mod_name = moduleFreeHoles mod
        -- It wasn't actually a hole
        | otherwise                           = emptyUniqDSet

-- See Note [Strictness in ModIface] about where we use lazyPut vs put
instance Binary ModIface where
   put_ bh (PrivateModIface
                { mi_hi_bytes_  = _hi_bytes, -- We don't serialise the 'mi_hi_bytes_', as it itself
                                            -- may contain an in-memory byte array buffer for this
                                            -- 'ModIface'. If we used 'put_' on this 'ModIface', then
                                            -- we likely have a good reason, and do not want to reuse
                                            -- the byte array.
                                            -- See Note [Private fields in ModIface]
                 mi_mod_info_    = mod_info,
                 mi_iface_hash_ = iface_hash,
                 mi_deps_      = deps,
                 mi_public_    = public,
                 mi_top_env_    = top_env,
                 mi_docs_      = docs,
                 mi_ext_fields_ = _ext_fields, -- Don't `put_` this in the instance so we
                                              -- can deal with it's pointer in the header
                                              -- when we write the actual file
                 mi_self_recomp_ = self_recomp,
                 mi_simplified_core_ = simplified_core
                 }) = do
        put_ bh mod_info
        put_ bh iface_hash
        lazyPut bh deps
        lazyPut bh public
        lazyPut bh top_env
        lazyPutMaybe bh docs
        lazyPutMaybe bh self_recomp
        lazyPutMaybe bh simplified_core

   get bh = do
        mod_info    <- get bh
        iface_hash  <- get bh
        deps        <- lazyGet bh
        public      <- lazyGet bh
        top_env     <- lazyGet bh
        docs        <- lazyGetMaybe bh
        self_recomp <- lazyGetMaybe bh
        simplified_core <- lazyGetMaybe bh

        return (PrivateModIface {
                 mi_mod_info_   = mod_info,
                 mi_iface_hash_ = iface_hash,
                 mi_deps_        = deps,
                 mi_public_      = public,
                 mi_simplified_core_ = simplified_core,
                 mi_docs_        = docs,
                 mi_top_env_     = top_env,
                 mi_self_recomp_ = self_recomp,
                -- placeholder because this is dealt
                -- with specially when the file is read
                 mi_ext_fields_  = emptyExtensibleFields,
                 -- We can't populate this field here, as we are
                 -- missing the 'mi_ext_fields_' field, which is
                 -- handled in 'getIfaceWithExtFields'.
                 mi_hi_bytes_    = FullIfaceBinHandle Strict.Nothing
                 })

instance Binary IfaceModInfo where
  put_ bh (IfaceModInfo { mi_mod_info_module = mod
                        , mi_mod_info_sig_of = sig_of
                        , mi_mod_info_hsc_src = hsc_src
                        }) = do
    put_ bh mod
    put_ bh sig_of
    put_ bh hsc_src

  get bh = do
    mod <- get bh
    sig_of <- get bh
    hsc_src <- get bh
    return (IfaceModInfo { mi_mod_info_module = mod
                         , mi_mod_info_sig_of = sig_of
                         , mi_mod_info_hsc_src = hsc_src
                         })


instance Binary (IfacePublic_ 'ModIfaceFinal) where
  put_ bh (IfacePublic { mi_exports_ = exports
                       , mi_decls_ = decls
                       , mi_fixities_ = fixities
                       , mi_warns_ = warns
                       , mi_anns_ = anns
                       , mi_defaults_ = defaults
                       , mi_insts_ = insts
                       , mi_fam_insts_ = fam_insts
                       , mi_rules_ = rules
                       , mi_trust_ = trust
                       , mi_trust_pkg_ = trust_pkg
                       , mi_complete_matches_ = complete_matches
                       , mi_abi_hashes_ = abi_hashes
                       }) = do

    lazyPut bh exports
    lazyPut bh decls
    lazyPut bh fixities
    lazyPut bh warns
    lazyPut bh anns
    lazyPut bh defaults
    lazyPut bh insts
    lazyPut bh fam_insts
    lazyPut bh rules
    lazyPut bh trust
    lazyPut bh trust_pkg
    lazyPut bh complete_matches
    lazyPut bh abi_hashes

  get bh = do
    exports <- lazyGet bh
    decls <- lazyGet bh
    fixities <- lazyGet bh
    warns <- lazyGet bh
    anns <- lazyGet bh
    defaults <- lazyGet bh
    insts <- lazyGet bh
    fam_insts <- lazyGet bh
    rules <- lazyGet bh
    trust <- lazyGet bh
    trust_pkg <- lazyGet bh
    complete_matches <- lazyGet bh
    abi_hashes <- lazyGet bh
    return (mkIfacePublic exports decls fixities warns anns defaults insts fam_insts rules trust trust_pkg complete_matches abi_hashes)

instance Binary IfaceAbiHashes where
  put_ bh (IfaceAbiHashes { mi_abi_mod_hash = mod_hash
                              , mi_abi_orphan = orphan
                              , mi_abi_finsts = hasFamInsts
                              , mi_abi_exp_hash = exp_hash
                              , mi_abi_orphan_hash = orphan_hash
                              }) = do
    put_ bh mod_hash
    put_ bh orphan
    put_ bh hasFamInsts
    put_ bh exp_hash
    put_ bh orphan_hash
  get bh =  do
    mod_hash <- get bh
    orphan <- get bh
    hasFamInsts <- get bh
    exp_hash <- get bh
    orphan_hash <- get bh
    return $ IfaceAbiHashes  {
                   mi_abi_mod_hash = mod_hash,
                   mi_abi_orphan = orphan,
                   mi_abi_finsts = hasFamInsts,
                   mi_abi_exp_hash = exp_hash,
                   mi_abi_orphan_hash = orphan_hash
                   }

instance Binary IfaceSimplifiedCore where
  put_ bh (IfaceSimplifiedCore eds fs) = do
    put_ bh eds
    put_ bh fs

  get bh = do
    eds <- get bh
    fs <- get bh
    return (IfaceSimplifiedCore eds fs)

emptyPartialModIface :: Module -> PartialModIface
emptyPartialModIface mod
  = PrivateModIface
      { mi_mod_info_    = emptyIfaceModInfo mod,
        mi_iface_hash_  = fingerprint0,
        mi_hi_bytes_    = PartialIfaceBinHandle,
        mi_deps_        = noDependencies,
        mi_public_      = emptyPublicModIface (),
        mi_simplified_core_ = Nothing,
        mi_top_env_     = IfaceTopEnv emptyDetOrdAvails [] ,
        mi_docs_        = Nothing,
        mi_self_recomp_ = Nothing,
        mi_ext_fields_ = emptyExtensibleFields

      }

emptyIfaceModInfo :: Module -> IfaceModInfo
emptyIfaceModInfo mod = IfaceModInfo
  { mi_mod_info_module = mod
  , mi_mod_info_sig_of = Nothing
  , mi_mod_info_hsc_src = HsSrcFile
  }


emptyPublicModIface :: IfaceAbiHashesExts phase -> IfacePublic_ phase
emptyPublicModIface abi_hashes = IfacePublic
  { mi_exports_ = []
  , mi_decls_ = []
  , mi_fixities_ = []
  , mi_warns_ = IfWarnSome [] []
  , mi_anns_ = []
  , mi_defaults_ = []
  , mi_insts_ = []
  , mi_fam_insts_ = []
  , mi_rules_ = []
  , mi_abi_hashes_ = abi_hashes
  , mi_trust_ = noIfaceTrustInfo
  , mi_trust_pkg_ = False
  , mi_caches_ = emptyModIfaceCache
  , mi_complete_matches_ = []
  }

emptyModIfaceCache :: IfaceCache
emptyModIfaceCache = IfaceCache {
  mi_cache_decl_warn_fn = emptyIfaceWarnCache,
  mi_cache_export_warn_fn = emptyIfaceWarnCache,
  mi_cache_fix_fn = emptyIfaceFixCache,
  mi_cache_hash_fn = emptyIfaceHashCache
}

emptyIfaceBackend :: IfaceAbiHashes
emptyIfaceBackend = IfaceAbiHashes
        { mi_abi_mod_hash = fingerprint0,
          mi_abi_orphan = False,
          mi_abi_finsts = False,
          mi_abi_exp_hash = fingerprint0,
          mi_abi_orphan_hash = fingerprint0
        }

emptyFullModIface :: Module -> ModIface
emptyFullModIface mod =
    (emptyPartialModIface mod)
      { mi_public_ = emptyPublicModIface emptyIfaceBackend
      , mi_hi_bytes_ = FullIfaceBinHandle Strict.Nothing
      }


-- | Constructs cache for the 'mi_hash_fn' field of a 'ModIface'
mkIfaceHashCache :: [(Fingerprint,IfaceDecl)]
                 -> (OccName -> Maybe (OccName, Fingerprint))
mkIfaceHashCache pairs
  = \occ -> lookupOccEnv env occ
  where
    env = foldl' add_decl emptyOccEnv pairs
    add_decl env0 (v,d) = foldl' add env0 (ifaceDeclFingerprints v d)
      where
        add env0 (occ,hash) = extendOccEnv env0 occ (occ,hash)

emptyIfaceHashCache :: OccName -> Maybe (OccName, Fingerprint)
emptyIfaceHashCache _occ = Nothing

-- ModIface is completely forced since it will live in memory for a long time.
-- If forcing it uses a lot of memory, then store less things in ModIface.
instance ( NFData (IfaceAbiHashesExts (phase :: ModIfacePhase))
         , NFData (IfaceDeclExts (phase :: ModIfacePhase))
         ) => NFData (ModIface_ phase) where
  rnf (PrivateModIface a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    = (a1 :: IfaceBinHandle phase)
    `seq` rnf a2
    `seq` rnf a3
    `seq` rnf a4
    `seq` rnf a5
    `seq` rnf a6
    `seq` rnf a7
    `seq` rnf a8
    `seq` rnf a9
    `seq` rnf a10

instance NFData IfaceModInfo where
  rnf (IfaceModInfo a1 a2 a3)
    =  rnf a1
    `seq` rnf a2
    `seq` rnf a3


instance NFData IfaceSimplifiedCore where
  rnf (IfaceSimplifiedCore eds fs) = rnf eds `seq` rnf fs

instance NFData IfaceAbiHashes where
  rnf (IfaceAbiHashes a1 a2 a3 a4 a5)
    =  rnf a1
    `seq` rnf a2
    `seq` rnf a3
    `seq` rnf a4
    `seq` rnf a5

instance (NFData (IfaceAbiHashesExts phase), NFData (IfaceDeclExts phase)) => NFData (IfacePublic_ phase) where
  rnf (IfacePublic a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
    =  rnf a1
    `seq` rnf a2
    `seq` rnf a3
    `seq` rnf a4
    `seq` rnf a5
    `seq` rnf a6
    `seq` rnf a7
    `seq` rnf a8
    `seq` rnf a9
    `seq` rnf a10
    `seq` rnf a11
    `seq` rnf a12
    `seq` rnf a13
    `seq` rnf a14

instance NFData IfaceCache where
  rnf (IfaceCache a1 a2 a3 a4)
    =  rnf a1
    `seq` rnf a2
    `seq` rnf a3
    `seq` rnf a4



forceModIface :: ModIface -> IO ()
forceModIface iface = () <$ (evaluate $ force iface)

-- | Records whether a module has orphans. An \"orphan\" is one of:
--
-- * An instance declaration in a module other than the definition
--   module for one of the type constructors or classes in the instance head
--
-- * A rewrite rule in a module other than the one defining
--   the function in the head of the rule
--
type WhetherHasOrphans   = Bool

-- | Does this module define family instances?
type WhetherHasFamInst = Bool

-- ----------------------------------------------------------------------------
-- Modify a 'ModIface'.
-- ----------------------------------------------------------------------------

{-
Note [Private fields in ModIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The fields of 'ModIface' are private, e.g., not exported, to make the API
impossible to misuse. A 'ModIface' can be "compressed" in-memory using
'shareIface', which serialises the 'ModIface' to an in-memory buffer.
This has the advantage of reducing memory usage of 'ModIface', reducing the
overall memory usage of GHC.
See Note [Sharing of ModIface].

This in-memory buffer can be reused, if and only if the 'ModIface' is not
modified after it has been "compressed"/shared via 'shareIface'. Instead of
serialising 'ModIface', we simply write the in-memory buffer to disk directly.

However, we can't rely that a 'ModIface' isn't modified after 'shareIface' has
been called. Thus, we make all fields of 'ModIface' private and modification
only happens via exported update functions, such as 'set_mi_decls'.
These functions unconditionally clear any in-memory buffer if used, forcing us
to serialise the 'ModIface' to disk again.
-}

-- | Given a 'PartialModIface', turn it into a 'ModIface' by completing
-- missing fields.
completePartialModIface :: PartialModIface
  -> Fingerprint
  -> [(Fingerprint, IfaceDecl)]
  -> Maybe IfaceSimplifiedCore
  -> IfaceAbiHashes
  -> IfaceCache
  -> ModIface
completePartialModIface partial iface_hash decls extra_decls final_exts cache = partial
  { mi_public_ = completePublicModIface decls final_exts cache (mi_public_ partial)
  , mi_simplified_core_ = extra_decls
  , mi_hi_bytes_ = FullIfaceBinHandle Strict.Nothing
  , mi_iface_hash_ = iface_hash
  }
  where

-- | Given a 'PartialIfacePublic', turn it into an 'IfacePublic' by completing
-- missing fields.
completePublicModIface :: [(Fingerprint, IfaceDecl)]
                       -> IfaceAbiHashes
                       -> IfaceCache
                       -> PartialIfacePublic
                       -> IfacePublic
completePublicModIface decls abi_hashes cache partial = partial
  { mi_decls_ = decls
  , mi_abi_hashes_  = abi_hashes
  , mi_caches_ = cache
  }

set_mi_mod_info :: IfaceModInfo -> ModIface_ phase -> ModIface_ phase
set_mi_mod_info val iface = clear_mi_hi_bytes $ iface { mi_mod_info_ = val }

set_mi_self_recomp :: Maybe IfaceSelfRecomp-> ModIface_ phase -> ModIface_ phase
set_mi_self_recomp val iface = clear_mi_hi_bytes $ iface { mi_self_recomp_ = val }

set_mi_hi_bytes :: IfaceBinHandle phase -> ModIface_ phase -> ModIface_ phase
set_mi_hi_bytes val iface = iface { mi_hi_bytes_ = val }

set_mi_deps :: Dependencies -> ModIface_ phase -> ModIface_ phase
set_mi_deps val iface = clear_mi_hi_bytes $ iface { mi_deps_ = val }

set_mi_public :: (IfacePublic_ phase -> IfacePublic_ phase) -> ModIface_ phase -> ModIface_ phase
set_mi_public f iface = clear_mi_hi_bytes $ iface { mi_public_ = f (mi_public_ iface) }

set_mi_simplified_core :: Maybe IfaceSimplifiedCore -> ModIface_ phase -> ModIface_ phase
set_mi_simplified_core val iface = clear_mi_hi_bytes $ iface { mi_simplified_core_ = val }

set_mi_top_env :: IfaceTopEnv -> ModIface_ phase -> ModIface_ phase
set_mi_top_env val iface = clear_mi_hi_bytes $ iface { mi_top_env_ = val }

set_mi_docs :: Maybe Docs -> ModIface_ phase -> ModIface_ phase
set_mi_docs val iface = clear_mi_hi_bytes $  iface { mi_docs_ = val }

set_mi_ext_fields :: ExtensibleFields -> ModIface_ phase -> ModIface_ phase
set_mi_ext_fields val iface = clear_mi_hi_bytes $ iface { mi_ext_fields_ = val }

{- Settings for mi_public interface fields -}

set_mi_exports :: [IfaceExport] -> ModIface_ phase -> ModIface_ phase
set_mi_exports val = set_mi_public (\iface -> iface { mi_exports_ = val })

set_mi_fixities :: [(OccName, Fixity)] -> ModIface_ phase -> ModIface_ phase
set_mi_fixities val = set_mi_public (\iface -> iface { mi_fixities_ = val })

set_mi_warns :: IfaceWarnings -> ModIface_ phase -> ModIface_ phase
set_mi_warns val = set_mi_public (\iface -> iface { mi_warns_ = val })

set_mi_anns :: [IfaceAnnotation] -> ModIface_ phase -> ModIface_ phase
set_mi_anns val = set_mi_public (\iface -> iface { mi_anns_ = val })

set_mi_insts :: [IfaceClsInst] -> ModIface_ phase -> ModIface_ phase
set_mi_insts val = set_mi_public (\iface -> iface { mi_insts_ = val })

set_mi_fam_insts :: [IfaceFamInst] -> ModIface_ phase -> ModIface_ phase
set_mi_fam_insts val = set_mi_public (\iface -> iface { mi_fam_insts_ = val })

set_mi_rules :: [IfaceRule] -> ModIface_ phase -> ModIface_ phase
set_mi_rules val = set_mi_public (\iface -> iface { mi_rules_ = val })

set_mi_decls :: [IfaceDeclExts phase] -> ModIface_ phase -> ModIface_ phase
set_mi_decls val = set_mi_public (\iface -> iface { mi_decls_ = val })

set_mi_defaults :: [IfaceDefault] -> ModIface_ phase -> ModIface_ phase
set_mi_defaults val = set_mi_public (\iface -> iface { mi_defaults_ = val })

set_mi_trust :: IfaceTrustInfo -> ModIface_ phase -> ModIface_ phase
set_mi_trust val = set_mi_public (\iface -> iface { mi_trust_ = val })

set_mi_trust_pkg :: Bool -> ModIface_ phase -> ModIface_ phase
set_mi_trust_pkg val = set_mi_public (\iface -> iface { mi_trust_pkg_ = val })

set_mi_complete_matches :: [IfaceCompleteMatch] -> ModIface_ phase -> ModIface_ phase
set_mi_complete_matches val = set_mi_public (\iface -> iface { mi_complete_matches_ = val })

set_mi_abi_hashes :: IfaceAbiHashesExts phase -> ModIface_ phase -> ModIface_ phase
set_mi_abi_hashes val = set_mi_public (\iface -> iface { mi_abi_hashes_ = val })

{- Setters for mi_caches interface fields -}

set_mi_decl_warn_fn :: (OccName -> Maybe (WarningTxt GhcRn)) -> ModIface_ phase -> ModIface_ phase
set_mi_decl_warn_fn val = set_mi_public (\iface -> iface { mi_caches_ = (mi_caches_ iface) { mi_cache_decl_warn_fn = val } })

set_mi_export_warn_fn :: (Name -> Maybe (WarningTxt GhcRn)) -> ModIface_ phase -> ModIface_ phase
set_mi_export_warn_fn val = set_mi_public (\iface -> iface { mi_caches_ = (mi_caches_ iface) { mi_cache_export_warn_fn = val } })

set_mi_fix_fn :: (OccName -> Maybe Fixity) -> ModIface_ phase -> ModIface_ phase
set_mi_fix_fn val = set_mi_public (\iface -> iface { mi_caches_ = (mi_caches_ iface) { mi_cache_fix_fn = val } })

set_mi_hash_fn :: (OccName -> Maybe (OccName, Fingerprint)) -> ModIface_ phase -> ModIface_ phase
set_mi_hash_fn val = set_mi_public (\iface -> iface { mi_caches_ = (mi_caches_ iface) { mi_cache_hash_fn = val } })

set_mi_caches :: IfaceCache -> ModIface_ phase -> ModIface_ phase
set_mi_caches val = set_mi_public (\iface -> iface { mi_caches_ = val })

{-

-}

{- Setters for mi_mod_info interface fields -}

set_mi_module :: Module -> ModIface_ phase -> ModIface_ phase
set_mi_module val = set_mi_mod_info_field (\info -> info { mi_mod_info_module = val })

set_mi_sig_of :: Maybe Module -> ModIface_ phase -> ModIface_ phase
set_mi_sig_of val = set_mi_mod_info_field (\info -> info { mi_mod_info_sig_of = val })

set_mi_hsc_src :: HscSource -> ModIface_ phase -> ModIface_ phase
set_mi_hsc_src val = set_mi_mod_info_field (\info -> info { mi_mod_info_hsc_src = val })

-- | Helper function for setting fields in mi_mod_info_
set_mi_mod_info_field :: (IfaceModInfo -> IfaceModInfo) -> ModIface_ phase -> ModIface_ phase
set_mi_mod_info_field f iface = clear_mi_hi_bytes $ iface { mi_mod_info_ = f (mi_mod_info_ iface) }




-- | Invalidate any byte array buffer we might have.
clear_mi_hi_bytes :: ModIface_ phase -> ModIface_ phase
clear_mi_hi_bytes iface = iface
  { mi_hi_bytes_ = case mi_hi_bytes iface of
      PartialIfaceBinHandle -> PartialIfaceBinHandle
      FullIfaceBinHandle _ -> FullIfaceBinHandle Strict.Nothing
  }

-- ----------------------------------------------------------------------------
-- 'ModIface' pattern synonyms to keep breakage low.
-- ----------------------------------------------------------------------------

{-
Note [Inline Pattern synonym of ModIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The introduction of the 'ModIface' pattern synonym originally caused an increase
in allocated bytes in multiple performance tests.
In some benchmarks, it was a 2~3% increase.

Without {-# INLINE ModIface #-}, the generated core reveals the reason for this increase.
We show the core for the 'mi_module' record selector:

@
  mi_module
    = \ @phase iface -> $w$mModIface iface mi_module1

  $w$mModIface
    = \ @phase iface cont ->
        case iface of
        { PrivateModIface a b ... z ->
        cont
          a
          b
          ...
          z
        }

  mi_module1
    = \ @phase
        a
        _
        ...
        _ ->
        a
@

Thus, we can see the '$w$mModIface' is not inlined, leading to an increase in
the allocated bytes.

However, with the pragma, the correct core is generated:

@
  mi_module = mi_module_
@

-}

-- See Note [Inline Pattern synonym of ModIface] for why we have all these
-- inline pragmas.
{-# INLINE mi_mod_info #-}
{-# INLINE mi_iface_hash #-}
{-# INLINE mi_module #-}
{-# INLINE mi_sig_of #-}
{-# INLINE mi_hsc_src #-}
{-# INLINE mi_deps #-}
{-# INLINE mi_public #-}
{-# INLINE mi_exports #-}
{-# INLINE mi_fixities #-}
{-# INLINE mi_warns #-}
{-# INLINE mi_anns #-}
{-# INLINE mi_decls #-}
{-# INLINE mi_simplified_core #-}
{-# INLINE mi_defaults #-}
{-# INLINE mi_top_env #-}
{-# INLINE mi_insts #-}
{-# INLINE mi_fam_insts #-}
{-# INLINE mi_rules #-}
{-# INLINE mi_trust #-}
{-# INLINE mi_trust_pkg #-}
{-# INLINE mi_complete_matches #-}
{-# INLINE mi_docs #-}
{-# INLINE mi_abi_hashes #-}
{-# INLINE mi_ext_fields #-}
{-# INLINE mi_hi_bytes #-}
{-# INLINE mi_self_recomp_info #-}
{-# INLINE mi_fix_fn #-}
{-# INLINE mi_hash_fn #-}
{-# INLINE mi_decl_warn_fn #-}
{-# INLINE mi_export_warn_fn #-}
{-# INLINE ModIface #-}
{-# COMPLETE ModIface #-}

pattern ModIface ::
  IfaceModInfo
  -> Module
  -> Maybe Module
  -> HscSource
  -> Fingerprint
  -> Dependencies
  -> IfacePublic_ phase
  -> [IfaceExport]
  -> [(OccName, Fixity)]
  -> IfaceWarnings
  -> [IfaceAnnotation]
  -> [IfaceDeclExts phase]
  -> Maybe IfaceSimplifiedCore
  -> [IfaceDefault]
  -> IfaceTopEnv
  -> [IfaceClsInst]
  -> [IfaceFamInst]
  -> [IfaceRule]
  -> IfaceTrustInfo
  -> Bool
  -> [IfaceCompleteMatch]
  -> Maybe Docs
  -> IfaceAbiHashesExts phase
  -> ExtensibleFields
  -> IfaceBinHandle phase
  -> Maybe IfaceSelfRecomp
  -> (OccName -> Maybe Fixity)
  -> (OccName -> Maybe (OccName, Fingerprint))
  -> (OccName -> Maybe (WarningTxt GhcRn))
  -> (Name -> Maybe (WarningTxt GhcRn)) ->
  ModIface_ phase
pattern ModIface
  { mi_mod_info
  , mi_module
  , mi_sig_of
  , mi_hsc_src
  , mi_iface_hash
  , mi_deps
  , mi_public
  , mi_exports
  , mi_fixities
  , mi_warns
  , mi_anns
  , mi_decls
  , mi_simplified_core
  , mi_defaults
  , mi_top_env
  , mi_insts
  , mi_fam_insts
  , mi_rules
  , mi_trust
  , mi_trust_pkg
  , mi_complete_matches
  , mi_docs
  , mi_abi_hashes
  , mi_ext_fields
  , mi_hi_bytes
  , mi_self_recomp_info
  , mi_fix_fn
  , mi_hash_fn
  , mi_decl_warn_fn
  , mi_export_warn_fn
  } <- PrivateModIface
    { mi_mod_info_ = mi_mod_info@IfaceModInfo { mi_mod_info_module = mi_module
                                              , mi_mod_info_sig_of = mi_sig_of
                                              , mi_mod_info_hsc_src = mi_hsc_src }
    , mi_iface_hash_ = mi_iface_hash
    , mi_deps_ = mi_deps
    , mi_public_ = mi_public@IfacePublic {
        mi_exports_ = mi_exports
      , mi_fixities_ = mi_fixities
      , mi_warns_ = mi_warns
      , mi_anns_ = mi_anns
      , mi_decls_ = mi_decls
      , mi_defaults_ = mi_defaults
      , mi_insts_ = mi_insts
      , mi_fam_insts_ = mi_fam_insts
      , mi_rules_ = mi_rules
      , mi_trust_ = mi_trust
      , mi_trust_pkg_ = mi_trust_pkg
      , mi_complete_matches_ = mi_complete_matches
      , mi_caches_ = IfaceCache {
          mi_cache_decl_warn_fn = mi_decl_warn_fn,
          mi_cache_export_warn_fn = mi_export_warn_fn,
          mi_cache_fix_fn = mi_fix_fn,
          mi_cache_hash_fn = mi_hash_fn
        }
      , mi_abi_hashes_ = mi_abi_hashes
    }
    , mi_docs_ = mi_docs
    , mi_ext_fields_ = mi_ext_fields
    , mi_hi_bytes_ = mi_hi_bytes
    , mi_self_recomp_ = mi_self_recomp_info
    , mi_simplified_core_ = mi_simplified_core
    , mi_top_env_ = mi_top_env
    }
