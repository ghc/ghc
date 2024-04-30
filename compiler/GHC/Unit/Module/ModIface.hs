{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}


module GHC.Unit.Module.ModIface
   ( ModIface
   , ModIface_
      ( mi_module
      , mi_sig_of
      , mi_hsc_src
      , mi_deps
      , mi_usages
      , mi_exports
      , mi_used_th
      , mi_fixities
      , mi_warns
      , mi_anns
      , mi_decls
      , mi_extra_decls
      , mi_stub_objs
      , mi_top_env
      , mi_insts
      , mi_fam_insts
      , mi_rules
      , mi_hpc
      , mi_trust
      , mi_trust_pkg
      , mi_complete_matches
      , mi_docs
      , mi_final_exts
      , mi_ext_fields
      , mi_src_hash
      , mi_hi_bytes
      )
   , pattern ModIface
   , restoreFromOldModIface
   , addSourceFingerprint
   , set_mi_module
   , set_mi_sig_of
   , set_mi_hsc_src
   , set_mi_src_hash
   , set_mi_hi_bytes
   , set_mi_deps
   , set_mi_usages
   , set_mi_exports
   , set_mi_used_th
   , set_mi_fixities
   , set_mi_warns
   , set_mi_anns
   , set_mi_insts
   , set_mi_fam_insts
   , set_mi_rules
   , set_mi_decls
   , set_mi_extra_decls
   , set_mi_stub_objs
   , set_mi_top_env
   , set_mi_hpc
   , set_mi_trust
   , set_mi_trust_pkg
   , set_mi_complete_matches
   , set_mi_docs
   , set_mi_final_exts
   , set_mi_ext_fields
   , completePartialModIface
   , IfaceBinHandle(..)
   , PartialModIface
   , ModIfaceBackend (..)
   , IfaceDeclExts
   , IfaceBackendExts
   , IfaceExport
   , WhetherHasOrphans
   , WhetherHasFamInst
   , IfaceTopEnv (..)
   , IfaceImport(..)
   , mi_boot
   , mi_fix
   , mi_semantic_module
   , mi_free_holes
   , mi_mnwib
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
import GHC.Iface.Ext.Fields

import GHC.Unit
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Warnings

import GHC.Types.Avail
import GHC.Types.Fixity
import GHC.Types.Fixity.Env
import GHC.Types.HpcInfo
import GHC.Types.Name
import GHC.Types.Name.Reader (IfGlobalRdrEnv)
import GHC.Types.SafeHaskell
import GHC.Types.SourceFile
import GHC.Types.Unique.DSet
import GHC.Types.Unique.FM

import GHC.Data.Maybe

import GHC.Utils.Fingerprint
import GHC.Utils.Binary

import Control.DeepSeq
import Control.Exception
import qualified GHC.Data.Strict as Strict
import Data.ByteString (ByteString)

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

-- | Extends a PartialModIface with information which is either:
-- * Computed after codegen
-- * Or computed just before writing the iface to disk. (Hashes)
-- In order to fully instantiate it.
data ModIfaceBackend = ModIfaceBackend
  { mi_iface_hash :: !Fingerprint
    -- ^ Hash of the whole interface
  , mi_mod_hash :: !Fingerprint
    -- ^ Hash of the ABI only
  , mi_flag_hash :: !Fingerprint
    -- ^ Hash of the important flags used when compiling the module, excluding
    -- optimisation flags
  , mi_opt_hash :: !Fingerprint
    -- ^ Hash of optimisation flags
  , mi_hpc_hash :: !Fingerprint
    -- ^ Hash of hpc flags
  , mi_plugin_hash :: !Fingerprint
    -- ^ Hash of plugins
  , mi_orphan :: !WhetherHasOrphans
    -- ^ Whether this module has orphans
  , mi_finsts :: !WhetherHasFamInst
    -- ^ Whether this module has family instances. See Note [The type family
    -- instance consistency story].
  , mi_exp_hash :: !Fingerprint
    -- ^ Hash of export list
  , mi_orphan_hash :: !Fingerprint
    -- ^ Hash for orphan rules, class and family instances combined

    -- Cached environments for easy lookup. These are computed (lazily) from
    -- other fields and are not put into the interface file.
    -- Not really produced by the backend but there is no need to create them
    -- any earlier.
  , mi_decl_warn_fn :: !(OccName -> Maybe (WarningTxt GhcRn))
    -- ^ Cached lookup for 'mi_warns' for declaration deprecations
  , mi_export_warn_fn :: !(Name -> Maybe (WarningTxt GhcRn))
    -- ^ Cached lookup for 'mi_warns' for export deprecations
  , mi_fix_fn :: !(OccName -> Maybe Fixity)
    -- ^ Cached lookup for 'mi_fixities'
  , mi_hash_fn :: !(OccName -> Maybe (OccName, Fingerprint))
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

type family IfaceBackendExts (phase :: ModIfacePhase) = bk | bk -> phase where
  IfaceBackendExts 'ModIfaceCore = ()
  IfaceBackendExts 'ModIfaceFinal = ModIfaceBackend

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

-- | A 'ModIface' plus a 'ModDetails' summarises everything we know
-- about a compiled module.  The 'ModIface' is the stuff *before* linking,
-- and can be written out to an interface file. The 'ModDetails is after
-- linking and can be completely recovered from just the 'ModIface'.
--
-- When we read an interface file, we also construct a 'ModIface' from it,
-- except that we explicitly make the 'mi_decls' and a few other fields empty;
-- as when reading we consolidate the declarations etc. into a number of indexed
-- maps and environments in the 'ExternalPackageState'.
--
-- See Note [Strictness in ModIface] to learn about why some fields are
-- strict and others are not.
--
-- See Note [Private fields in ModIface] to learn why we don't export any of the
-- fields.
data ModIface_ (phase :: ModIfacePhase)
  = PrivateModIface {
        mi_module_     :: !Module,             -- ^ Name of the module we are for
        mi_sig_of_     :: !(Maybe Module),     -- ^ Are we a sig of another mod?

        mi_hsc_src_    :: !HscSource,          -- ^ Boot? Signature?

        mi_deps_     :: Dependencies,
                -- ^ The dependencies of the module.  This is
                -- consulted for directly-imported modules, but not
                -- for anything else (hence lazy)

        mi_usages_   :: [Usage],
                -- ^ Usages; kept sorted so that it's easy to decide
                -- whether to write a new iface file (changing usages
                -- doesn't affect the hash of this module)
                -- NOT STRICT!  we read this field lazily from the interface file
                -- It is *only* consulted by the recompilation checker

        mi_exports_  :: ![IfaceExport],
                -- ^ Exports
                -- Kept sorted by (mod,occ), to make version comparisons easier
                -- Records the modules that are the declaration points for things
                -- exported by this module, and the 'OccName's of those things


        mi_used_th_  :: !Bool,
                -- ^ Module required TH splices when it was compiled.
                -- This disables recompilation avoidance (see #481).

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

        mi_extra_decls_ :: Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo],
                -- ^ Extra variable definitions which are **NOT** exposed but when
                -- combined with mi_decls allows us to restart code generation.
                -- See Note [Interface Files with Core Definitions] and Note [Interface File with Core: Sharing RHSs]

        mi_stub_objs_ :: ![ByteString],
                -- ^ Serialized foreign stub dynamic objects when
                -- compiled with -fbyte-code-and-object-code, empty
                -- and unused in other cases. This is required to make
                -- whole core bindings properly work with foreign
                -- stubs (see #24634).

        mi_top_env_  :: !(Maybe IfaceTopEnv),
                -- ^ Just enough information to reconstruct the top level environment in
                -- the /original source/ code for this module. which
                -- is NOT the same as mi_exports, nor mi_decls (which
                -- may contains declarations for things not actually
                -- defined by the user).  Used for GHCi and for inspecting
                -- the contents of modules via the GHC API only.
                --
                -- (We need the source file to figure out the
                -- top-level environment, if we didn't compile this module
                -- from source then this field contains @Nothing@).
                --
                -- Strictly speaking this field should live in the
                -- 'HomeModInfo', but that leads to more plumbing.

                -- Instance declarations and rules
        mi_insts_       :: [IfaceClsInst],     -- ^ Sorted class instance
        mi_fam_insts_   :: [IfaceFamInst],  -- ^ Sorted family instances
        mi_rules_       :: [IfaceRule],     -- ^ Sorted rules

        mi_hpc_       :: !AnyHpcUsage,
                -- ^ True if this program uses Hpc at any point in the program.

        mi_trust_     :: !IfaceTrustInfo,
                -- ^ Safe Haskell Trust information for this module.

        mi_trust_pkg_ :: !Bool,
                -- ^ Do we require the package this module resides in be trusted
                -- to trust this module? This is used for the situation where a
                -- module is Safe (so doesn't require the package be trusted
                -- itself) but imports some trustworthy modules from its own
                -- package (which does require its own package be trusted).
                -- See Note [Trust Own Package] in GHC.Rename.Names
        mi_complete_matches_ :: ![IfaceCompleteMatch],

        mi_docs_ :: !(Maybe Docs),
                -- ^ Docstrings and related data for use by haddock, the ghci
                -- @:doc@ command, and other tools.
                --
                -- @Just _@ @<=>@ the module was built with @-haddock@.

        mi_final_exts_ :: !(IfaceBackendExts phase),
                -- ^ Either `()` or `ModIfaceBackend` for
                -- a fully instantiated interface.

        mi_ext_fields_ :: !ExtensibleFields,
                -- ^ Additional optional fields, where the Map key represents
                -- the field name, resulting in a (size, serialized data) pair.
                -- Because the data is intended to be serialized through the
                -- internal `Binary` class (increasing compatibility with types
                -- using `Name` and `FastString`, such as HIE), this format is
                -- chosen over `ByteString`s.
                --

        mi_src_hash_ :: !Fingerprint,
                -- ^ Hash of the .hs source, used for recompilation checking.
        mi_hi_bytes_ :: !(IfaceBinHandle phase)
                -- ^ A serialised in-memory buffer of this 'ModIface'.
                -- If this handle is given, we can avoid serialising the 'ModIface'
                -- when writing this 'ModIface' to disk, and write this buffer to disk instead.
                -- See Note [Sharing of ModIface].
     }

-- Enough information to reconstruct the top level environment for a module
data IfaceTopEnv
  = IfaceTopEnv
  { ifaceTopExports :: !IfGlobalRdrEnv -- ^ all top level things in this module, including unexported stuff
  , ifaceImports :: ![IfaceImport]    -- ^ all the imports in this module
  }

instance NFData IfaceTopEnv where
  rnf (IfaceTopEnv a b) = rnf a `seq` rnf b

{-
Note [Strictness in ModIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ModIface is the Haskell representation of an interface (.hi) file.

* During compilation we write out ModIface values to disk for files
  that we have just compiled
* For packages that we depend on we load the ModIface from disk.

Some fields in the ModIface are deliberately lazy because when we read
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
mi_fix iface name = mi_fix_fn (mi_final_exts iface) name `orElse` defaultFixity

-- | The semantic module for this interface; e.g., if it's a interface
-- for a signature, if 'mi_module' is @p[A=<A>]:A@, 'mi_semantic_module'
-- will be @<A>@.
mi_semantic_module :: ModIface_ a -> Module
mi_semantic_module iface = case mi_sig_of iface of
                            Nothing -> mi_module iface
                            Just mod -> mod

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
   put_ bh (PrivateModIface {
                 mi_module_    = mod,
                 mi_sig_of_    = sig_of,
                 mi_hsc_src_   = hsc_src,
                 mi_src_hash_ = _src_hash, -- Don't `put_` this in the instance
                                          -- because we are going to write it
                                          -- out separately in the actual file
                 mi_hi_bytes_  = _hi_bytes, -- We don't serialise the 'mi_hi_bytes_', as it itself
                                            -- may contain an in-memory byte array buffer for this
                                            -- 'ModIface'. If we used 'put_' on this 'ModIface', then
                                            -- we likely have a good reason, and do not want to reuse
                                            -- the byte array.
                                            -- See Note [Private fields in ModIface]
                 mi_deps_      = deps,
                 mi_usages_    = usages,
                 mi_exports_   = exports,
                 mi_used_th_   = used_th,
                 mi_fixities_  = fixities,
                 mi_warns_     = warns,
                 mi_anns_      = anns,
                 mi_decls_     = decls,
                 mi_extra_decls_ = extra_decls,
                 mi_stub_objs_ = stub_objs,
                 mi_insts_     = insts,
                 mi_fam_insts_ = fam_insts,
                 mi_rules_     = rules,
                 mi_hpc_       = hpc_info,
                 mi_trust_     = trust,
                 mi_trust_pkg_ = trust_pkg,
                 mi_complete_matches_ = complete_matches,
                 mi_docs_      = docs,
                 mi_ext_fields_ = _ext_fields, -- Don't `put_` this in the instance so we
                                              -- can deal with it's pointer in the header
                                              -- when we write the actual file
                 mi_final_exts_ = ModIfaceBackend {
                   mi_iface_hash = iface_hash,
                   mi_mod_hash = mod_hash,
                   mi_flag_hash = flag_hash,
                   mi_opt_hash = opt_hash,
                   mi_hpc_hash = hpc_hash,
                   mi_plugin_hash = plugin_hash,
                   mi_orphan = orphan,
                   mi_finsts = hasFamInsts,
                   mi_exp_hash = exp_hash,
                   mi_orphan_hash = orphan_hash
                 }}) = do
        put_ bh mod
        put_ bh sig_of
        put_ bh hsc_src
        put_ bh iface_hash
        put_ bh mod_hash
        put_ bh flag_hash
        put_ bh opt_hash
        put_ bh hpc_hash
        put_ bh plugin_hash
        put_ bh orphan
        put_ bh hasFamInsts
        lazyPut bh deps
        lazyPut bh usages
        put_ bh exports
        put_ bh exp_hash
        put_ bh used_th
        put_ bh fixities
        lazyPut bh warns
        lazyPut bh anns
        put_ bh decls
        put_ bh extra_decls
        put_ bh stub_objs
        put_ bh insts
        put_ bh fam_insts
        lazyPut bh rules
        put_ bh orphan_hash
        put_ bh hpc_info
        put_ bh trust
        put_ bh trust_pkg
        put_ bh complete_matches
        lazyPutMaybe bh docs

   get bh = do
        mod         <- get bh
        sig_of      <- get bh
        hsc_src     <- get bh
        iface_hash  <- get bh
        mod_hash    <- get bh
        flag_hash   <- get bh
        opt_hash    <- get bh
        hpc_hash    <- get bh
        plugin_hash <- get bh
        orphan      <- get bh
        hasFamInsts <- get bh
        deps        <- lazyGet bh
        usages      <- {-# SCC "bin_usages" #-} lazyGet bh
        exports     <- {-# SCC "bin_exports" #-} get bh
        exp_hash    <- get bh
        used_th     <- get bh
        fixities    <- {-# SCC "bin_fixities" #-} get bh
        warns       <- {-# SCC "bin_warns" #-} lazyGet bh
        anns        <- {-# SCC "bin_anns" #-} lazyGet bh
        decls       <- {-# SCC "bin_tycldecls" #-} get bh
        extra_decls <- get bh
        stub_objs   <- get bh
        insts       <- {-# SCC "bin_insts" #-} get bh
        fam_insts   <- {-# SCC "bin_fam_insts" #-} get bh
        rules       <- {-# SCC "bin_rules" #-} lazyGet bh
        orphan_hash <- get bh
        hpc_info    <- get bh
        trust       <- get bh
        trust_pkg   <- get bh
        complete_matches <- get bh
        docs        <- lazyGetMaybe bh
        return (PrivateModIface {
                 mi_module_      = mod,
                 mi_sig_of_      = sig_of,
                 mi_hsc_src_     = hsc_src,
                 mi_src_hash_ = fingerprint0, -- placeholder because this is dealt
                                             -- with specially when the file is read
                 mi_hi_bytes_    =
                                   -- We can't populate this field here, as we are
                                   -- missing the 'mi_ext_fields_' field, which is
                                   -- handled in 'getIfaceWithExtFields'.
                                   FullIfaceBinHandle Strict.Nothing,
                 mi_deps_        = deps,
                 mi_usages_      = usages,
                 mi_exports_     = exports,
                 mi_used_th_     = used_th,
                 mi_anns_        = anns,
                 mi_fixities_    = fixities,
                 mi_warns_       = warns,
                 mi_decls_       = decls,
                 mi_extra_decls_ = extra_decls,
                 mi_stub_objs_   = stub_objs,
                 mi_top_env_     = Nothing,
                 mi_insts_       = insts,
                 mi_fam_insts_   = fam_insts,
                 mi_rules_       = rules,
                 mi_hpc_         = hpc_info,
                 mi_trust_       = trust,
                 mi_trust_pkg_   = trust_pkg,
                        -- And build the cached values
                 mi_complete_matches_ = complete_matches,
                 mi_docs_        = docs,
                 mi_ext_fields_  = emptyExtensibleFields, -- placeholder because this is dealt
                                                         -- with specially when the file is read
                 mi_final_exts_ = ModIfaceBackend {
                   mi_iface_hash = iface_hash,
                   mi_mod_hash = mod_hash,
                   mi_flag_hash = flag_hash,
                   mi_opt_hash = opt_hash,
                   mi_hpc_hash = hpc_hash,
                   mi_plugin_hash = plugin_hash,
                   mi_orphan = orphan,
                   mi_finsts = hasFamInsts,
                   mi_exp_hash = exp_hash,
                   mi_orphan_hash = orphan_hash,
                   mi_decl_warn_fn = mkIfaceDeclWarnCache $ fromIfaceWarnings warns,
                   mi_export_warn_fn = mkIfaceExportWarnCache $ fromIfaceWarnings warns,
                   mi_fix_fn = mkIfaceFixCache fixities,
                   mi_hash_fn = mkIfaceHashCache decls
                 }})


-- | The original names declared of a certain module that are exported
type IfaceExport = AvailInfo

emptyPartialModIface :: Module -> PartialModIface
emptyPartialModIface mod
  = PrivateModIface
      { mi_module_      = mod,
        mi_sig_of_      = Nothing,
        mi_hsc_src_     = HsSrcFile,
        mi_src_hash_    = fingerprint0,
        mi_hi_bytes_    = PartialIfaceBinHandle,
        mi_deps_        = noDependencies,
        mi_usages_      = [],
        mi_exports_     = [],
        mi_used_th_     = False,
        mi_fixities_    = [],
        mi_warns_       = IfWarnSome [] [],
        mi_anns_        = [],
        mi_insts_       = [],
        mi_fam_insts_   = [],
        mi_rules_       = [],
        mi_decls_       = [],
        mi_extra_decls_ = Nothing,
        mi_stub_objs_   = [],
        mi_top_env_     = Nothing,
        mi_hpc_         = False,
        mi_trust_       = noIfaceTrustInfo,
        mi_trust_pkg_   = False,
        mi_complete_matches_ = [],
        mi_docs_        = Nothing,
        mi_final_exts_  = (),
        mi_ext_fields_  = emptyExtensibleFields
      }

emptyFullModIface :: Module -> ModIface
emptyFullModIface mod =
    (emptyPartialModIface mod)
      { mi_decls_ = []
      , mi_hi_bytes_ = FullIfaceBinHandle Strict.Nothing
      , mi_final_exts_ = ModIfaceBackend
        { mi_iface_hash = fingerprint0,
          mi_mod_hash = fingerprint0,
          mi_flag_hash = fingerprint0,
          mi_opt_hash = fingerprint0,
          mi_hpc_hash = fingerprint0,
          mi_plugin_hash = fingerprint0,
          mi_orphan = False,
          mi_finsts = False,
          mi_exp_hash = fingerprint0,
          mi_orphan_hash = fingerprint0,
          mi_decl_warn_fn = emptyIfaceWarnCache,
          mi_export_warn_fn = emptyIfaceWarnCache,
          mi_fix_fn = emptyIfaceFixCache,
          mi_hash_fn = emptyIfaceHashCache } }

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

-- Take care, this instance only forces to the degree necessary to
-- avoid major space leaks.
instance ( NFData (IfaceBackendExts (phase :: ModIfacePhase))
         , NFData (IfaceDeclExts (phase :: ModIfacePhase))
         ) => NFData (ModIface_ phase) where
  rnf (PrivateModIface
               { mi_module_, mi_sig_of_, mi_hsc_src_, mi_hi_bytes_, mi_deps_, mi_usages_
               , mi_exports_, mi_used_th_, mi_fixities_, mi_warns_, mi_anns_
               , mi_decls_, mi_extra_decls_, mi_stub_objs_, mi_top_env_, mi_insts_
               , mi_fam_insts_, mi_rules_, mi_hpc_, mi_trust_, mi_trust_pkg_
               , mi_complete_matches_, mi_docs_, mi_final_exts_
               , mi_ext_fields_, mi_src_hash_ })
    =     rnf mi_module_
    `seq` rnf mi_sig_of_
    `seq`     mi_hsc_src_
    `seq`     mi_hi_bytes_
    `seq`     mi_deps_
    `seq`     mi_usages_
    `seq`     mi_exports_
    `seq` rnf mi_used_th_
    `seq`     mi_fixities_
    `seq` rnf mi_warns_
    `seq` rnf mi_anns_
    `seq` rnf mi_decls_
    `seq` rnf mi_extra_decls_
    `seq` rnf mi_stub_objs_
    `seq` rnf mi_top_env_
    `seq` rnf mi_insts_
    `seq` rnf mi_fam_insts_
    `seq` rnf mi_rules_
    `seq` rnf mi_hpc_
    `seq`     mi_trust_
    `seq` rnf mi_trust_pkg_
    `seq` rnf mi_complete_matches_
    `seq` rnf mi_docs_
    `seq`     mi_final_exts_
    `seq`     mi_ext_fields_
    `seq` rnf mi_src_hash_
    `seq` ()

instance NFData (ModIfaceBackend) where
  rnf (ModIfaceBackend{ mi_iface_hash, mi_mod_hash, mi_flag_hash, mi_opt_hash
                      , mi_hpc_hash, mi_plugin_hash, mi_orphan, mi_finsts, mi_exp_hash
                      , mi_orphan_hash, mi_decl_warn_fn, mi_export_warn_fn, mi_fix_fn
                      , mi_hash_fn})
    =     rnf mi_iface_hash
    `seq` rnf mi_mod_hash
    `seq` rnf mi_flag_hash
    `seq` rnf mi_opt_hash
    `seq` rnf mi_hpc_hash
    `seq` rnf mi_plugin_hash
    `seq` rnf mi_orphan
    `seq` rnf mi_finsts
    `seq` rnf mi_exp_hash
    `seq` rnf mi_orphan_hash
    `seq` rnf mi_decl_warn_fn
    `seq` rnf mi_export_warn_fn
    `seq` rnf mi_fix_fn
    `seq` rnf mi_hash_fn


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
  -> [(Fingerprint, IfaceDecl)]
  -> Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo]
  -> ModIfaceBackend
  -> ModIface
completePartialModIface partial decls extra_decls final_exts = partial
  { mi_decls_ = decls
  , mi_extra_decls_ = extra_decls
  , mi_final_exts_ = final_exts
  , mi_hi_bytes_ = FullIfaceBinHandle Strict.Nothing
  }

-- | Add a source fingerprint to a 'ModIface_' without invalidating the byte array
-- buffer 'mi_hi_bytes'.
-- This is a variant of 'set_mi_src_hash' which does invalidate the buffer.
--
-- The 'mi_src_hash' is computed outside of 'ModIface_' based on the 'ModSummary'.
addSourceFingerprint :: Fingerprint -> ModIface_ phase -> ModIface_ phase
addSourceFingerprint val iface = iface { mi_src_hash_ = val }

-- | Copy fields that aren't serialised to disk to the new 'ModIface_'.
-- This includes especially hashes that are usually stored in the interface
-- file header and 'mi_top_env'.
--
-- We need this function after calling 'shareIface', to make sure the
-- 'ModIface_' doesn't lose any information. This function does not discard
-- the in-memory byte array buffer 'mi_hi_bytes'.
restoreFromOldModIface :: ModIface_ phase -> ModIface_ phase -> ModIface_ phase
restoreFromOldModIface old new = new
  { mi_top_env_ = mi_top_env_ old
  , mi_hsc_src_ = mi_hsc_src_ old
  , mi_src_hash_ = mi_src_hash_ old
  }

set_mi_module :: Module -> ModIface_ phase -> ModIface_ phase
set_mi_module val iface = clear_mi_hi_bytes $ iface { mi_module_ = val }

set_mi_sig_of :: Maybe Module -> ModIface_ phase -> ModIface_ phase
set_mi_sig_of val iface = clear_mi_hi_bytes $ iface { mi_sig_of_ = val }

set_mi_hsc_src :: HscSource -> ModIface_ phase -> ModIface_ phase
set_mi_hsc_src val iface = clear_mi_hi_bytes $ iface { mi_hsc_src_ = val }

set_mi_src_hash :: Fingerprint -> ModIface_ phase -> ModIface_ phase
set_mi_src_hash val iface = clear_mi_hi_bytes $ iface { mi_src_hash_ = val }

set_mi_hi_bytes :: IfaceBinHandle phase -> ModIface_ phase -> ModIface_ phase
set_mi_hi_bytes val iface = iface { mi_hi_bytes_ = val }

set_mi_deps :: Dependencies -> ModIface_ phase -> ModIface_ phase
set_mi_deps val iface = clear_mi_hi_bytes $ iface { mi_deps_ = val }

set_mi_usages :: [Usage] -> ModIface_ phase -> ModIface_ phase
set_mi_usages val iface = clear_mi_hi_bytes $ iface { mi_usages_ = val }

set_mi_exports :: [IfaceExport] -> ModIface_ phase -> ModIface_ phase
set_mi_exports val iface = clear_mi_hi_bytes $ iface { mi_exports_ = val }

set_mi_used_th :: Bool -> ModIface_ phase -> ModIface_ phase
set_mi_used_th val iface = clear_mi_hi_bytes $ iface { mi_used_th_ = val }

set_mi_fixities :: [(OccName, Fixity)] -> ModIface_ phase -> ModIface_ phase
set_mi_fixities val iface = clear_mi_hi_bytes $ iface { mi_fixities_ = val }

set_mi_warns :: IfaceWarnings -> ModIface_ phase -> ModIface_ phase
set_mi_warns val iface = clear_mi_hi_bytes $ iface { mi_warns_ = val }

set_mi_anns :: [IfaceAnnotation] -> ModIface_ phase -> ModIface_ phase
set_mi_anns val iface = clear_mi_hi_bytes $ iface { mi_anns_ = val }

set_mi_insts :: [IfaceClsInst] -> ModIface_ phase -> ModIface_ phase
set_mi_insts val iface = clear_mi_hi_bytes $ iface { mi_insts_ = val }

set_mi_fam_insts :: [IfaceFamInst] -> ModIface_ phase -> ModIface_ phase
set_mi_fam_insts val iface = clear_mi_hi_bytes $ iface { mi_fam_insts_ = val }

set_mi_rules :: [IfaceRule] -> ModIface_ phase -> ModIface_ phase
set_mi_rules val iface = clear_mi_hi_bytes $ iface { mi_rules_ = val }

set_mi_decls :: [IfaceDeclExts phase] -> ModIface_ phase -> ModIface_ phase
set_mi_decls val iface = clear_mi_hi_bytes $ iface { mi_decls_ = val }

set_mi_extra_decls :: Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -> ModIface_ phase -> ModIface_ phase
set_mi_extra_decls val iface = clear_mi_hi_bytes $ iface { mi_extra_decls_ = val }

set_mi_stub_objs :: [ByteString] -> ModIface_ phase -> ModIface_ phase
set_mi_stub_objs stub_objs iface = clear_mi_hi_bytes $ iface { mi_stub_objs_ = stub_objs }

set_mi_top_env :: Maybe IfaceTopEnv -> ModIface_ phase -> ModIface_ phase
set_mi_top_env val iface = clear_mi_hi_bytes $ iface { mi_top_env_ = val }

set_mi_hpc :: AnyHpcUsage -> ModIface_ phase -> ModIface_ phase
set_mi_hpc val iface = clear_mi_hi_bytes $ iface { mi_hpc_ = val }

set_mi_trust :: IfaceTrustInfo -> ModIface_ phase -> ModIface_ phase
set_mi_trust val iface = clear_mi_hi_bytes $ iface { mi_trust_ = val }

set_mi_trust_pkg :: Bool -> ModIface_ phase -> ModIface_ phase
set_mi_trust_pkg val iface = clear_mi_hi_bytes $ iface { mi_trust_pkg_ = val }

set_mi_complete_matches :: [IfaceCompleteMatch] -> ModIface_ phase -> ModIface_ phase
set_mi_complete_matches val iface = clear_mi_hi_bytes $ iface { mi_complete_matches_ = val }

set_mi_docs :: Maybe Docs -> ModIface_ phase -> ModIface_ phase
set_mi_docs val iface = clear_mi_hi_bytes $  iface { mi_docs_ = val }

set_mi_final_exts :: IfaceBackendExts phase -> ModIface_ phase -> ModIface_ phase
set_mi_final_exts val iface = clear_mi_hi_bytes $ iface { mi_final_exts_ = val }

set_mi_ext_fields :: ExtensibleFields -> ModIface_ phase -> ModIface_ phase
set_mi_ext_fields val iface = clear_mi_hi_bytes $ iface { mi_ext_fields_ = val }

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
{-# INLINE ModIface #-}
{-# INLINE mi_module #-}
{-# INLINE mi_sig_of #-}
{-# INLINE mi_hsc_src #-}
{-# INLINE mi_deps #-}
{-# INLINE mi_usages #-}
{-# INLINE mi_exports #-}
{-# INLINE mi_used_th #-}
{-# INLINE mi_fixities #-}
{-# INLINE mi_warns #-}
{-# INLINE mi_anns #-}
{-# INLINE mi_decls #-}
{-# INLINE mi_extra_decls #-}
{-# INLINE mi_stub_objs #-}
{-# INLINE mi_top_env #-}
{-# INLINE mi_insts #-}
{-# INLINE mi_fam_insts #-}
{-# INLINE mi_rules #-}
{-# INLINE mi_hpc #-}
{-# INLINE mi_trust #-}
{-# INLINE mi_trust_pkg #-}
{-# INLINE mi_complete_matches #-}
{-# INLINE mi_docs #-}
{-# INLINE mi_final_exts #-}
{-# INLINE mi_ext_fields #-}
{-# INLINE mi_src_hash #-}
{-# INLINE mi_hi_bytes #-}
{-# COMPLETE ModIface #-}

pattern ModIface ::
  Module -> Maybe Module -> HscSource -> Dependencies -> [Usage] ->
  [IfaceExport] -> Bool -> [(OccName, Fixity)] -> IfaceWarnings ->
  [IfaceAnnotation] -> [IfaceDeclExts phase] -> Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -> [ByteString] ->
  Maybe IfaceTopEnv -> [IfaceClsInst] -> [IfaceFamInst] -> [IfaceRule] ->
  AnyHpcUsage -> IfaceTrustInfo -> Bool -> [IfaceCompleteMatch] -> Maybe Docs ->
  IfaceBackendExts phase -> ExtensibleFields -> Fingerprint -> IfaceBinHandle phase ->
  ModIface_ phase
pattern ModIface
  { mi_module
  , mi_sig_of
  , mi_hsc_src
  , mi_deps
  , mi_usages
  , mi_exports
  , mi_used_th
  , mi_fixities
  , mi_warns
  , mi_anns
  , mi_decls
  , mi_extra_decls
  , mi_stub_objs
  , mi_top_env
  , mi_insts
  , mi_fam_insts
  , mi_rules
  , mi_hpc
  , mi_trust
  , mi_trust_pkg
  , mi_complete_matches
  , mi_docs
  , mi_final_exts
  , mi_ext_fields
  , mi_src_hash
  , mi_hi_bytes
  } <- PrivateModIface
    { mi_module_ = mi_module
    , mi_sig_of_ = mi_sig_of
    , mi_hsc_src_ = mi_hsc_src
    , mi_deps_ = mi_deps
    , mi_usages_ = mi_usages
    , mi_exports_ = mi_exports
    , mi_used_th_ = mi_used_th
    , mi_fixities_ = mi_fixities
    , mi_warns_ = mi_warns
    , mi_anns_ = mi_anns
    , mi_decls_ = mi_decls
    , mi_extra_decls_ = mi_extra_decls
    , mi_stub_objs_ = mi_stub_objs
    , mi_top_env_ = mi_top_env
    , mi_insts_ = mi_insts
    , mi_fam_insts_ = mi_fam_insts
    , mi_rules_ = mi_rules
    , mi_hpc_ = mi_hpc
    , mi_trust_ = mi_trust
    , mi_trust_pkg_ = mi_trust_pkg
    , mi_complete_matches_ = mi_complete_matches
    , mi_docs_ = mi_docs
    , mi_final_exts_ = mi_final_exts
    , mi_ext_fields_ = mi_ext_fields
    , mi_src_hash_ = mi_src_hash
    , mi_hi_bytes_ = mi_hi_bytes
    }
