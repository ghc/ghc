{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module GHC.Unit.Module.ModIface
   ( ModIface
   , ModIface_ (..)
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
import GHC.Unit.Module.WholeCoreBindings (IfaceForeign (..), emptyIfaceForeign)

import GHC.Types.Avail
import GHC.Types.Fixity
import GHC.Types.Fixity.Env
import GHC.Types.HpcInfo
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
import GHC.Types.Name.Reader (IfGlobalRdrEnv)

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
data ModIface_ (phase :: ModIfacePhase)
  = ModIface {
        mi_module     :: !Module,             -- ^ Name of the module we are for
        mi_sig_of     :: !(Maybe Module),     -- ^ Are we a sig of another mod?

        mi_hsc_src    :: !HscSource,          -- ^ Boot? Signature?

        mi_deps     :: Dependencies,
                -- ^ The dependencies of the module.  This is
                -- consulted for directly-imported modules, but not
                -- for anything else (hence lazy)

        mi_usages   :: [Usage],
                -- ^ Usages; kept sorted so that it's easy to decide
                -- whether to write a new iface file (changing usages
                -- doesn't affect the hash of this module)
                -- NOT STRICT!  we read this field lazily from the interface file
                -- It is *only* consulted by the recompilation checker

        mi_exports  :: ![IfaceExport],
                -- ^ Exports
                -- Kept sorted by (mod,occ), to make version comparisons easier
                -- Records the modules that are the declaration points for things
                -- exported by this module, and the 'OccName's of those things


        mi_used_th  :: !Bool,
                -- ^ Module required TH splices when it was compiled.
                -- This disables recompilation avoidance (see #481).

        mi_fixities :: [(OccName,Fixity)],
                -- ^ Fixities
                -- NOT STRICT!  we read this field lazily from the interface file

        mi_warns    :: IfaceWarnings,
                -- ^ Warnings
                -- NOT STRICT!  we read this field lazily from the interface file

        mi_anns     :: [IfaceAnnotation],
                -- ^ Annotations
                -- NOT STRICT!  we read this field lazily from the interface file


        mi_decls    :: [IfaceDeclExts phase],
                -- ^ Type, class and variable declarations
                -- The hash of an Id changes if its fixity or deprecations change
                --      (as well as its type of course)
                -- Ditto data constructors, class operations, except that
                -- the hash of the parent class/tycon changes

        mi_extra_decls :: Maybe [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo],
                -- ^ Extra variable definitions which are **NOT** exposed but when
                -- combined with mi_decls allows us to restart code generation.
                -- See Note [Interface Files with Core Definitions] and Note [Interface File with Core: Sharing RHSs]

        mi_top_env  :: !(Maybe IfaceTopEnv),
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

        mi_foreign :: !IfaceForeign,
                -- ^ Foreign stubs and files to supplement 'mi_extra_decls_'.
                -- See Note [Foreign stubs and TH bytecode linking]

                -- Instance declarations and rules
        mi_insts       :: [IfaceClsInst],     -- ^ Sorted class instance
        mi_fam_insts   :: [IfaceFamInst],  -- ^ Sorted family instances
        mi_rules       :: [IfaceRule],     -- ^ Sorted rules

        mi_hpc       :: !AnyHpcUsage,
                -- ^ True if this program uses Hpc at any point in the program.

        mi_trust     :: !IfaceTrustInfo,
                -- ^ Safe Haskell Trust information for this module.

        mi_trust_pkg :: !Bool,
                -- ^ Do we require the package this module resides in be trusted
                -- to trust this module? This is used for the situation where a
                -- module is Safe (so doesn't require the package be trusted
                -- itself) but imports some trustworthy modules from its own
                -- package (which does require its own package be trusted).
                -- See Note [Trust Own Package] in GHC.Rename.Names
        mi_complete_matches :: ![IfaceCompleteMatch],

        mi_docs :: !(Maybe Docs),
                -- ^ Docstrings and related data for use by haddock, the ghci
                -- @:doc@ command, and other tools.
                --
                -- @Just _@ @<=>@ the module was built with @-haddock@.

        mi_final_exts :: !(IfaceBackendExts phase),
                -- ^ Either `()` or `ModIfaceBackend` for
                -- a fully instantiated interface.

        mi_ext_fields :: !ExtensibleFields,
                -- ^ Additional optional fields, where the Map key represents
                -- the field name, resulting in a (size, serialized data) pair.
                -- Because the data is intended to be serialized through the
                -- internal `Binary` class (increasing compatibility with types
                -- using `Name` and `FastString`, such as HIE), this format is
                -- chosen over `ByteString`s.
                --

        mi_src_hash :: !Fingerprint
                -- ^ Hash of the .hs source, used for recompilation checking.
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
   put_ bh (ModIface {
                 mi_module    = mod,
                 mi_sig_of    = sig_of,
                 mi_hsc_src   = hsc_src,
                 mi_src_hash = _src_hash, -- Don't `put_` this in the instance
                                          -- because we are going to write it
                                          -- out separately in the actual file
                 mi_deps      = deps,
                 mi_usages    = usages,
                 mi_exports   = exports,
                 mi_used_th   = used_th,
                 mi_fixities  = fixities,
                 mi_warns     = warns,
                 mi_anns      = anns,
                 mi_decls     = decls,
                 mi_extra_decls = extra_decls,
                 mi_foreign   = foreign_,
                 mi_insts     = insts,
                 mi_fam_insts = fam_insts,
                 mi_rules     = rules,
                 mi_hpc       = hpc_info,
                 mi_trust     = trust,
                 mi_trust_pkg = trust_pkg,
                 mi_complete_matches = complete_matches,
                 mi_docs      = docs,
                 mi_ext_fields = _ext_fields, -- Don't `put_` this in the instance so we
                                              -- can deal with it's pointer in the header
                                              -- when we write the actual file
                 mi_final_exts = ModIfaceBackend {
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
        put_ bh foreign_
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
        foreign_    <- get bh
        insts       <- {-# SCC "bin_insts" #-} get bh
        fam_insts   <- {-# SCC "bin_fam_insts" #-} get bh
        rules       <- {-# SCC "bin_rules" #-} lazyGet bh
        orphan_hash <- get bh
        hpc_info    <- get bh
        trust       <- get bh
        trust_pkg   <- get bh
        complete_matches <- get bh
        docs        <- lazyGetMaybe bh
        return (ModIface {
                 mi_module      = mod,
                 mi_sig_of      = sig_of,
                 mi_hsc_src     = hsc_src,
                 mi_src_hash = fingerprint0, -- placeholder because this is dealt
                                             -- with specially when the file is read
                 mi_deps        = deps,
                 mi_usages      = usages,
                 mi_exports     = exports,
                 mi_used_th     = used_th,
                 mi_anns        = anns,
                 mi_fixities    = fixities,
                 mi_warns       = warns,
                 mi_decls       = decls,
                 mi_extra_decls = extra_decls,
                 mi_foreign     = foreign_,
                 mi_top_env     = Nothing,
                 mi_insts       = insts,
                 mi_fam_insts   = fam_insts,
                 mi_rules       = rules,
                 mi_hpc         = hpc_info,
                 mi_trust       = trust,
                 mi_trust_pkg   = trust_pkg,
                        -- And build the cached values
                 mi_complete_matches = complete_matches,
                 mi_docs        = docs,
                 mi_ext_fields  = emptyExtensibleFields, -- placeholder because this is dealt
                                                         -- with specially when the file is read
                 mi_final_exts = ModIfaceBackend {
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
  = ModIface { mi_module      = mod,
               mi_sig_of      = Nothing,
               mi_hsc_src     = HsSrcFile,
               mi_src_hash    = fingerprint0,
               mi_deps        = noDependencies,
               mi_usages      = [],
               mi_exports     = [],
               mi_used_th     = False,
               mi_fixities    = [],
               mi_warns       = IfWarnSome [] [],
               mi_anns        = [],
               mi_insts       = [],
               mi_fam_insts   = [],
               mi_rules       = [],
               mi_decls       = [],
               mi_extra_decls = Nothing,
               mi_foreign     = emptyIfaceForeign,
               mi_top_env     = Nothing,
               mi_hpc         = False,
               mi_trust       = noIfaceTrustInfo,
               mi_trust_pkg   = False,
               mi_complete_matches = [],
               mi_docs        = Nothing,
               mi_final_exts  = (),
               mi_ext_fields  = emptyExtensibleFields
             }

emptyFullModIface :: Module -> ModIface
emptyFullModIface mod =
    (emptyPartialModIface mod)
      { mi_decls = []
      , mi_final_exts = ModIfaceBackend
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
  rnf (ModIface{ mi_module, mi_sig_of, mi_hsc_src, mi_deps, mi_usages
               , mi_exports, mi_used_th, mi_fixities, mi_warns, mi_anns
               , mi_decls, mi_extra_decls, mi_foreign, mi_top_env, mi_insts
               , mi_fam_insts, mi_rules, mi_hpc, mi_trust, mi_trust_pkg
               , mi_complete_matches, mi_docs, mi_final_exts
               , mi_ext_fields, mi_src_hash })
    =     rnf mi_module
    `seq` rnf mi_sig_of
    `seq`     mi_hsc_src
    `seq`     mi_deps
    `seq`     mi_usages
    `seq`     mi_exports
    `seq` rnf mi_used_th
    `seq`     mi_fixities
    `seq` rnf mi_warns
    `seq` rnf mi_anns
    `seq` rnf mi_decls
    `seq` rnf mi_extra_decls
    `seq` rnf mi_foreign
    `seq` rnf mi_top_env
    `seq` rnf mi_insts
    `seq` rnf mi_fam_insts
    `seq` rnf mi_rules
    `seq` rnf mi_hpc
    `seq`     mi_trust
    `seq` rnf mi_trust_pkg
    `seq` rnf mi_complete_matches
    `seq` rnf mi_docs
    `seq`     mi_final_exts
    `seq`     mi_ext_fields
    `seq` rnf mi_src_hash
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



