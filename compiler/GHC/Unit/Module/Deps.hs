-- | Dependencies and Usage of a module
module GHC.Unit.Module.Deps
   ( Dependencies
   , mkDependencies
   , noDependencies
   , dep_direct_mods
   , dep_direct_pkgs
   , dep_sig_mods
   , dep_trusted_pkgs
   , dep_orphs
   , dep_plugin_pkgs
   , dep_finsts
   , dep_boot_mods
   , dep_orphs_update
   , dep_finsts_update
   , pprDeps
   , Usage (..)
   , ImportAvails (..)
   )
where

import GHC.Prelude

import GHC.Types.SafeHaskell
import GHC.Types.Name

import GHC.Unit.Module.Name
import GHC.Unit.Module.Imported
import GHC.Unit.Module
import GHC.Unit.Home
import GHC.Unit.State

import GHC.Utils.Fingerprint
import GHC.Utils.Binary
import GHC.Utils.Outputable

import Data.List (sortBy, sort, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor

-- | Dependency information about ALL modules and packages below this one
-- in the import hierarchy. This is the serialisable version of `ImportAvails`.
--
-- Invariant: the dependencies of a module @M@ never includes @M@.
--
-- Invariant: none of the lists contain duplicates.
--
-- Invariant: lists are ordered canonically (e.g. using stableModuleCmp)
--
-- See Note [Transitive Information in Dependencies]
data Dependencies = Deps
   { dep_direct_mods :: Set (UnitId, ModuleNameWithIsBoot)
      -- ^ All home-package modules which are directly imported by this one.
      -- This may include modules from other units when using multiple home units

   , dep_direct_pkgs :: Set UnitId
      -- ^ All packages directly imported by this module
      -- I.e. packages to which this module's direct imports belong.
      -- Does not include other home units when using multiple home units.
      -- Modules from these units will go in `dep_direct_mods`

   , dep_plugin_pkgs :: Set UnitId
      -- ^ All units needed for plugins

    ------------------------------------
    -- Transitive information below here

   , dep_sig_mods :: ![ModuleName]
    -- ^ Transitive closure of hsig files in the home package


   , dep_trusted_pkgs :: Set UnitId
      -- Packages which we are required to trust
      -- when the module is imported as a safe import
      -- (Safe Haskell). See Note [Tracking Trust Transitively] in GHC.Rename.Names

   , dep_boot_mods :: Set (UnitId, ModuleNameWithIsBoot)
      -- ^ All modules which have boot files below this one, and whether we
      -- should use the boot file or not.
      -- This information is only used to populate the eps_is_boot field.
      -- See Note [Structure of dep_boot_mods]

   , dep_orphs  :: [Module]
      -- ^ Transitive closure of orphan modules (whether
      -- home or external pkg).
      --
      -- (Possible optimization: don't include family
      -- instance orphans as they are anyway included in
      -- 'dep_finsts'.  But then be careful about code
      -- which relies on dep_orphs having the complete list!)
      -- This does NOT include us, unlike 'imp_orphs'.

   , dep_finsts :: [Module]
      -- ^ Transitive closure of depended upon modules which
      -- contain family instances (whether home or external).
      -- This is used by 'checkFamInstConsistency'.  This
      -- does NOT include us, unlike 'imp_finsts'. See Note
      -- [The type family instance consistency story].

   }
   deriving( Eq )
        -- Equality used only for old/new comparison in GHC.Iface.Recomp.addFingerprints
        -- See 'GHC.Tc.Utils.ImportAvails' for details on dependencies.


-- | Extract information from the rename and typecheck phases to produce
-- a dependencies information for the module being compiled.
--
-- The fourth argument is a list of plugin modules.
mkDependencies :: HomeUnit -> Module -> ImportAvails -> [Module] -> Dependencies
mkDependencies home_unit mod imports plugin_mods =
  let (home_plugins, external_plugins) = partition (isHomeUnit home_unit . moduleUnit) plugin_mods
      plugin_units = Set.fromList (map (toUnitId . moduleUnit) external_plugins)
      all_direct_mods = foldr (\mn m -> extendInstalledModuleEnv m mn (GWIB (moduleName mn) NotBoot))
                              (imp_direct_dep_mods imports)
                              (map (fmap toUnitId) home_plugins)

      modDepsElts = Set.fromList . installedModuleEnvElts
        -- It's OK to use nonDetEltsUFM here because sorting by module names
        -- restores determinism

      direct_mods = first moduleUnit `Set.map` modDepsElts (delInstalledModuleEnv all_direct_mods (toUnitId <$> mod))
            -- M.hi-boot can be in the imp_dep_mods, but we must remove
            -- it before recording the modules on which this one depends!
            -- (We want to retain M.hi-boot in imp_dep_mods so that
            --  loadHiBootInterface can see if M's direct imports depend
            --  on M.hi-boot, and hence that we should do the hi-boot consistency
            --  check.)

      dep_orphs = filter (/= mod) (imp_orphs imports)
            -- We must also remove self-references from imp_orphs. See
            -- Note [Module self-dependency]

      direct_pkgs = imp_dep_direct_pkgs imports

      -- Set the packages required to be Safe according to Safe Haskell.
      -- See Note [Tracking Trust Transitively] in GHC.Rename.Names
      trust_pkgs  = imp_trust_pkgs imports

      -- If there's a non-boot import, then it shadows the boot import
      -- coming from the dependencies
      source_mods = first moduleUnit `Set.map` modDepsElts (imp_boot_mods imports)

      sig_mods = filter (/= (moduleName mod)) $ imp_sig_mods imports

  in Deps { dep_direct_mods  = direct_mods
          , dep_direct_pkgs  = direct_pkgs
          , dep_plugin_pkgs  = plugin_units
          , dep_sig_mods     = sort sig_mods
          , dep_trusted_pkgs = trust_pkgs
          , dep_boot_mods    = source_mods
          , dep_orphs        = sortBy stableModuleCmp dep_orphs
          , dep_finsts       = sortBy stableModuleCmp (imp_finsts imports)
            -- sort to get into canonical order
            -- NB. remember to use lexicographic ordering
          }

-- | Update module dependencies containing orphans (used by Backpack)
dep_orphs_update :: Monad m => Dependencies -> ([Module] -> m [Module]) -> m Dependencies
dep_orphs_update deps f = do
  r <- f (dep_orphs deps)
  pure (deps { dep_orphs = sortBy stableModuleCmp r })

-- | Update module dependencies containing family instances (used by Backpack)
dep_finsts_update :: Monad m => Dependencies -> ([Module] -> m [Module]) -> m Dependencies
dep_finsts_update deps f = do
  r <- f (dep_finsts deps)
  pure (deps { dep_finsts = sortBy stableModuleCmp r })


instance Binary Dependencies where
    put_ bh deps = do put_ bh (dep_direct_mods deps)
                      put_ bh (dep_direct_pkgs deps)
                      put_ bh (dep_plugin_pkgs deps)
                      put_ bh (dep_trusted_pkgs deps)
                      put_ bh (dep_sig_mods deps)
                      put_ bh (dep_boot_mods deps)
                      put_ bh (dep_orphs deps)
                      put_ bh (dep_finsts deps)

    get bh = do dms <- get bh
                dps <- get bh
                plugin_pkgs <- get bh
                tps <- get bh
                hsigms <- get bh
                sms <- get bh
                os <- get bh
                fis <- get bh
                return (Deps { dep_direct_mods = dms
                             , dep_direct_pkgs = dps
                             , dep_plugin_pkgs = plugin_pkgs
                             , dep_sig_mods = hsigms
                             , dep_boot_mods = sms
                             , dep_trusted_pkgs = tps
                             , dep_orphs = os,
                               dep_finsts = fis })

noDependencies :: Dependencies
noDependencies = Deps
  { dep_direct_mods  = Set.empty
  , dep_direct_pkgs  = Set.empty
  , dep_plugin_pkgs  = Set.empty
  , dep_sig_mods     = []
  , dep_boot_mods    = Set.empty
  , dep_trusted_pkgs = Set.empty
  , dep_orphs        = []
  , dep_finsts       = []
  }

-- | Pretty-print unit dependencies
pprDeps :: UnitState -> Dependencies -> SDoc
pprDeps unit_state (Deps { dep_direct_mods = dmods
                         , dep_boot_mods = bmods
                         , dep_plugin_pkgs = plgns
                         , dep_orphs = orphs
                         , dep_direct_pkgs = pkgs
                         , dep_trusted_pkgs = tps
                         , dep_finsts = finsts
                         })
  = pprWithUnitState unit_state $
    vcat [text "direct module dependencies:"  <+> ppr_set ppr_mod dmods,
          text "boot module dependencies:"    <+> ppr_set ppr bmods,
          text "direct package dependencies:" <+> ppr_set ppr pkgs,
          text "plugin package dependencies:" <+> ppr_set ppr plgns,
          if null tps
            then empty
            else text "trusted package dependencies:" <+> ppr_set ppr tps,
          text "orphans:" <+> fsep (map ppr orphs),
          text "family instance modules:" <+> fsep (map ppr finsts)
        ]
  where
    ppr_mod (uid, (GWIB mod IsBoot))  = ppr uid <> colon <> ppr mod <+> text "[boot]"
    ppr_mod (uid, (GWIB mod NotBoot)) = ppr uid <> colon <> ppr mod

    ppr_set :: Outputable a => (a -> SDoc) -> Set a -> SDoc
    ppr_set w = fsep . fmap w . Set.toAscList

-- | Records modules for which changes may force recompilation of this module
-- See wiki: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance
--
-- This differs from Dependencies.  A module X may be in the dep_mods of this
-- module (via an import chain) but if we don't use anything from X it won't
-- appear in our Usage
data Usage
  -- | Module from another package
  = UsagePackageModule {
        usg_mod      :: Module,
           -- ^ External package module depended on
        usg_mod_hash :: Fingerprint,
            -- ^ Cached module ABI fingerprint (corresponds to mi_mod_hash)
        usg_safe :: IsSafeImport
            -- ^ Was this module imported as a safe import
    }
  -- | Module from the current package
  | UsageHomeModule {
        usg_mod_name :: ModuleName,
            -- ^ Name of the module
        usg_mod_hash :: Fingerprint,
            -- ^ Cached module ABI fingerprint (corresponds to mi_mod_hash).
            -- This may be out dated after recompilation was avoided, but is
            -- still used as a fast initial check for change during
            -- recompilation avoidance.
        usg_entities :: [(OccName,Fingerprint)],
            -- ^ Entities we depend on, sorted by occurrence name and fingerprinted.
            -- NB: usages are for parent names only, e.g. type constructors
            -- but not the associated data constructors.
        usg_exports  :: Maybe Fingerprint,
            -- ^ Fingerprint for the export list of this module,
            -- if we directly imported it (and hence we depend on its export list)
        usg_safe :: IsSafeImport
            -- ^ Was this module imported as a safe import
    }
  -- | A file upon which the module depends, e.g. a CPP #include, or using TH's
  -- 'addDependentFile'
  | UsageFile {
        usg_file_path  :: FilePath,
        -- ^ External file dependency. From a CPP #include or TH
        -- addDependentFile. Should be absolute.
        usg_file_hash  :: Fingerprint,
        -- ^ 'Fingerprint' of the file contents.

        usg_file_label :: Maybe String
        -- ^ An optional string which is used in recompilation messages if
        -- file in question has changed.

        -- Note: We don't consider things like modification timestamps
        -- here, because there's no reason to recompile if the actual
        -- contents don't change.  This previously lead to odd
        -- recompilation behaviors; see #8114
  }
  | UsageHomeModuleInterface {
        usg_mod_name :: ModuleName
        -- ^ Name of the module
        , usg_iface_hash :: Fingerprint
        -- ^ The *interface* hash of the module, not the ABI hash.
        -- This changes when anything about the interface (and hence the
        -- module) has changed.

        -- UsageHomeModuleInterface is *only* used for recompilation
        -- checking when using TemplateHaskell in the interpreter (where
        -- some modules are loaded as BCOs).

  }
  -- | A requirement which was merged into this one.
  | UsageMergedRequirement {
        usg_mod :: Module,
        usg_mod_hash :: Fingerprint
  }
    deriving( Eq )
        -- The export list field is (Just v) if we depend on the export list:
        --      i.e. we imported the module directly, whether or not we
        --           enumerated the things we imported, or just imported
        --           everything
        -- We need to recompile if M's exports change, because
        -- if the import was    import M,       we might now have a name clash
        --                                      in the importing module.
        -- if the import was    import M(x)     M might no longer export x
        -- The only way we don't depend on the export list is if we have
        --                      import M()
        -- And of course, for modules that aren't imported directly we don't
        -- depend on their export lists

instance Binary Usage where
    put_ bh usg@UsagePackageModule{} = do
        putByte bh 0
        put_ bh (usg_mod usg)
        put_ bh (usg_mod_hash usg)
        put_ bh (usg_safe     usg)

    put_ bh usg@UsageHomeModule{} = do
        putByte bh 1
        put_ bh (usg_mod_name usg)
        put_ bh (usg_mod_hash usg)
        put_ bh (usg_exports  usg)
        put_ bh (usg_entities usg)
        put_ bh (usg_safe     usg)

    put_ bh usg@UsageFile{} = do
        putByte bh 2
        put_ bh (usg_file_path usg)
        put_ bh (usg_file_hash usg)
        put_ bh (usg_file_label usg)

    put_ bh usg@UsageMergedRequirement{} = do
        putByte bh 3
        put_ bh (usg_mod      usg)
        put_ bh (usg_mod_hash usg)

    put_ bh usg@UsageHomeModuleInterface{} = do
        putByte bh 4
        put_ bh (usg_mod_name usg)
        put_ bh (usg_iface_hash usg)

    get bh = do
        h <- getByte bh
        case h of
          0 -> do
            nm    <- get bh
            mod   <- get bh
            safe  <- get bh
            return UsagePackageModule { usg_mod = nm, usg_mod_hash = mod, usg_safe = safe }
          1 -> do
            nm    <- get bh
            mod   <- get bh
            exps  <- get bh
            ents  <- get bh
            safe  <- get bh
            return UsageHomeModule { usg_mod_name = nm, usg_mod_hash = mod,
                     usg_exports = exps, usg_entities = ents, usg_safe = safe }
          2 -> do
            fp   <- get bh
            hash <- get bh
            label <- get bh
            return UsageFile { usg_file_path = fp, usg_file_hash = hash, usg_file_label = label }
          3 -> do
            mod <- get bh
            hash <- get bh
            return UsageMergedRequirement { usg_mod = mod, usg_mod_hash = hash }
          4 -> do
            mod <- get bh
            hash <- get bh
            return UsageHomeModuleInterface { usg_mod_name = mod, usg_iface_hash = hash }
          i -> error ("Binary.get(Usage): " ++ show i)


{-
Note [Transitive Information in Dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is important to be careful what information we put in 'Dependencies' because
ultimately it ends up serialised in an interface file. Interface files must always
be kept up-to-date with the state of the world, so if `Dependencies` needs to be updated
then the module had to be recompiled just to update `Dependencies`.

Before #16885, the dependencies used to contain the transitive closure of all
home modules. Therefore, if you added an import somewhere low down in the home package
it would recompile nearly every module in your project, just to update this information.

Now, we are a bit more careful about what we store and
explicitly store transitive information only if it is really needed.

~ Direct Information

* dep_direct_mods - Directly imported home package modules
* dep_direct_pkgs - Directly imported packages
* dep_plgins      - Directly used plugins

~ Transitive Information

Some features of the compiler require transitive information about what is currently
being compiled, so that is explicitly stored separately in the form they need.

* dep_trusted_pkgs - Only used for the -fpackage-trust feature
* dep_boot_mods  - Only used to populate eps_is_boot in -c mode
* dep_orphs        - Modules with orphan instances
* dep_finsts       - Modules with type family instances

Important note: If you add some transitive information to the interface file then
you need to make sure recompilation is triggered when it could be out of date.
The correct way to do this is to include the transitive information in the export
hash of the module. The export hash is computed in `GHC.Iface.Recomp.addFingerprints`.
-}

{-
Note [Structure of dep_boot_deps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In `-c` mode we always need to know whether to load the normal or boot version of
an interface file, and this can't be determined from just looking at the direct imports.

Consider modules with dependencies:

```
A -(S)-> B
A -> C -> B -(S)-> B
```

Say when compiling module `A` that we need to load the interface for `B`, do we load
`B.hi` or `B.hi-boot`? Well, `A` does directly {-# SOURCE #-} import B, so you might think
that we would load the `B.hi-boot` file, however this is wrong because `C` imports
`B` normally. Therefore in the interface file for `C` we still need to record that
there is a hs-boot file for `B` below it but that we now want `B.hi` rather than
`B.hi-boot`. When `C` is imported, the fact that it needs `B.hi` clobbers the `{- SOURCE -}`
import for `B`.

Therefore in mod_boot_deps we store the names of any modules which have hs-boot files,
and whether we want to import the .hi or .hi-boot version of the interface file.

If you get this wrong, then GHC fails to compile, so there is a test but you might
not make it that far if you get this wrong!

Question: does this happen even across packages?
No: if I need to load the interface for module X from package P I always look for p:X.hi.

-}

-- | 'ImportAvails' summarises what was imported from where, irrespective of
-- whether the imported things are actually used or not.  It is used:
--
--  * when processing the export list,
--
--  * when constructing usage info for the interface file,
--
--  * to identify the list of directly imported modules for initialisation
--    purposes and for optimised overlap checking of family instances,
--
--  * when figuring out what things are really unused
--
data ImportAvails
   = ImportAvails {
        imp_mods :: ImportedMods,
          --      = ModuleEnv [ImportedModsVal],
          -- ^ Domain is all directly-imported modules
          --
          -- See the documentation on ImportedModsVal in
          -- "GHC.Unit.Module.Imported" for the meaning of the fields.
          --
          -- We need a full ModuleEnv rather than a ModuleNameEnv here,
          -- because we might be importing modules of the same name from
          -- different packages. (currently not the case, but might be in the
          -- future).

        imp_direct_dep_mods :: InstalledModuleEnv ModuleNameWithIsBoot,
          -- ^ Home-package modules directly imported by the module being compiled.

        imp_dep_direct_pkgs :: Set UnitId,
          -- ^ Packages directly needed by the module being compiled

        imp_trust_own_pkg :: Bool,
          -- ^ Do we require that our own package is trusted?
          -- This is to handle efficiently the case where a Safe module imports
          -- a Trustworthy module that resides in the same package as it.
          -- See Note [Trust Own Package] in "GHC.Rename.Names"

        -- Transitive information below here

        imp_trust_pkgs :: Set UnitId,
          -- ^ This records the
          -- packages the current module needs to trust for Safe Haskell
          -- compilation to succeed. A package is required to be trusted if
          -- we are dependent on a trustworthy module in that package.
          -- See Note [Tracking Trust Transitively] in "GHC.Rename.Names"

        imp_boot_mods :: InstalledModuleEnv ModuleNameWithIsBoot,
          -- ^ Domain is all modules which have hs-boot files, and whether
          -- we should import the boot version of interface file. Only used
          -- in one-shot mode to populate eps_is_boot.

        imp_sig_mods :: [ModuleName],
          -- ^ Signature modules below this one

        imp_orphs :: [Module],
          -- ^ Orphan modules below us in the import tree (and maybe including
          -- us for imported modules)

        imp_finsts :: [Module]
          -- ^ Family instance modules below us in the import tree (and maybe
          -- including us for imported modules)
      }
