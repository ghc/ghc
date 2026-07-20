module GHC.Unit.External.Providers (
  ModuleNameProvidersMap,
  pprModuleMap,
  mkModuleNameProvidersMap,
  mkUnusableModuleNameProvidersMap,
) where

import GHC.Prelude

import GHC.Data.Maybe
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Unit.External.ModuleOrigin
import GHC.Unit.External.Query
import GHC.Unit.External.Validate
import GHC.Unit.External.Visibility
import GHC.Unit.Info
import GHC.Unit.Module
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Map from 'ModuleName' to a set of module providers (i.e. a 'Module' and
-- its 'ModuleOrigin').
--
-- NB: the set is in fact a 'Map Module ModuleOrigin', probably to keep only one
-- origin for a given 'Module'

type ModuleNameProvidersMap =
    UniqMap ModuleName (UniqMap Module ModuleOrigin)

-- | Show the mapping of modules to where they come from.
pprModuleMap :: ModuleNameProvidersMap -> SDoc
pprModuleMap mod_map =
  vcat (map pprLine (nonDetUniqMapToList mod_map))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (nonDetUniqMapToList e)))
      pprEntry :: Outputable a => ModuleName -> (Module, a) -> SDoc
      pprEntry m (m',o)
        | m == moduleName m' = ppr (moduleUnit m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

-- -----------------------------------------------------------------------------
-- | Makes the mapping from ModuleName to package info

-- Slight irritation: we proceed by leafing through everything
-- in the installed package database, which makes handling indefinite
-- packages a bit bothersome.

mkModuleNameProvidersMap
  :: Logger
  -> Bool
  -> UnitInfoMap
  -> VisibilityMap
  -> ModuleNameProvidersMap
mkModuleNameProvidersMap logger allowVirtualUnits pkg_map vis_map =
    -- What should we fold on?  Both situations are awkward:
    --
    --    * Folding on the visibility map means that we won't create
    --      entries for packages that aren't mentioned in vis_map
    --      (e.g., hidden packages, causing #14717)
    --
    --    * Folding on pkg_map is awkward because if we have an
    --      Backpack instantiation, we need to possibly add a
    --      package from pkg_map multiple times to the actual
    --      ModuleNameProvidersMap.  Also, we don't really want
    --      definite package instantiations to show up in the
    --      list of possibilities.
    --
    -- So what will we do instead?  We'll extend vis_map with
    -- entries for every definite (for non-Backpack) and
    -- indefinite (for Backpack) package, so that we get the
    -- hidden entries we need.
    nonDetFoldUniqMap extend_modmap emptyMap vis_map_extended
 where
  vis_map_extended = {- preferred -} default_vis `plusUniqMap` vis_map

  default_vis = listToUniqMap
                  [ (mkUnit pkg, mempty)
                  | (_, pkg) <- nonDetUniqMapToList pkg_map
                  -- Exclude specific instantiations of an indefinite
                  -- package
                  , unitIsIndefinite pkg || null (unitInstantiations pkg)
                  ]

  emptyMap = emptyUniqMap
  setOrigins m os = fmap (const os) m
  extend_modmap (uid, UnitVisibility { uv_expose_all = b, uv_renamings = rns }) modmap
    = addListTo modmap theBindings
   where
    pkg = unit_lookup uid

    theBindings :: [(ModuleName, UniqMap Module ModuleOrigin)]
    theBindings = newBindings b rns

    newBindings :: Bool
                -> [(ModuleName, ModuleName)]
                -> [(ModuleName, UniqMap Module ModuleOrigin)]
    newBindings e rns  = es e ++ hiddens ++ map rnBinding rns

    rnBinding :: (ModuleName, ModuleName)
              -> (ModuleName, UniqMap Module ModuleOrigin)
    rnBinding (orig, new) = (new, setOrigins origEntry fromFlag)
     where origEntry = case lookupUFM esmap orig of
            Just r -> r
            Nothing -> throwGhcException (CmdLineError (renderWithContext
                        (log_default_user_context (logFlags logger))
                        (text "package flag: could not find module name" <+>
                            ppr orig <+> text "in package" <+> ppr pk)))

    es :: Bool -> [(ModuleName, UniqMap Module ModuleOrigin)]
    es e = do
     (m, exposedReexport) <- exposed_mods
     let (pk', m', origin') =
          case exposedReexport of
           Nothing -> (pk, m, fromExposedModules e)
           Just (Module pk' m') ->
              (pk', m', fromReexportedModules e pkg)
     return (m, mkModMap pk' m' origin')

    esmap :: UniqFM ModuleName (UniqMap Module ModuleOrigin)
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, mkModMap pk m ModHidden) | m <- hidden_mods]

    pk = mkUnit pkg
    unit_lookup uid = lookupUnit' allowVirtualUnits pkg_map uid
                        `orElse` pprPanic "unit_lookup" (ppr uid)

    exposed_mods = unitExposedModules pkg
    hidden_mods  = unitHiddenModules pkg

-- | Make a 'ModuleNameProvidersMap' covering a set of unusable packages.
mkUnusableModuleNameProvidersMap :: UnusableUnits -> ModuleNameProvidersMap
mkUnusableModuleNameProvidersMap unusables =
    nonDetFoldUniqMap extend_modmap emptyUniqMap unusables
 where
    extend_modmap (_uid, (unit_info, reason)) modmap = addListTo modmap bindings
      where bindings :: [(ModuleName, UniqMap Module ModuleOrigin)]
            bindings = exposed ++ hidden

            origin_reexport =  ModUnusable (UnusableUnit unit reason True)
            origin_normal   =  ModUnusable (UnusableUnit unit reason False)
            unit = mkUnit unit_info

            exposed = map get_exposed exposed_mods
            hidden = [(m, mkModMap unit m origin_normal) | m <- hidden_mods]

            -- with re-exports, c:Foo can be reexported from two (or more)
            -- unusable packages:
            --  Foo -> a:Foo (unusable reason A) -> c:Foo
            --      -> b:Foo (unusable reason B) -> c:Foo
            --
            -- We must be careful to not record the following (#21097):
            --  Foo -> c:Foo (unusable reason A)
            --      -> c:Foo (unusable reason B)
            -- But:
            --  Foo -> a:Foo (unusable reason A)
            --      -> b:Foo (unusable reason B)
            --
            get_exposed (mod, Just _) = (mod, mkModMap unit mod origin_reexport)
            get_exposed (mod, _) = (mod, mkModMap unit mod origin_normal)
              -- in the reexport case, we create a virtual module that doesn't
              -- exist but we don't care as it's only used as a key in the map.

            exposed_mods = unitExposedModules unit_info
            hidden_mods  = unitHiddenModules  unit_info

-- | Add a list of key/value pairs to a nested map.
--
-- The outer map is processed with 'Data.Map.Strict' to prevent memory leaks
-- when reloading modules in GHCi (see #4029). This ensures that each
-- value is forced before installing into the map.
addListTo :: (Monoid a, Ord k1, Ord k2, Uniquable k1, Uniquable k2)
          => UniqMap k1 (UniqMap k2 a)
          -> [(k1, UniqMap k2 a)]
          -> UniqMap k1 (UniqMap k2 a)
addListTo = foldl' merge
  where merge m (k, v) = addToUniqMap_C (plusUniqMap_C mappend) m k v

-- | Create a singleton module mapping
mkModMap :: Unit -> ModuleName -> ModuleOrigin -> UniqMap Module ModuleOrigin
mkModMap pkg mod = unitUniqMap (mkModule pkg mod)
