{-# LANGUAGE LambdaCase #-}
-- | This module abstracts over the two main states of the compiler: the external package state and the home unit graph.
-- If you don't know whether you are going to find something in an external package or a home unit, you should use the abstractions
-- in this module to find the information you need.
module GHC.Unit.CombinedState where

import GHC.Prelude
import GHC.Unit.External
import GHC.Unit.Home.Graph

import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Unit.Module

import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.ModInfo
import GHC.Types.Name.Env
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps
import GHC.Unit.Module.ModIface
import GHC.Utils.Fingerprint

import GHC.Types.SafeHaskell
import Data.Maybe (isJust)
import Control.Arrow ((&&&))
import GHC.Types.Unique.DSet


data CombinedState = CombinedState {
    combined_state_eps :: ExternalPackageState,
    combined_state_hug :: HomeUnitGraph
}

lookupSimpleIface :: CombinedState -> Module -> IO (Maybe SimpleModIface)
lookupSimpleIface (CombinedState eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just hm -> pure $! Just $! mkSimpleModiface (hm_iface hm)
    Nothing -> pure $! lookupEPSimpleInfo eps mod

isModuleLoaded :: CombinedState -> Module -> IO Bool
isModuleLoaded (CombinedState eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just {} -> pure $! True
    Nothing -> pure $! (isJust $! lookupModuleEnv (eps_simple_info eps) mod)

lookupType :: CombinedState -> Module -> Name -> IO (Maybe TyThing)
lookupType (CombinedState eps hug) mod name = ty
  where
    ty = HUG.lookupHugByModule mod hug >>= \case
            Just hm -> pure $! lookupNameEnv (md_types (hm_details hm)) name
            Nothing -> pure $! lookupNameEnv (eps_PTE eps) name

lookupDependencies :: CombinedState -> Module -> IO (Maybe Dependencies)
lookupDependencies (CombinedState eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just hm -> pure $! Just $! mi_deps (hm_iface hm)
    Nothing -> pure $! mi_simple_info_deps <$> lookupEPSimpleInfo eps mod 

lookupIfaceHash :: CombinedState -> Module -> IO (Maybe Fingerprint)
lookupIfaceHash (CombinedState eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just hm -> pure $! Just $! mi_iface_hash (hm_iface hm)
    Nothing -> pure $! mi_simple_info_iface_hash <$> lookupEPSimpleInfo eps mod

lookupSafeHaskell :: CombinedState -> Module -> IO (Maybe (IfaceTrustInfo, Bool))
lookupSafeHaskell (CombinedState eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just hm -> pure $! Just $! (mi_trust (hm_iface hm), mi_trust_pkg (hm_iface hm))
    Nothing -> pure $! ((mi_simple_trust_info . mi_simple_info_public) &&& (mi_simple_trust_pkg . mi_simple_info_public)) <$> lookupEPSimpleInfo eps mod



lookupHashFunction :: CombinedState -> Module -> IO (Maybe ((OccName -> Maybe (OccName, Fingerprint))))
lookupHashFunction (CombinedState _eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just hm -> pure $! Just $! mi_hash_fn (hm_iface hm)
    Nothing -> pure $! mi_cache_hash_fn . mi_simple_caches . mi_simple_info_public <$> lookupEPSimpleInfo _eps mod

lookupFreeHoles :: CombinedState -> Module -> IO (Maybe (UniqDSet ModuleName))
lookupFreeHoles (CombinedState eps hug) mod = 
  HUG.lookupHugByModule mod hug >>= \case
    Just hm -> pure $! Just $! mi_free_holes (hm_iface hm)
    Nothing -> pure $! (lookupInstalledModuleEnv (eps_free_holes eps) (fst $ getModuleInstantiation mod))