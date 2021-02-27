{-# LANGUAGE ExistentialQuantification #-}
-- | Info about modules in the "home" unit
module GHC.Unit.Home.ModInfo
   ( HomeModInfo (..)
   , HomePackageTable(..)
   , emptyHomePackageTable
   , lookupHpt
   , eltsHpt
   , filterHpt
   , allHpt
   , mapHpt
   , delFromHpt
   , addToHpt
   , addListToHpt
   , lookupHptDirectly
   , lookupHptByModule
   , listToHpt
   , pprHPT
   )
where

import GHC.Prelude

import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module

import GHC.Linker.Types ( Linkable(..) )

import GHC.Types.Unique
import GHC.Types.Unique.DFM

import GHC.Utils.Outputable
import GHC.Compact
import Control.Monad

-- | Information about modules in the package being compiled
data HomeModInfo = HomeModInfo
   { hm_iface    :: !ModIface
        -- ^ The basic loaded interface file: every loaded module has one of
        -- these, even if it is imported from another package

   , hm_details  :: !ModDetails
        -- ^ Extra information that has been created from the 'ModIface' for
        -- the module, typically during typechecking

   , hm_linkable :: !(Maybe Linkable)
        -- ^ The actual artifact we would like to link to access things in
        -- this module.
        --
        -- 'hm_linkable' might be Nothing:
        --
        --   1. If this is an .hs-boot module
        --
        --   2. Temporarily during compilation if we pruned away
        --      the old linkable because it was out of date.
        --
        -- After a complete compilation ('GHC.load'), all 'hm_linkable' fields
        -- in the 'HomePackageTable' will be @Just@.
        --
        -- When re-linking a module ('GHC.Driver.Main.HscNoRecomp'), we construct the
        -- 'HomeModInfo' by building a new 'ModDetails' from the old
        -- 'ModIface' (only).
   }

data CompactRegion = forall a . CompactRegion (Compact a) | EmptyRegion
-- | Helps us find information about modules in the home package
data HomePackageTable = HomePackageTable {
                            hptCompactRegion :: (Maybe CompactRegion)
                            , hptModules :: (DModuleNameEnv HomeModInfo)
                            }
   -- Domain = modules in the home unit that have been fully compiled
   -- "home" unit id cached (implicit) here for convenience

-- | Constructs an empty HomePackageTable
emptyHomePackageTable :: HomePackageTable
emptyHomePackageTable  = HomePackageTable (Just EmptyRegion) emptyUDFM

lookupHpt :: HomePackageTable -> ModuleName -> Maybe HomeModInfo
lookupHpt (HomePackageTable _ dm) mn = lookupUDFM dm mn

lookupHptDirectly :: HomePackageTable -> Unique -> Maybe HomeModInfo
lookupHptDirectly (HomePackageTable _ dm) u = lookupUDFM_Directly dm u

eltsHpt :: HomePackageTable -> [HomeModInfo]
eltsHpt (HomePackageTable _ e) = eltsUDFM e

filterHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> HomePackageTable
filterHpt f (HomePackageTable c e) = HomePackageTable c (filterUDFM f e)

allHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
allHpt f hpt = allUDFM f (hptModules hpt)

mapHpt :: (HomeModInfo -> HomeModInfo) -> HomePackageTable -> HomePackageTable
mapHpt f hpt = hpt { hptModules = mapUDFM f (hptModules hpt) }

delFromHpt :: HomePackageTable -> ModuleName -> HomePackageTable
delFromHpt hpt mn = hpt { hptModules = delFromUDFM (hptModules hpt) mn }

addToHpt :: HomePackageTable -> ModuleName -> HomeModInfo -> IO HomePackageTable
addToHpt hpt mn hmi = do
  (hmi', new_c) <- case hptCompactRegion hpt of
    Nothing -> return (hmi, Nothing)
    Just r  -> do
      let raw_iface = forgetModIfaceCaches (hm_iface hmi)
      cr <- case r of
        CompactRegion c -> do
          compactAddWithSharing c raw_iface
        EmptyRegion -> do
          compactWithSharing raw_iface
      let compacted_iface = initModIfaceCaches $ getCompact cr
      return $ (hmi { hm_iface = compacted_iface }, Just $ CompactRegion cr)
  return $ hpt { hptModules = addToUDFM (hptModules hpt) mn hmi'
               , hptCompactRegion = new_c }

addListToHpt
  :: HomePackageTable -> [(ModuleName, HomeModInfo)] -> IO HomePackageTable
addListToHpt hpt mods = foldM (\hpt (mn, hmi) -> addToHpt hpt mn hmi) hpt mods

listToHpt :: [(ModuleName, HomeModInfo)] -> IO HomePackageTable
listToHpt = addListToHpt emptyHomePackageTable

lookupHptByModule :: HomePackageTable -> Module -> Maybe HomeModInfo
-- The HPT is indexed by ModuleName, not Module,
-- we must check for a hit on the right Module
lookupHptByModule hpt mod
  = case lookupHpt hpt (moduleName mod) of
      Just hm | mi_module (hm_iface hm) == mod -> Just hm
      _otherwise                               -> Nothing

pprHPT :: HomePackageTable -> SDoc
-- A bit arbitrary for now
pprHPT (HomePackageTable _ hpt) = pprUDFM hpt $ \hms ->
    vcat [ hang (ppr (mi_module (hm_iface hm)))
              2 (ppr (md_types (hm_details hm)))
         | hm <- hms ]

