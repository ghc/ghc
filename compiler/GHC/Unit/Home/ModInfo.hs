-- | Info about modules in the "home" unit
module GHC.Unit.Home.ModInfo
   ( HomeModInfo (..)
   , HomePackageTable
   , emptyHomePackageTable
   , lookupHpt
   , eltsHpt
   , filterHpt
   , allHpt
   , anyHpt
   , mapHpt
   , delFromHpt
   , addToHpt
   , addHomeModInfoToHpt
   , addListToHpt
   , lookupHptDirectly
   , lookupHptByModule
   , listToHpt
   , listHMIToHpt
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
import Data.List (sortOn)
import Data.Ord

-- | Information about modules in the package being compiled
data HomeModInfo = HomeModInfo
   { hm_iface    :: !ModIface
        -- ^ The basic loaded interface file: every loaded module has one of
        -- these, even if it is imported from another package

   , hm_details  :: ModDetails
        -- ^ Extra information that has been created from the 'ModIface' for
        -- the module, typically during typechecking

        -- This field is LAZY because a ModDetails is constructed by knot tying.

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

-- | Helps us find information about modules in the home package
type HomePackageTable = DModuleNameEnv HomeModInfo
   -- Domain = modules in the home unit that have been fully compiled
   -- "home" unit id cached (implicit) here for convenience

-- | Constructs an empty HomePackageTable
emptyHomePackageTable :: HomePackageTable
emptyHomePackageTable  = emptyUDFM

lookupHpt :: HomePackageTable -> ModuleName -> Maybe HomeModInfo
lookupHpt = lookupUDFM

lookupHptDirectly :: HomePackageTable -> Unique -> Maybe HomeModInfo
lookupHptDirectly = lookupUDFM_Directly

eltsHpt :: HomePackageTable -> [HomeModInfo]
eltsHpt = eltsUDFM

filterHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> HomePackageTable
filterHpt = filterUDFM

allHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
allHpt = allUDFM

anyHpt :: (HomeModInfo -> Bool) -> HomePackageTable -> Bool
anyHpt = anyUDFM

mapHpt :: (HomeModInfo -> HomeModInfo) -> HomePackageTable -> HomePackageTable
mapHpt = mapUDFM

delFromHpt :: HomePackageTable -> ModuleName -> HomePackageTable
delFromHpt = delFromUDFM

addToHpt :: HomePackageTable -> ModuleName -> HomeModInfo -> HomePackageTable
addToHpt = addToUDFM

addHomeModInfoToHpt :: HomeModInfo -> HomePackageTable -> HomePackageTable
addHomeModInfoToHpt hmi hpt = addToHpt hpt (moduleName (mi_module (hm_iface hmi))) hmi

addListToHpt
  :: HomePackageTable -> [(ModuleName, HomeModInfo)] -> HomePackageTable
addListToHpt = addListToUDFM

listToHpt :: [(ModuleName, HomeModInfo)] -> HomePackageTable
listToHpt = listToUDFM

listHMIToHpt :: [HomeModInfo] -> HomePackageTable
listHMIToHpt hmis =
  listToHpt [(moduleName (mi_module (hm_iface hmi)), hmi) | hmi <- sorted_hmis]
  where
    -- Sort to put Non-boot things last, so they overwrite the boot interfaces
    -- in the HPT, other than that, the order doesn't matter
    sorted_hmis = sortOn (Down . mi_boot . hm_iface) hmis

lookupHptByModule :: HomePackageTable -> Module -> Maybe HomeModInfo
-- The HPT is indexed by ModuleName, not Module,
-- we must check for a hit on the right Module
lookupHptByModule hpt mod
  = case lookupHpt hpt (moduleName mod) of
      Just hm | mi_module (hm_iface hm) == mod -> Just hm
      _otherwise                               -> Nothing

pprHPT :: HomePackageTable -> SDoc
-- A bit arbitrary for now
pprHPT hpt = pprUDFM hpt $ \hms ->
    vcat [ ppr (mi_module (hm_iface hm))
         | hm <- hms ]

