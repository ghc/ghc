module GHC.Unit.External.Wired (
  -- * 'WireMap'
  WireMap,
  emptyWireMap,
  isWireMapEmpty,
  lookupWireMap,
  listWireMap,
  -- * 'UnwireMap'
  UnwireMap,
  emptyUnwireMap,
  lookupUnwireMap,
  unwiringMapFromWireMap,
  -- * Creating 'WireMap'
  findWiredInUnits,
) where

import GHC.Prelude

import GHC.Unit.External.Database
import GHC.Unit.External.Visibility

import GHC.Data.Maybe
import GHC.Types.Unique.Map
import GHC.Unit.Database
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable as Outputable

-- | The 'WireMap' records the mapping from the 'UnitId' of on-disk 'UnitInfo'
-- to the 'UnitId' of the 'wiredInMap'.
--
-- See 'wiredInUnitIds' for the set of wired-in units.
--
newtype WireMap =
  WireMap (UniqMap UnitId UnitId)

emptyWireMap :: WireMap
emptyWireMap = WireMap emptyUniqMap

isWireMapEmpty :: WireMap -> Bool
isWireMapEmpty (WireMap wmap) = isNullUniqMap wmap

lookupWireMap :: UnitId -> WireMap -> Maybe UnitId
lookupWireMap uid (WireMap wmap) = lookupUniqMap wmap uid

listWireMap :: WireMap -> [(UnitId, UnitId)]
listWireMap (WireMap wmap) = nonDetUniqMapToList wmap

-- | The reverse of 'WireMap'.
-- Records the mapping from the wired-in 'UnitId' to the on-disk 'UnitId'.
newtype UnwireMap =
  UnwireMap (UniqMap UnitId UnitId)

emptyUnwireMap :: UnwireMap
emptyUnwireMap = UnwireMap emptyUniqMap

unwiringMapFromWireMap :: WireMap -> UnwireMap
unwiringMapFromWireMap (WireMap wired_map) =
  UnwireMap $ listToUniqMap [ (v,k) | (k,v) <- nonDetUniqMapToList wired_map ]

lookupUnwireMap :: UnitId -> UnwireMap -> Maybe UnitId
lookupUnwireMap uid (UnwireMap wmap) = lookupUniqMap wmap uid

-- -----------------------------------------------------------------------------
-- Wired-in units
--
-- See Note [Wired-in units] in GHC.Unit.Types

findWiredInUnits
   :: Logger
   -> UnitPrecedenceMap
   -> [UnitInfo]           -- database
   -> VisibilityMap             -- info on what units are visible
                                -- for wired in selection
   -> IO WireMap   -- map from unit id to wired identity
findWiredInUnits logger prec_map pkgs vis_map = do
  -- Now we must find our wired-in units, and rename them to
  -- their canonical names (eg. base-1.0 ==> base), as described
  -- in Note [Wired-in units] in GHC.Unit.Types
  let
        matches :: UnitInfo -> UnitId -> Bool
        pc `matches` pid = unitPackageName pc == PackageName (unitIdFS pid)

        -- find which package corresponds to each wired-in package
        -- delete any other packages with the same name
        -- update the package and any dependencies to point to the new
        -- one.
        --
        -- When choosing which package to map to a wired-in package
        -- name, we try to pick the latest version of exposed packages.
        -- However, if there are no exposed wired in packages available
        -- (e.g. -hide-all-packages was used), we can't bail: we *have*
        -- to assign a package for the wired-in package: so we try again
        -- with hidden packages included to (and pick the latest
        -- version).
        --
        -- You can also override the default choice by using -ignore-package:
        -- this works even when there is no exposed wired in package
        -- available.
        --
        findWiredInUnit :: [UnitInfo] -> UnitId -> IO (Maybe (UnitId, UnitInfo))
        findWiredInUnit pkgs wired_pkg = firstJustsM [try all_exposed_ps, try all_ps, notfound]
          where
                all_ps = [ p | p <- pkgs, p `matches` wired_pkg ]
                all_exposed_ps = [ p | p <- all_ps, (mkUnit p) `elemUniqMap` vis_map ]

                try ps = case sortByPreference prec_map ps of
                    p:_ -> Just <$> pick p
                    _ -> pure Nothing

                notfound = do
                          debugTraceMsg logger 2 $
                            text "wired-in package "
                                 <> ftext (unitIdFS wired_pkg)
                                 <> text " not found."
                          return Nothing
                pick :: UnitInfo -> IO (UnitId, UnitInfo)
                pick pkg = do
                        debugTraceMsg logger 2 $
                            text "wired-in package "
                                 <> ftext (unitIdFS wired_pkg)
                                 <> text " mapped to "
                                 <> ppr (unitId pkg)
                        return (wired_pkg, pkg)


  mb_wired_in_pkgs <- mapM (findWiredInUnit pkgs) wiredInUnitIds
  let
        wired_in_pkgs = catMaybes mb_wired_in_pkgs

        wiredInMap :: UniqMap UnitId UnitId
        wiredInMap = listToUniqMap
          [ (unitId realUnitInfo, wiredInUnitId)
          | (wiredInUnitId, realUnitInfo) <- wired_in_pkgs
          , not (unitIsIndefinite realUnitInfo)
          ]

  return $ WireMap wiredInMap
