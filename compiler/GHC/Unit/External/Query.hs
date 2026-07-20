module GHC.Unit.External.Query (
  -- * Query the 'UnitInfoMap'
  lookupUnit',
  lookupUnitId',
) where

import GHC.Prelude

import GHC.Types.Unique.Map
import GHC.Unit.External.Substitution
import GHC.Unit.Info
import GHC.Unit.Module

-- | A more specialized interface, which doesn't require a 'UnitState' (so it
-- can be used while we're initializing 'DynFlags')
--
-- Parameters:
--    * a boolean specifying whether or not to look for on-the-fly renamed interfaces
--    * a 'UnitInfoMap'
lookupUnit' :: Bool -> UnitInfoMap -> Unit -> Maybe UnitInfo
lookupUnit' allowOnTheFlyInst pkg_map u = case u of
   HoleUnit   -> error "Hole unit"
   RealUnit i -> lookupUniqMap pkg_map (unDefinite i)
   VirtUnit i
      | allowOnTheFlyInst
      -> -- lookup UnitInfo of the indefinite unit to be instantiated and
         -- instantiate it on-the-fly
         fmap (renameUnitInfo pkg_map (instUnitInsts i))
           (lookupUniqMap pkg_map (instUnitInstanceOf i))

      | otherwise
      -> -- lookup UnitInfo by virtual UnitId. This is used to find indefinite
         -- units. Even if they are real, installed units, they can't use the
         -- `RealUnit` constructor (it is reserved for definite units) so we use
         -- the `VirtUnit` constructor.
         lookupUniqMap pkg_map (virtualUnitId i)


-- | Find the unit we know about with the given unit id, if any
lookupUnitId' :: UnitInfoMap -> UnitId -> Maybe UnitInfo
lookupUnitId' db uid = lookupUniqMap db uid
