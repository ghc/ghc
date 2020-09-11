-- | Model fuel consumption to detect recursive use of a 'Uniqable' thing.
module GHC.Types.Unique.FuelTank
  ( FuelTank, initFuelTank, setFuel, burnFuel, FuelBurntResult(..)
  ) where

import GHC.Prelude

import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Utils.Outputable

data FuelTank uniq
  = FT
  { init_fuel  :: !Int               -- ^ The upper bound of encounters
  , encounters :: !(UniqFM uniq Int) -- ^ Number of times we have seen a 'u'
  }

-- | Initialise a 'FuelTank' with the given amount of /fuel/, an upper bound
-- for how often a given uniquable thing may be encountered.
initFuelTank :: Int -> FuelTank uniq
initFuelTank fuel = FT { init_fuel = fuel, encounters = emptyUFM }

-- | Change the upper bound for the number of times a 'FuelTank' is allowed
-- to encounter each 'TyCon'.
setFuel :: Int -> FuelTank uniq -> FuelTank uniq
setFuel new_fuel tank = tank { init_fuel = new_fuel }

data FuelBurntResult uniq
  = OutOfFuel
  | FuelLeft !(FuelTank uniq)

-- | Burns one fuel in the 'FuelTank' for the given uniq thing. Returns
-- 'OutOfFuel' when all fuel was burned and @'FuelLeft' tank@ when there's
-- still fuel left in the new @tank@.
burnFuel :: Uniquable uniq => FuelTank uniq -> uniq -> FuelBurntResult uniq
burnFuel (FT init_fuel encounters) u = case lookupUFM encounters u of
  Just fuel_used | fuel_used >= init_fuel -> OutOfFuel
  _ -> FuelLeft (FT init_fuel (addToUFM_C (+) encounters u 1))

instance Outputable (FuelTank u) where
  ppr (FT init_fuel encounters) = ppr (init_fuel, encounters)
