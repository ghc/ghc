-- | UnitId type
module GHC.Unit.Id
   ( UnitId(..)
   , unitIdString
   , stringToUnitId
   )
where

import GhcPrelude

import GHC.Unit.Types
import FastString

unitIdString :: UnitId -> String
unitIdString = unpackFS . unitIdFS

stringToUnitId :: String -> UnitId
stringToUnitId = UnitId . mkFastString
