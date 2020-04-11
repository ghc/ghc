-- | UnitId type
module GHC.Types.Unit.Id
   ( UnitId(..)
   , unitIdString
   )
where

import GhcPrelude

import Outputable
import GHC.Types.Unique
import FastString
import Binary

import {-# SOURCE #-} GHC.Types.Unit.State (displayUnitId, getPackageState)

-- | A UnitId identifies a built library in a database and is used to generate
-- unique symbols, etc. It's usually of the form:
--
--    pkgname-1.2:libname+hash
--
-- These UnitId are provided to us via the @-this-unit-id@ flag.
--
-- The library in question may be definite or indefinite; if it is indefinite,
-- none of the holes have been filled (we never install partially instantiated
-- libraries as we can cheaply instantiate them on-the-fly, cf VirtUnit).  Put
-- another way, an installed unit id is either fully instantiated, or not
-- instantiated at all.
newtype UnitId =
    UnitId {
      -- | The full hashed unit identifier, including the component id
      -- and the hash.
      unitIdFS :: FastString
    }

instance Binary UnitId where
  put_ bh (UnitId fs) = put_ bh fs
  get bh = do fs <- get bh; return (UnitId fs)

instance Eq UnitId where
    uid1 == uid2 = getUnique uid1 == getUnique uid2

instance Ord UnitId where
    u1 `compare` u2 = unitIdFS u1 `compare` unitIdFS u2

instance Uniquable UnitId where
    getUnique = getUnique . unitIdFS

instance Outputable UnitId where
    ppr uid@(UnitId fs) =
        getPprStyle $ \sty ->
        sdocWithDynFlags $ \dflags ->
          case displayUnitId (getPackageState dflags) uid of
            Just str | not (debugStyle sty) -> text str
            _ -> ftext fs

unitIdString :: UnitId -> String
unitIdString = unpackFS . unitIdFS

