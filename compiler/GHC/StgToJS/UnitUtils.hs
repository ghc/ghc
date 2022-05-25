{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.UnitUtils
  ( unitModuleString
  , moduleGlobalSymbol
  , moduleExportsSymbol
  , getPackageName
  , encodeModule
  ) where


import GHC.Data.ShortText as ST
import GHC.Unit.Module
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Unit.Env
import GHC.Unit.Home
import GHC.Utils.Encoding
import GHC.Driver.Session

import GHC.Prelude

unitModuleString :: Module -> String
unitModuleString mod = mconcat
  [ unitIdString (moduleUnitId mod)
  , ":"
  , moduleNameString (moduleName mod)
  ]

-- | the global linkable unit of a module exports this symbol, depend on it to
--   include that unit (used for cost centres)
moduleGlobalSymbol :: Module -> ShortText
moduleGlobalSymbol m = mconcat
  [ "h$"
  , ST.pack (zEncodeString $ unitModuleString m)
  , "_<global>"
  ]

moduleExportsSymbol :: Module -> ShortText
moduleExportsSymbol m = mconcat
  [ "h$"
  , ST.pack (zEncodeString $ unitModuleString m)
  , "_<exports>"
  ]

-- FIXME: Use FastString
encodeModule :: UnitEnv -> DynFlags -> Module -> String
encodeModule u_env dflags k
  | isGhcjsPrimUnit u_env dflags (moduleUnitId k) = "ghcjs-prim"
  | isGhcjsThUnit   u_env dflags (moduleUnitId k) = "ghcjs-th"
  | otherwise                            = unitModuleString k

{-
   some packages are wired into GHCJS, but not GHC
   make sure we don't version them in the output
   since the RTS uses thins from them
-}

-- FIXME: Jeff (2022,03): I've swapped DynFlags for HscEnv to gain access to the
-- UnitState for these checks. Unsure if this is a great idea or even workable.
-- In either case it will proliferate DynFlags throughout the Linker. So the fix
-- should be to add flags to the Linker config so we do not need to carry HscEnv
-- or DynFlags around.
isGhcjsPrimUnit :: UnitEnv -> DynFlags -> UnitId -> Bool
isGhcjsPrimUnit u_env dflags pkgKey
  =  pn == "ghcjs-prim" || -- FIXME: Jeff (2022,03): use UnitID only instead of
                           -- a hacky String comparison, same for
                           -- @isGhcjsThUnit@
     (GHC.Prelude.null pn && pkgKey == home_uid &&
      elem "-DBOOTING_PACKAGE=ghcjs-prim" (opt_P dflags))
  where
    pn = unitIdString . ue_current_unit $ u_env
    -- FIXME: Jeff (2022,03): remove call to unsafe. Only using this because I
    -- am unsure when exactly the home unit for the GhcJS prims gets
    -- instantiated
    home_uid = homeUnitId . ue_unsafeHomeUnit $ u_env

isGhcjsThUnit :: UnitEnv -> DynFlags -> UnitId -> Bool
isGhcjsThUnit u_env dflags pkgKey
  =  pn == "ghcjs-th" ||
     (GHC.Prelude.null pn && pkgKey == home_uid &&
      elem "-DBOOTING_PACKAGE=ghcjs-th" (opt_P dflags))
  where
    home_uid = homeUnitId   . ue_unsafeHomeUnit $ u_env
    pn       = unitIdString . ue_current_unit   $ u_env

getPackageName :: UnitState -> UnitId -> String
getPackageName u_st = maybe "" unitPackageNameString . lookupUnitId u_st
