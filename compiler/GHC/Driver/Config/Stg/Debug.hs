module GHC.Driver.Config.Stg.Debug
  ( initStgDebugOpts
  ) where

import GHC.Stg.Debug

import GHC.Driver.Session

-- | Initialize STG pretty-printing options from DynFlags
initStgDebugOpts :: DynFlags -> StgDebugOpts
initStgDebugOpts dflags = StgDebugOpts
  { stgDebug_infoTableMap = gopt Opt_InfoTableMap dflags
  , stgDebug_distinctConstructorTables = gopt Opt_DistinctConstructorTables dflags
  }
