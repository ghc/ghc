module GHC.Driver.Config.Stg.Ppr
  ( initStgPprOpts
  ) where

import GHC.Stg.Syntax

import GHC.Driver.Session

-- | Initialize STG pretty-printing options from DynFlags
initStgPprOpts :: DynFlags -> StgPprOpts
initStgPprOpts dflags = StgPprOpts
   { stgSccEnabled = sccProfilingEnabled dflags
   }
