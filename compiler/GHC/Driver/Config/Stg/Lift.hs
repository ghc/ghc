module GHC.Driver.Config.Stg.Lift
  ( initStgLiftConfig
  ) where

import GHC.Stg.Lift.Config

import GHC.Driver.Session

initStgLiftConfig :: DynFlags -> StgLiftConfig
initStgLiftConfig dflags = StgLiftConfig
    { c_targetProfile      = targetProfile dflags
    , c_liftLamsRecArgs    = liftLamsRecArgs dflags
    , c_liftLamsNonRecArgs = liftLamsNonRecArgs dflags
    , c_liftLamsKnown      = liftLamsKnown dflags
    }
