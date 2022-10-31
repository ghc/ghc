module GHC.Driver.Config.StgToCmm.Ticky
  ( initCmmTickyConfig
  ) where

import GHC.StgToCmm.Ticky.Config

import GHC.Driver.Session

initCmmTickyConfig :: DynFlags -> CmmTickyConfig
initCmmTickyConfig dflags = CmmTickyConfig
  { cmmTickyEnable   = gopt Opt_Ticky                 dflags
  , cmmTickyAllocd   = gopt Opt_Ticky_Allocd          dflags
  , cmmTickyLNE      = gopt Opt_Ticky_LNE             dflags
  , cmmTickyDynThunk = gopt Opt_Ticky_Dyn_Thunk       dflags
  , cmmTickyTag      = gopt Opt_Ticky_Tag             dflags
  }
